# 3 - Designing & Implementing A Logging Capability

Let's put this design pattern into practice with a tiny example. In the next step, we'll actually design and implement a real capability from Conduit.

We'd like to represent a new capability, and then provide both a production and a test implementation. The capability should be a type class with pure functions and can contain whatever business logic we'd like. The underlying implementation can perform effects but should be as thin as possible — only performing the bare minimum work necessary before delegating back to the capability.

*Note: I haven't explained how to set up your application to run in a custom monad with capabilities instead of in `Aff`. Vlad Ciobanu has created [a small example Halogen app](https://github.com/vladciobanu/purescript-halogen-example) that walks through how to wire everything together and I highly recommend it. You can also review the source code for this RealWorld app — specifically, `Main.purs`.*

## A small logger capability

We'd like to represent the ability to log output to the console or an external service like Rollbar (dependent on some global environment). For example, we might not want to send any information to external services unless the app is running in production. Or we might want to log a lot of information in debug mode, but suppress that output in other environments. We'd like to keep this capability pure so that we can easily test it.

Let's dive right in! Let's start by defining the modes that logging will depend on. We'll support three modes. The first two modes will simply take the message to log and defer to the implementation for how to do so; the error mode should also be able to specify 

    data LogType
      = Debug
      | Info
      | Error
    derive instance eqLogType :: Eq LogType
    derive instance ordLogType :: Ord LogType

This is the only data we'll require of users. Now, we can define a new type class that represents the ability to log messages: 

    -- We will ultimately want these functions to run in a monad which
    -- can support effects.
    class Monad m <= LogMessages m where
      log :: LogType -> String -> m Unit

I've opted to support one core function, `log`. When a user uses `log`, they should expect that their message will be written to some output, whether an external service, the console, or our tests. We'll write each of those implementations later.

There's one more thing we need to do for convenience. We'd like to be able to use these functions directly in any component that has the `LogMessages` constraint. While we choose what monad our components should use for evaluation, all components ultimately run in the Halogen monad, `HalogenM`. If we give `HalogenM` an instance of our `LogMessages` class, then we can use these functions without having to lift them. It'll be just like using `get`, `modify`, and `put`. To write our instance, we'll require that the monad we chose for our component also has an instance of `LogMessages`, and we'll lift all the functions into `HalogenM`. 

The short version of all this is that a little boilerplate here will allow us to write `_ ← logInfo "..."` instead of `_ ← H.lift $ logInfo "..."`.

    instance LogMessagesHalogenM 
      :: LogMessages m 
      => LogMessages (HalogenM s f g p o m)
      where
        log l s = lift (log l s)

And that's it! We can now represent the ability of a monad to log messages. We could even give instances for monads like `Effect` and `Aff` here if we wanted to. Instead, in our next step, we'll implement the core logic for logging messages as pure functions, and then we'll implement our own monad that represents our application as the domain.

## Implementing our capability with pure functions

When we log a message we'd like to perform a few actions:

1. We should check to see whether the message should be output at all. For example, we might want to write all messages to the console when we're in testing mode, but write errors only to an external logging service when we're in production.
2. We should format the log's message with some metadata we'd like to use to search our logs later on. For example, we might want to provide standard formatting for the different levels (debug, info, and error) and include a timestamp.
3. Finally, we should write the output.

This is going to be the core implementation for our logger. We'll keep this function abstract so that we can easily test it with monads other than our application monad and pure to follow functional programming best practices.

    -- We aren't logging an arbitrary string anymore -- we've now
    -- formatted it according to particular rules.
    newtype LogMessage = LogMessage String
    
    logMessage 
      :: forall m
       . Monad m
      => m DateTime             -- | How should we fetch the current time?
      -> (LogMessage -> m Unit) -- | How should we write this message?
      -> LogType                -- | What kind of log is this?
      -> String                 -- | What is the input string?
      -> m Unit
    logMessage getTime writeLog logType msg = do
      t <- fmtDateTime <$> getTime
      let msg' = LogMessage $ case logType of
            Debug -> "[DEBUG: " <> t <> "]\n" <> msg
            Info -> "[INFO: " <> t <> "]\n" <> msg
            Error -> "[ERROR: " <> t <> "]\n" <> msg
      writeLog msg'

With our core logic in hand, we can move on to implement our app monad. Once that's set up, we'll use this logic to write an instance of `LogMessages` for our monad.

## Implementing the ReaderT pattern with a custom monad

We've written an interface for the ability to log messages, and we've written the core logic, but right now there is no monad in the world that actually has a complete instance. This is where we move from our functional core and actually implement some effects. We're going to design a monad, `AppM`, which will contain implementations for all our capabilities.

This is the place where we'll finally start writing effectful code. This is our thin imperative shell. We'll finally reach for `Aff` and `Effect` and get this code running in the real world!

We're going to follow the ReaderT pattern and implement our own custom monad. Our monad will be part of a transformer using `ReaderT` and a small read-only global environment. Our environment will contain a flag noting what the logging level is. We'll use this flag when we implement our logging functions for our monad. When we're finished we'll have a complete application monad following the ReaderT pattern!

Let's start by defining our read-only environment with a log level:

    data LogLevel = Testing | Production
    type Env = { logLevel :: LogLevel }

This will be our global environment that any function running inside our app monad will have access to — once we write a `MonadAsk` instance for it. Let's move on to actually define our monad, now:

    newtype AppM a = AppM (ReaderT Env Aff a)

We need to use `Aff` as the base of our transformer because ultimately we'll require a component running in `Aff` to run our Halogen application. In fact, let's go ahead and write the transformation from `AppM` to `Aff` now:

    runAppM :: Env -> AppM ~> Aff
    runAppM e (AppM m) = runReaderT m e

Next, we need to define the instances that will make our new `AppM` type an actual monad. Fortunately we can derive all of them for free:

    derive instance newtypeAppM :: Newtype (AppM a) _
    derive newtype instance functorAppM :: Functor AppM
    derive newtype instance applyAppM :: Apply AppM
    derive newtype instance applicativeAppM :: Applicative AppM
    derive newtype instance bindAppM :: Bind AppM
    derive newtype instance monadAppM :: Monad AppM

While we're at it, we might as well derive instances for `Aff` and `Effect`, too:

    derive newtype instance monadEffectAppM :: MonadEffect AppM
    derive newtype instance monadAffAppM :: MonadAff AppM

There's one last instance we need to write before we can return to our logger: `MonadAsk`, so we can retrieve our global environment. Since we're already working within our `ReaderT` transformer, the instance is trivial. 

The only tricky part is that we've implemented our environment as a type synonym, rather than a newtype, and you can't have instances for type synonyms. We can get around the issue using `TypeEquals`, and if you're curious to learn more about why, [check out the purescript-halogen-example repo](https://github.com/vladciobanu/purescript-halogen-example).

    import Type.Equality as TE
    
    instance monadAskAppM :: TE.TypeEquals e Env => MonadAsk e AppM where
      ask = AppM $ asks TE.from

This has been a whirlwind tour of implementing a custom monad following the `ReaderT` pattern. It is meant as a brief introduction to the pattern, and I'd recommend reviewing the source code for more detailed comments about the implementation.

With this out of the way, though, we can finally move on to implement our logging capability with all its effects.

## Implementing the logger capability

To complete our capability we'll need to write an instance of `LogMessages` for our monad, `AppM`. We can freely use any of our other instances, like `Effect`, `Aff`, and `MonadAsk`. 

We'd like our monad to support two logging modes:

- When we're in `Testing` mode, we'll log messages to the console so developers can see them and we won't send them to our logging service.
- When we're in `Production` mode, we won't log any messages marked `Debug` or `Warn`, and we'll send `Error` messages to our external service.

    -- Our instance
    instance logMessagesAppM :: LogMessages AppM where
      log logType str = do
        env <- ask
    
        let shouldOutput
              | env.logLevel == Production && logType == Error = true
              | env.logLevel == Testing = true
              | otherwise = false
    
            writeLog :: Message -> AppM Unit
            writeLog msg = do case env.logLevel of
              Testing -> liftEffect $ Console.log $ unwrap msg
              Production -> liftAff $ sendToExternalService msg
            
        when shouldOutput do
          logMessage (liftEffect nowDateTime) writeLog logType str

The beauty of these instances is that our logging capability is decoupled from its implementation in `AppM`. If we decided to start writing logs to local storage instead of an external service, or we started sending them to a new service with new credentials, or we need to disable them for a while, we can do any of that by tweaking our instance without ever touching the rest of our code.

We can also write instances for `MonadAff` and `MonadEffect` so that our capability is available even to functions that don't use our underlying monad.

We can even write a separate monad that exists for integration testing or mocks called `TestM`, `IntegrationM`, or `MockM`.

## Testing our new logger capability

We'd like to test our logger without actually writing to our external service or the console. Instead, we want to verify that our function formats inputs correctly, calls the writing function, and the order and contents of each message are correct. We can record having written a series of messages using `Writer` at a particular timestamp and then examine the result:

    logMessageWriter :: DateTime -> LogType -> String -> Writer (Array String) Unit
    logMessageWriter dt logType str  = 
      logMessage getTime writeMsg logType str
      where
        getTime = const dt
        writeMsg = tell <<< pure

We can use this to test writing several logs of different levels and then test that the result is what we expected:

    logMessages :: Writer (Array String) Unit
    logMessages =
      logMessage' Debug "Preparing to start application"
       >>= logMessage' Debug "Setting environment log level to debug"
       >>= logMessage' Info "Starting up application"
       >>= logMessage' Error "Failed to start up application"
      where
       -- Log messages on January 1, 2018 at midnight
        logMessage' = logMessageWriter (unsafeMkDateTime 2018 1 1 0 0 0 0)

We can't prove that our real-world app monad is actually writing to the console or our external service with this alone, but we can at least test the rest of the logic. We could also have used Node to write to a file, and used our test monad as part of a [suite of golden tests](https://ro-che.info/articles/2017-12-04-golden-tests). Implementing our test monad would follow the exact same process as implementing our application monad.