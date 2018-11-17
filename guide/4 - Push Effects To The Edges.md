# 4 - Push Effects To The Edges

So far we've spent all our time writing core data types and the types of functions that will operate on them. We haven't implemented any of these functions yet. We could go ahead and implement any pure functions we'd like, but we should pause before implementing any non-pure functions (functions with `Aff` or `Effect` in their type). We need to be careful about how we design effects in our system.

A core principle in purely functional programming is to separate effects and data as much as possible. This naturally leads to applications with a [functional core](https://www.destroyallsoftware.com/talks/boundaries) and [imperative shell](https://www.destroyallsoftware.com/screencasts/catalog/functional-core-imperative-shell). The vast majority of code is written as side-effect-free functions and data and only at the boundaries of the application do the effects show up. The boundaries of the application are where our core logic meets the outside world, whether via API requests, outside input, components rendering to the page, and so on.

What about Conduit? At first glance, the application is almost entirely made up of boundaries! Most of our data loads from the server or local storage, and we barely decode and transform it before we're making another request or rendering. Even so, we'll find that we can still build a substantial functional core with only a thin shell of code with effects.

In this section we'll review a common architecture for Halogen applications and then put it into practice to implement our core capabilities.

## Table of Contents

1. [Principles for Designing Capabilities](#1-principles-for-designing-capabilities)
2. [Capabilities in Conduit](#2-capabilities-in-conduit)
3. [Designing & Implementing A Logging Capability](#3-designing--implementing-a-logging-capability)

# 1. Principles for Designing Capabilities

It's lovely to write pure functions and data, but at some point you've got to actually make those requests, render components for users to interact with, write and read from local storage, and do other highly effectful actions. We've already designed some pure data and functions, but when we get into the bulk of the Conduit application we'll have to fight the temptation to reach for `Aff` and `Effect` pervasively. We have lots of imperative, mutative, effectful tasks to perform, but we can still write mostly-pure code by following a simple design pattern.

Michael Snoyman and Matt Parsons have done a wonderful job of explaining this design pattern already and I will gladly defer to their existing writing on the subject. I've provided summaries of their work below, but I highly recommend reading the originals (linked below) as well.

After you know a little more about the ReaderT and three-layer patterns for applications in Haskell and PureScript we'll walk through how to put them into practice in Conduit.

## The ReaderT Design Pattern

In mid-2017, Michael Snoyman wrote an excellent blog post about the [ReaderT design pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern). In a nutshell, the ReaderT pattern involves a few steps:

- Implement a custom application monad using the `ReaderT` monad transformer. This transformer allows you to have a global read-only environment with configuration values like logging levels, database connections, credentials, and so on. Don't use any other monad transformers except for small subsets of your application where they are particularly useful.
- If you need some global mutable state, use a mutable reference instead of the `StateT` monad transformer. It's critical you avoid `StateT` for Halogen applications because every component runs in its own thread; `StateT` won't provide the global state you want. [See this explanation in the Halogen repo for more details](https://github.com/slamdata/purescript-halogen/issues/386).
- Write functions that need perform effects (`Aff`, `Effect`) in terms of pure type classes that can be run to perform effects in your app monad later, but could also be run in other, pure contexts (like your test suite).

Don't worry if this is all brand-new to you — in the remainder of the guide we'll see plenty of examples demonstrating how to implement this pattern.

## The Three-Layer App

In early 2018 Matt Parsons wrote a followup article titled [The Three Layer Haskell Cake](http://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html). This article expands on the ReaderT pattern by dividing applications into three areas of responsibility:

- **Layer 1: The ReaderT Pattern**
  You can define all the pure functions you'd like, but eventually your application must run and produce effects. This layer is where that happens: your monad is interpreted from `ReaderT` to `Aff` and executed. This layer is purely about operational concerns: maintaining configuration values, executing requests, implementing concurrency, and so on. You'll also write instances that implement the next layer up. This layer should be kept as minimal as possible.
- **Layer 2: External Services & Dependencies**
  You'll need Layer 1 to actually manage a database connection, but you shouldn't implement functions to interface with external services and dependencies there. Instead, write pure functions that can be run by Layer 1 later to perform effects. We'll see plenty of examples of these functions in the next section of the guide. This layer should also be kept fairly small, deferring to functions from Layer 3 where possible.
- **Layer 3: Pure Business Logic**
  The rest of your application should be practically effect-free. Use pure functions and simple data types to implement your business logic. Since we've kept effects out of this layer we'll get to use classic functional programming principles and techniques.

In the following sections we'll implement Conduit according to this architecture. It's not strictly necessary to follow this architecture for such a small application, but it provides the proper foundation for the app to scale easily over time.

# 2. Capabilities in Conduit

Capabilities let us write pure functions for actions in our system that will eventually require effects to run. We can use them to push effects all the way to the outer edge of our system. Let's consider some capabilities that we might want to support in Conduit!

## Choosing Capabilities

Conduit interacts with a few resources outside the system, including:

- A REST API over HTTP, which we use to fetch information about users, articles, and comments, and to write data to our database
- Local storage, which we use to store and load credentials for the currently-authenticated user
- The console, which we'll use for logging messages until our startup can afford a better logging service
- The browser's window object, which we'll use to navigate the user from place to place in our application

We'd like to represent each of these as capabilities. How should we approach this?

## Capabilities are about the interface, not the implementation

We should design our type classes to describe the information we want to work with, not the mechanism we use to retrieve or process it. For example, we don't want to represent the capability to hit an API — we want to represent being able to read and write information about users, articles, and comments.

Ideally we don't care whether it comes from a REST API, GraphQL, local storage, or even a test mock. That way when our backend team decides suddenly to move to GraphQL we don't have to change our pure code and can simply adjust the underlying implementation.

The same is true of our other capabilities. We want to represent being able to retrieve and store user credentials, but not necessarily that it will require local storage. We want to represent the ability to navigate and log messages, but not that we'll be using push state or hashes or the console or an external logging service.

Let's examine what a data access capability might look like. We'll start with the ability to access resources without needing any credentials:

```purescript
-- This class represents the ability to acquire various resources
-- without authentication
class Monad m <= GetPublicResource m where
  getArticle :: Slug -> m (Either Error Article)
  getUserProfile :: Username -> m (Either Error UserProfile)
  getArticlesByTag :: Tag -> m (Either Error (Array Article))
```

We can run and test these few functions without needing access to any particular user. The rest of our data access will require a bit more, however: we'll have to load credentials for the currently-authenticated user, use that to construct our request, and then send it off. We'll represent the ability to manage authentication with a separate class:

```purescript
-- This class represents authentication and the ability to read /
-- write credentials
class Monad m <= Authentication m where
  readCredentials :: m (Either Error AuthUser)
  writeCredentials :: AuthUser -> m (Either Error Unit)
  deleteCredentials :: m (Either Error Unit)
```

While we'll use local storage to read and write our credentials in Conduit, we don't have to, and we can easily switch to another method in the future.

Next, we'll represent the ability to access and modify resources that require credentials.

```purescript
-- This class represents the ability to acquire resources that require
-- authentication. Because it relies on our `Authentication` class, we
-- can use those functions in our implementation.
class Authentication m <= GetAuthResource m
  getAuthUserProfile :: m (Either Error UserProfile)
  getArticleFeed :: m (Either Error (Array Article))

-- This class represents the ability to update resources in the system,
-- all of which will require authentication. It's separated because we
-- can then make it clear which functions have read-only access and
-- which have read & write access.
class GetAuthResource m <= ManageAuthResource m where
  updateUser :: UserProfile -> m (Either Error UserProfile)
  followUser :: FollowedAuthor -> m (Either Error UserProfile)
  unfollowUser :: UnfollowedAuthor -> m (Either Error UserProfile)
  deleteArticle :: Slug -> m (Either Error Unit)
```

This set of classes lets us represent to creating, reading, updating, and deleting resources in Conduit without tying us to a particular implementation or requiring effects. These can be implemented and tested as pure functions!

## Next Steps

In the next section, we'll implement a logging capability from scratch, building a `ReaderT` implementation along the way.

# 3. Designing & Implementing A Logging Capability

Let's put this design pattern into practice with a tiny example. In the next step, we'll actually design and implement a real capability from Conduit.

We'd like to represent a new capability, and then provide both a production and a test implementation. The capability should be a type class with pure functions and can contain whatever business logic we'd like. The underlying implementation can perform effects but should be as thin as possible — only performing the bare minimum work necessary before delegating back to the capability.

_Note: See `Main.purs` for how this monad is translated into `Aff` for Halogen to run._

## A small logger capability

We'd like to represent the ability to log output to the console or an external service like Rollbar (dependent on some global environment). For example, we might not want to send any information to external services unless the app is running in production. Or we might want to log a lot of information in debug mode, but suppress that output in other environments. We'd like to keep this capability pure so that we can easily test it.

Let's dive right in! Let's start by defining the modes that logging will depend on. We'll support three modes. The first two modes will simply take the message to log and defer to the implementation for how to do so; the error mode should also be able to specify

```purescript
data LogType
  = Debug
  | Info
  | Error

derive instance eqLogType :: Eq LogType
derive instance ordLogType :: Ord LogType
```

This is the only data we'll require of users. Now, we can define a new type class that represents the ability to log messages:

```purescript
-- We will ultimately want these functions to run in a monad which
-- can support effects.
class Monad m <= LogMessages m where
  log :: LogType -> String -> m Unit
```

I've opted to support one core function, `log`. When a user uses `log`, they should expect that their message will be written to some output, whether an external service, the console, or our tests. We'll write each of those implementations later.

There's one more thing we need to do for convenience. We'd like to be able to use these functions directly in any component that has the `LogMessages` constraint. While we choose what monad our components should use for evaluation, all components ultimately run in the Halogen monad, `HalogenM`. If we give `HalogenM` an instance of our `LogMessages` class, then we can use these functions without having to lift them. It'll be just like using `get`, `modify`, and `put`. To write our instance, we'll require that the monad we chose for our component also has an instance of `LogMessages`, and we'll lift all the functions into `HalogenM`.

The short version of all this is that a little boilerplate here will allow us to write `_ ← logInfo "..."` instead of `_ ← H.lift $ logInfo "..."`.

```purescript
instance LogMessagesHalogenM
  :: LogMessages m
  => LogMessages (HalogenM s f g p o m)
  where
    log l s = lift (log l s)
```

And that's it! We can now represent the ability of a monad to log messages. We could even give instances for monads like `Effect` and `Aff` here if we wanted to. Instead, in our next step, we'll implement the core logic for logging messages as pure functions, and then we'll implement our own monad that represents our application as the domain.

## Implementing our capability with pure functions

When we log a message we'd like to perform a few actions:

1. We should check to see whether the message should be output at all. For example, we might want to write all messages to the console when we're in testing mode, but write errors only to an external logging service when we're in production.
2. We should format the log's message with some metadata we'd like to use to search our logs later on. For example, we might want to provide standard formatting for the different levels (debug, info, and error) and include a timestamp.
3. Finally, we should write the output.

This is going to be the core implementation for our logger. We'll keep this function abstract so that we can easily test it with monads other than our application monad and pure to follow functional programming best practices.

```purescript
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
```

With our core logic in hand, we can move on to implement our app monad. Once that's set up, we'll use this logic to write an instance of `LogMessages` for our monad.

## Implementing the ReaderT pattern with a custom monad

We've written an interface for the ability to log messages, and we've written the core logic, but right now there is no monad in the world that actually has a complete instance. This is where we move from our functional core and actually implement some effects. We're going to design a monad, `AppM`, which will contain implementations for all our capabilities.

This is the place where we'll finally start writing effectful code. This is our thin imperative shell. We'll finally reach for `Aff` and `Effect` and get this code running in the real world!

We're going to follow the ReaderT pattern and implement our own custom monad. Our monad will be part of a transformer using `ReaderT` and a small read-only global environment. Our environment will contain a flag noting what the logging level is. We'll use this flag when we implement our logging functions for our monad. When we're finished we'll have a complete application monad following the ReaderT pattern!

Let's start by defining our read-only environment with a log level:

```purescript
data LogLevel = Testing | Production
type Env = { logLevel :: LogLevel }
```

This will be our global environment that any function running inside our app monad will have access to — once we write a `MonadAsk` instance for it. Let's move on to actually define our monad, now:

```purescript
newtype AppM a = AppM (ReaderT Env Aff a)
```

We need to use `Aff` as the base of our transformer because ultimately we'll require a component running in `Aff` to run our Halogen application. In fact, let's go ahead and write the transformation from `AppM` to `Aff` now:

```purescript
runAppM :: Env -> AppM ~> Aff
runAppM e (AppM m) = runReaderT m e
```

Next, we need to define the instances that will make our new `AppM` type an actual monad. Fortunately we can derive all of them for free:

```purescript
derive instance newtypeAppM :: Newtype (AppM a) _
derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
```

While we're at it, we might as well derive instances for `Aff` and `Effect`, too:

```purescript
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
```

There's one last instance we need to write before we can return to our logger: `MonadAsk`, so we can retrieve our global environment. Since we're already working within our `ReaderT` transformer, the instance is trivial.

The only tricky part is that we've implemented our environment as a type synonym, rather than a newtype, and you can't have instances for type synonyms. We can get around the issue using `TypeEquals`, and if you're curious to learn more about why, [check out the purescript-halogen-example repo](https://github.com/vladciobanu/purescript-halogen-example).

```purescript
import Type.Equality as TE

instance monadAskAppM :: TE.TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks TE.from
```

This has been a whirlwind tour of implementing a custom monad following the `ReaderT` pattern. It is meant as a brief introduction to the pattern, and I'd recommend reviewing the source code for more detailed comments about the implementation.

With this out of the way, though, we can finally move on to implement our logging capability with all its effects.

## Implementing the logger capability

To complete our capability we'll need to write an instance of `LogMessages` for our monad, `AppM`. We can freely use any of our other instances, like `Effect`, `Aff`, and `MonadAsk`.

We'd like our monad to support two logging modes:

- When we're in `Testing` mode, we'll log messages to the console so developers can see them and we won't send them to our logging service.
- When we're in `Production` mode, we won't log any messages marked `Debug` or `Warn`, and we'll send `Error` messages to our external service.

```purescript
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
```

The beauty of these instances is that our logging capability is decoupled from its implementation in `AppM`. If we decided to start writing logs to local storage instead of an external service, or we started sending them to a new service with new credentials, or we need to disable them for a while, we can do any of that by tweaking our instance without ever touching the rest of our code.

We can also write instances for `MonadAff` and `MonadEffect` so that our capability is available even to functions that don't use our underlying monad.

We can even write a separate monad that exists for integration testing or mocks called `TestM`, `IntegrationM`, or `MockM`.

## Testing our new logger capability

We'd like to test our logger without actually writing to our external service or the console. Instead, we want to verify that our function formats inputs correctly, calls the writing function, and the order and contents of each message are correct. We can record having written a series of messages using `Writer` at a particular timestamp and then examine the result:

```purescript
logMessageWriter :: DateTime -> LogType -> String -> Writer (Array String) Unit
logMessageWriter dt logType str  =
  logMessage getTime writeMsg logType str
  where
  getTime = const dt
  writeMsg = tell <<< pure
```

We can use this to test writing several logs of different levels and then test that the result is what we expected:

```purescript
logMessages :: Writer (Array String) Unit
logMessages =
  logMessage' Debug "Preparing to start application"
    >>= logMessage' Debug "Setting environment log level to debug"
    >>= logMessage' Info "Starting up application"
    >>= logMessage' Error "Failed to start up application"
  where
  -- Log messages on January 1, 2018 at midnight
  logMessage' = logMessageWriter (unsafeMkDateTime 2018 1 1 0 0 0 0)
```

We can't prove that our real-world app monad is actually writing to the console or our external service with this alone, but we can at least test the rest of the logic. We could also have used Node to write to a file, and used our test monad as part of a [suite of golden tests](https://ro-che.info/articles/2017-12-04-golden-tests). Implementing our test monad would follow the exact same process as implementing our application monad.
