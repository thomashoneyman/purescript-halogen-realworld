# 1 - Principles for Designing Capabilities

It's lovely to write pure functions and data, but at some point you've got to actually make those requests, render components for users to interact with, write and read from local storage, and do other highly effectful actions. We've already designed some pure data and functions, but when we get into the bulk of the Conduit application we'll have to fight the temptation to reach for `Aff` and `Effect` pervasively. We have lots of imperative, mutative, effectful tasks to perform, but we can still write mostly-pure code by following a simple design pattern.

Michael Snoyman and Matt Parsons have done a wonderful job of explaining this design pattern already and I will gladly defer to their existing writing on the subject. I've provided summaries of their work below, but I highly recommend reading the originals (linked below) as well.

After you know a little more about the ReaderT and three-layer patterns for applications in Haskell and PureScript we'll walk through how to put them into practice in Conduit.

# The ReaderT Design Pattern

In mid-2017, Michael Snoyman wrote an excellent blog post about the [ReaderT design pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern). In a nutshell, the ReaderT pattern involves a few steps:

- Implement a custom application monad using the `ReaderT` monad transformer. This transformer allows you to have a global read-only environment with configuration values like logging levels, database connections, credentials, and so on. Don't use any other monad transformers except for small subsets of your application where they are particularly useful.
- If you need some global mutable state, use a mutable reference instead of the `StateT` monad transformer. It's critical you avoid `StateT` for Halogen applications because every component runs in its own thread; `StateT` won't provide the global state you want. [See this explanation in the Halogen repo for more details](https://github.com/slamdata/purescript-halogen/issues/386).
- Write functions that need perform effects (`Aff`, `Effect`) in terms of pure type classes that can be run to perform effects in your app monad later, but could also be run in other, pure contexts (like your test suite).

Don't worry if this is all brand-new to you — in the remainder of the guide we'll see plenty of examples demonstrating how to implement this pattern.

# The Three-Layer App

In early 2018 Matt Parsons wrote a followup article titled [The Three Layer Haskell Cake](http://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html). This article expands on the ReaderT pattern by dividing applications into three areas of responsibility:

- **Layer 1: The ReaderT Pattern**
You can define all the pure functions you'd like, but eventually your application must run and produce effects. This layer is where that happens: your monad is interpreted from `ReaderT` to `Aff` and executed. This layer is purely about operational concerns: maintaining configuration values, executing requests, implementing concurrency, and so on. You'll also write instances that implement the next layer up. This layer should be kept as minimal as possible.
- **Layer 2: External Services & Dependencies**
You'll need Layer 1 to actually manage a database connection, but you shouldn't implement functions to interface with external services and dependencies there. Instead, write pure functions that can be run by Layer 1 later to perform effects. We'll see plenty of examples of these functions in the next section of the guide. This layer should also be kept fairly small, deferring to functions from Layer 3 where possible.
- **Layer 3: Pure Business Logic**
The rest of your application should be practically effect-free. Use pure functions and simple data types to implement your business logic. Since we've kept effects out of this layer we'll get to use classic functional programming principles and techniques.

In the following sections we'll implement Conduit according to this architecture. It's not strictly necessary to follow this architecture for such a small application, but it provides the proper foundation for the app to scale easily over time.