# 2 - Capabilities in Conduit

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

    -- This class represents the ability to acquire various resources
    -- without authentication
    class Monad m <= GetPublicResource m where
      getArticle :: Slug -> m (Either Error Article)
      getUserProfile :: Username -> m (Either Error UserProfile)
      getArticlesByTag :: Tag -> m (Either Error (Array Article))

We can run and test these few functions without needing access to any particular user. The rest of our data access will require a bit more, however: we'll have to load credentials for the currently-authenticated user, use that to construct our request, and then send it off. We'll represent the ability to manage authentication with a separate class:

    -- This class represents authentication and the ability to read / 
    -- write credentials
    class Monad m <= Authentication m where
      readCredentials :: m (Either Error AuthUser)
      writeCredentials :: AuthUser -> m (Either Error Unit)
      deleteCredentials :: m (Either Error Unit)

While we'll use local storage to read and write our credentials in Conduit, we don't have to, and we can easily switch to another method in the future.

Next, we'll represent the ability to access and modify resources that require credentials.

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

This set of classes lets us represent to creating, reading, updating, and deleting resources in Conduit without tying us to a particular implementation or requiring effects. These can be implemented and tested as pure functions!

## Next Steps

In the next section, we'll implement a logging capability from scratch, building a `ReaderT` implementation along the way.
