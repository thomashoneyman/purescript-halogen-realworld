# 2 - Entities & Values

Let's put these principles into practice! We'll start by figuring out what entities exist in our system; these are typically the most important. We'll design one entity along with any required supporting data types. To see how I've approached the rest of the data, see the `Data` directory in the source code.

# Entities in Conduit

Entities are data that has a persistent identity. I reviewed the [backend and API specs](https://github.com/gothinkster/realworld/tree/master/api) and discovered a few entities and how they are identified. Conduit is a social blogging platform, so these entities won't be surprising:

- **User**, an entity representing the currently-authenticated user, identified by a *username*. This is used specifically for actions that need authentication, like logging in.
- **Profile**, an entity representing public information about a user in the system, identified with a *username*. This is used all over the place to represent who wrote a particular article or comment and to keep track of whether the currently logged-in user follows them.
- **Article**, an entity representing a piece of content published by a user, identified by a *slug.* It refers to an **author**, which is the same as the **profile** we already discussed.
- **Comment**, identified by an *id* combined with the *slug* of a particular article. It also refers to an **author**.

The **profile** entity is the most interesting of all because it's used pervasively and is necessary to implement comments and articles as data types. We'll start there with our data design, designing other data types as necessary. Here's the data returned in JSON via our API:

    {
      "profile": {
        "username": "jake",
        "bio": "I work at statefarm",
        "image": "https://static.productionready.io/images/smiley-cyrus.jpg",
        "following": false
      }
    }

Let's walk through this structure and decide how to represent the values contained within.

**Username**

Users are identified uniquely by a username, which comes back from the server as a string. Right away, I know that `String` is not the right type to represent a username. We want this value to represent a `Username` as a unique identifier for a user.

We don't have any other information about this username beyond the fact it cannot be empty. We can capture this in one of two ways: 

- We can create a `Username` newtype over a `String`, and then use the smart constructor pattern to ensure only non-empty strings can become usernames. If the server sent us bad data (an empty username), then we'll treat that as an error.
- We can create a `Username` newtype over a `NonEmptyString`, which will guarantee all usernames are non-empty. We'd again treat bad data from the server as an error.

    -- option A
    newtype Username = Username String
    
    mkUsername :: String -> Maybe Username
    mkUsername "" = Nothing
    mkUsername s = Just (Username s)
    
    -- option B
    newtype Username = Username NonEmptyString

**Bio**

User profiles contain a biography, which comes back from the server as a `String` or `null`. Our spec gives almost no information about this data. Besides being aware this is an optional field we've got basically no information about it. For that reason, I'll  just represent this field as a `Maybe String`.

    { ...
    , bio :: Maybe String 
    }

**Describing profile images**

We know that a user's profile image, stored as a string, will always be either a URL representing the location of some image on the server or `null`. Sounds like we'd better use a type to clarify what this value is used for! 

SlamData maintains the excellent [purescript-uri](https://pursuit.purescript.org/packages/purescript-uri/6.0.0) library, which uses types to guarantee that any URI or URL constructed or manipulated with library functions will be valid. This *could* be a good choice to represent these images — after all, they are urls.

On the other hand, we'll never create or manipulate this information. We won't perform any operations on it as a URI. We will simply treat the string we receive from the server as the location of the user's profile photo and leave it at that. With this in mind, the `URI` type is closer than `String`, but it's still not quite right — it doesn't quite capture how we intend to use these values.

In the end, we probably just need to reach for another newtype the way we did for `Username`. But this time there may not be a profile photo at all — it might come back from the server as `null`. PureScript gives us an elegant way to represent a possibly empty value with the `Maybe` type. So we'll say that a user *might* have a profile photo, and we'll ensure that profile photos are never empty. That way we can either render a default photo (`Nothing`) or whatever image is specified (`NonEmptyString`).

    newtype ProfilePhoto = ProfilePhoto NonEmptyString
    
    { ...
    , image :: Maybe ProfilePhoto
    }

**Representing follow status**

Finally, we'll look at the last value associated with a user profile: whether you, the authenticated user, follow them. We're going to use this value to control what you see when you view a user profile and whether you can attempt to follow or unfollow them.

Here's how it should work: 

- if you follow a user, then you should see an "unfollow" button on their profile, and you should be able to unfollow them.
- if you don't follow a user, then you should see a "follow" button on their profile, and you should be able to follow them.
- if you are viewing your own profile, then you shouldn't see a button at all, and you shouldn't be able to follow or unfollow yourself.

The first two cases are pretty straightforward and they indicate a boolean would work just fine. But the third case is a problem: when you view your own profile, there's no sense of whether you follow yourself. That means that putting a boolean flag inside our user profile type will be an illegal state when you look at your own profile — you ought not even be able to consider the case in which you follow or don't follow yourself!

We need to move the concept of "following" outside the user profile type we've been building so far. Every user has a profile, so our underlying type will remain, but we need to 
augment it with a new type to represent following. Since the `following` flag is usually used in a field called `author` in the server JSON, let's re-use that as our type name:

    data Author
      = Following UserProfile
      | NotFollowing UserProfile
      | You UserProfile

This nicely captures the three contexts in which you can view a profile. However, we haven't finished building out our user data type, so we can't quite put this structure together, and it's not clear how to represent you, the currently-authenticated user. Let's do that now.

## Putting the user type together: a first cut

At this point we know enough to represent the **user profile** entity. Let's put the type together:

    type UserProfile =
      { username :: Username
      , bio :: Maybe String
      , image :: Maybe ProfilePhoto
      }

This nicely covers the concept of a user profile using the types we designed earlier. When used in conjunction with the `Author` type, this lets us model the relationship between the current user and this profile, too.

# Designing The Authenticated User Type

Our profile entity is a great starting point, but things get hazier as soon as we move on to our authenticated user entity. This entity represents the current user sending requests, viewing profiles, and so on, and it contains information necessary to perform those actions in addition to the same data involved in the user profile.

Consider the JSON returned by the server for a profile:

    {
      "profile": {
        "username": "jake",
        "bio": "I work at statefarm",
        "image": "https://static.productionready.io/images/smiley-cyrus.jpg",
        "following": false
      }
    }

and the JSON returned by the server for the authenticated user:

    {
      "user": {
        "email": "jake@jake.jake",
        "token": "jwt.token.here",
        "username": "jake",
        "bio": "I work at statefarm",
        "image": null
      }
    }

The authenticated user shares the `username` identifier field, as well as the `bio` and `image` fields from a user profile. It includes two additional fields — an email address and a JWT token — and lacks the `following` field. Let's start piecing together a type that could represent an authenticated user.

**Handling the new email field**

We once again see string data that ought to be represented by a more accurate type. We'll follow our same smart constructor pattern to newtype `Email` and ensure that incorrect values can't be constructed.

    newtype Email = Email String
    mkEmail :: String -> Maybe Email

**Handling the new token field**

At first glance, the token field seems like yet another prime candidate to be newtyped and stuck as a field in a record. But this token will be used for authentication only — we don't want the value to ever be accessed outside the module we define it in. It shouldn't be possible to retrieve this value from our type, though we'd still like to define functions to save it to local storage or construct API headers. This is a hint that our `AuthUser` type won't be able to be a simple record! We want to have more control than that.

Instead of reaching for a newtype, which can only have one data constructor, we need to create our own custom product type and hide its constructors:

    module AuthUser (AuthUser, toLocalStorage, ...) where
    
    -- Usually I wouldn't reach for a type alias, but the token is not
    -- being exported from this module.
    type Token = String
    
    -- We aren't sure what additional data to store beyond the token just
    -- yet, but we'll fix this later.
    data AuthUser = AuthUser Token ???
    
    -- We can still freely use the token within this module, though it
    -- will be inaccessible outside the module.
    toLocalStorage :: AuthUser -> Effect Unit
    toLocalStorage (AuthUser tok _) = do ...

What should go in the second part of the data constructor? Well, an `AuthUser` appears to be a regular user, just with an email address and a token added on top and no concept of following. We've already pulled the token and concept of following out of the profile, so at this point all we need to do is add an email address to the fields in a profile when the user is an `AuthUser`.

**Re-using the underlying profile fields**

Fortunately, PureScript's row types let us do this quite easily. We can go back and revise our earlier profile type so that its fields can be re-used for both an authenticated user and a regular user.

    -- This type is now an "open record", which means it can be extended
    -- with more fields.
    type UserProfileFields r =
      { username :: Username
      , bio :: Maybe String
      , image :: Maybe ProfilePhoto
      | r
      }
    
    -- We can recover our original record by simply not providing any
    -- additional fields.
    type UserProfile = UserProfileFields ()

Now, with this extensible record, we can simply re-use the same underlying type to create our `AuthUser` as well:

    data AuthUser 
      = AuthUser Token (UserProfileFields (email :: Email))

## Revising our types for simplicity

This design works quite well, and given what we know at this point, it's entirely adequate. That said, the design process involves jumping back and forth between implementing data and functions over that data. Let's walk through how that might work.

**Revising the authenticated user type**

This type actually contains too much information. After reviewing the spec and how this type will be used in more detail, a few factors jump out:

- An `AuthUser` already has a profile; it's not necessary to copy all the same fields into this additional type and therefore cause them to live in two places.
- Outside of when they are being treated as any other user, an `AuthUser`'s profile details are *never used.* In other words, an `AuthUser` is necessary for requests and so on, but those actions don't use the profile information. When the profile information associated with an `AuthUser` is being used (rendering), then the token and email are not.
- We definitely need to maintain two entities — profile and authenticated user — but it's only necessary that the authenticated user maintain information for requests and local storage and other auth-related purposes. That's all it's ever used for.

For these reasons, we can actually simplify our `AuthUser` type to just two fields: the `Username` (used for identity) and the token (used for authentication).

    data AuthUser = AuthUser Username Token
    
    -- AuthUser follows the smart constructor pattern, but we'll want to
    -- be able to retrieve the username (not the token!)
    username :: AuthUser -> Username
    username (AuthUser u _) = u

**Revising the author and user profile types**

In contrast, our user profile types aren't informative enough on their own. We intentionally moved the idea of a profile and its relationship with the current user into an `Author` type. Next, lets turn to how these profiles will be used in our system. Besides viewing, we also need to support following and unfollowing.

To follow a user, we'll need both the user to follow and the token of the currently-authenticated user. We'd like to write a function like this:

    -- We can't just use a `Token` directly because it is only accessible
    -- via the AuthUser type. Later, we'll examine ways to make even this
    -- a pure function!
    follow :: UserProfile -> AuthUser -> Effect Unit

This isn't quite right. You shouldn't be able to follow *any* username. You should only be able to follow a user you don't follow already, and certainly not yourself. What we really want is a function like this:

    newtype UnfollowedAuthor = UnfollowedAuthor UserProfile
    follow :: UnfollowedAuthor -> AuthUser -> Effect Unit

We want to restrict the inputs to our function so that it only operates on users we don't already follow. With this in mind, we should go back to our `Author` type and revise it to use these new wrappers, too.

    data Author
      = Following FollowedAuthor
      | NotFollowing UnfollowedAuthor
      | You UserProfile

# Wrapping Up

We've now designed two important entities in our system: user profiles and authenticated users. As we went through this process, we used our design principles to ensure our types accurately reflected what our data means and how it ought to be used. To see how the rest of the data in the system has been implemented, check out the source code!

## An Exercise

Try implementing the `Article` and `Comment` entities and their associated values, reviewing the spec as you go. Then, compare your implementation with mine in the source code. Was yours better or more accurate? Let me know by filing an issue and suggesting an improvement to the code!