# 3 - Design Data & Pure Functions

PureScript has a best-in-class type system with powerful features like algebraic data types, generics, row types, type classes. Let's put this type system to work to help us represent the the data our application will use to support our use cases.

We'll sometimes use primitive data like strings, booleans, and numbers to represent values in our application. But most data we'll work with have meaning in our application beyond being a boolean or a string. Types allow us to imbue values with compiler-enforced meaning and perform double-duty as a form of documentation.

I like to break data down into three distinct kinds:

- **Entities** are data with a persistent identity, like a _User_ or an _Article_. In other words, two users are the same user only if they have the same identity, and a user can change value over time and be considered the same user so long as it has preserved its identity. They'll often be aggregates of other values.
- **Values** are data without an identity, like an _Email_, _Username_, or _List_. Two emails are the same if their value is the same. If an email is changed, it should be considered a new value.
- **Lifecycles** are data representing the various states that a value or entity can be in at different points in time. Lifecycles will usually be data types that contain values or entities and might represent transitions among several types.

If you're familiar with type-driven design and best practices for ensuring types capture valid states in your domain while disallowing invalid states, then feel free to skip ahead to see how I've designed data specific to the Conduit application. If you're new to this process, however, start with _Principles For Designing Data_.

## Table of Contents

1. [Principles For Designing Data](#1-principles-for-designing-data)
2. [Entities & Values](#2-entities--values)
3. [Processes & Lifecycles](#3-processes--lifecycles)

# 1. Principles For Designing Data

I like to follow a number of best practices when designing data in a strongly-typed functional language like PureScript. As we create data types for Conduit I'll adhere to these principles.

## Write Data To Support Business Processes

We define data in our applications so that we can process and transform it. The business processes we need to support dictate what data types we build, and the data types inform how we implement those processes as functions. Without understanding the sorts of use cases our application is meant to support it's impossible to create adequate data types. Without the proper data types our functions will be a mess.

Throughout the rest of this guide we'll see plenty of back and forth between defining types and defining functions that operate on those types.

## Use Types To Give Values Meaning

A short rule of thumb for creating custom types: **create a new type when you need to represent some data, but no existing type properly captures its semantics.**

A type can be thought of as a set of values. A `Boolean` is a set of two values: true or false. When we create a custom type, we get to choose what values are a part of that type's set. Unlike primitive types like `Boolean`, our custom types usually map to some concept in our domain that we want to precisely represent. Our rules for what set of values belongs to our type are what give that type meaning.

**Give Data An Informative Type**

As much as possible we'd like to work with types that have meaning in our domain instead of primitive types. For example, we _could_ represent routes in our application as `String`s:

```purescript
home :: String
home = "home"

-- A function which will set the hash, triggering a route change
navigate :: String -> Effect Unit
navigate r = setHashTo r
```

But `String` can represent a lot more than the routes in our application (of all possible strings, an absurdly tiny number will be valid routes) and is not informative in a type signature. Reading `String → Effect Unit` could represent innumerable functions, only one of which is routing.

Let's fix this by creating a type with meaning:

```purescript
data Route
  = Home
  | Settings

navigate :: Route -> Effect Unit
navigate Home = setHashTo "home"
navigate Settings = setHashTo "settings"
```

Much better! We can now never accidentally provide a route that doesn't exist to our `navigate` function, and while the function could still perform all kinds of side effects, the fact that it takes a `Route` as an argument is a good hint as to its purpose. We'll take this approach to write our Conduit routes.

**Use Custom Types For Identification, Even If You Still Use Primitive Types Underneath**

We can also use types to give extra meaning to an existing primitive type.

For example, we might need to represent a customer ID, which will be a positive number of arbitrary size. Our routes were finite and could be represented by a small number of data constructors, but our customer IDs make sense to represent with a `Natural` number.

Unfortunately natural numbers don't have the right meaning for this value. We won't perform arithmetic on these numbers — they're purely being used for identification! One way to prevent our new type from being treated as a true natural number instead of as an identifier is to use a lightweight type with no runtime overhead called a `newtype`:

```purescript
newtype CustomerId = CustomerId Natural
newtype OrderId = OrderId Natural
```

Now, even though we're still using the `Natural` type under the hood, we have descriptive types and can no longer:

- accidentally perform arithmetic on a customer ID
- mix up the order of arguments to a function that takes multiple `Natural` numbers, like accidentally try to check `CustomerId` and `OrderId` for equality
- use the wrong primitive value accidentally, like calling a function meant for a customer ID with an order ID because they're both `Natural` values

It isn't always easy to decide whether data deserves a new, custom type, or whether it should re-use an existing type in the PureScript ecosystem. When in doubt, consider what a type representing your data should **mean** and how the data will be **used**.

If an existing type carries a similar meaning and values of that type are used the way your values will be used, then re-use that type. If not, create your own.

## Make Illegal States Unrepresentable

One of the delightful properties of a strongly-typed language is that it lets us make guarantees about our code. We'd like to enforce at compile-time that any values in our business logic are valid and consistent. We'll design types that make it impossible to even construct or use a value that is not allowed by the business logic or specification.

In an article about [making illegal states unrepresentable](https://fsharpforfunandprofit.com/posts/designing-with-types-making-illegal-states-unrepresentable/), Scott Wlaschin describes modeling a user's contact information. The user can have an email address, postal address, or both on file — but they must have at least one. We can't model the type with a simple sum or product because neither adequately restricts us from making something incorrect:

```purescript
-- We can't construct a user with both an email and postal address,
-- which we ought to be able to. Not permissive enough!
type ContactInfo = Either Email Postal

-- We could construct a user that doesn't have an email OR a postal
-- address. Too permissive!
type ContactInfo = Tuple (Maybe Email) (Maybe Postal)
```

Instead, we need to create a custom sum type that explicitly only allows valid states:

```purescript
-- Now, if we have a ContactInfo, we know it *must* be valid
data ContactInfo
  = EmailOnly Email
  | PostalOnly Postal
  | Both Email Postal
```

When we can guarantee a value is valid with a type, then all of the functions that operate on that type don't have to worry about failure cases! Rather than thread potential errors and failures throughout the system, we sanitize values before they get very far.

In a lovely article, [Type Safety Back And Forth](http://www.parsonsmatt.org/2017/10/11/type_safety_back_and_forth.html), Matt Parsons describes this as _restricting the domain_. He gives the example of an `Order` type, which contains a list of items:

```purescript
type Order = { items :: Array Item }
```

However, in this particular domain, we know that an order _always_ contains some items. You should never have an order that doesn't have any items in it. One way to address this is to expand the range of every function that operates on this type so that they handle the failure case (when the array is unexpectedly empty):

```purescript
-- We can encode the possibility of failure with Maybe, but
-- every function operating on the type now has to result in
-- additional cases, which will then have to be handled by the
-- caller of the function
processOrder :: Order -> Maybe Order
```

This isn't so ideal — it propagates potential failure throughout the system and keeps pushing the responsibility of handling that failure forward. You're going to have to constantly handle these failure cases when you use any function that processes an order.

What if we prevented an invalid order from being constructed in the first place?

```purescript
type Order = { items :: NonEmptyArray Item }
```

It is impossible to construct an empty `NonEmptyArray`. Thus we can't even construct an invalid order and we don't have to handle the possibility of an invalid order in any of our processing functions:

```purescript
processOrder :: Order -> Order
splitOrder :: Order -> NonEmptyArray Order
```

**Restricting The Domain Using Smart Constructors**

Let's turn to Conduit. We need to keep track of usernames for our users, and to do that, we could use a simple record:

```purescript
type User = { username :: String }
```

But this doesn't properly represent what a username can be in our system. Can usernames really be an unbounded string of any sequence of characters and any length, including an empty string? We could set this username to `"!"` and it'd compile just fine!

This type is much too permissive. Functions that operate on usernames are going to have to account for all sorts of failure cases in case the input string isn't valid. So let's force usernames in our system to be correct.

If we have a `Username`, then we should be guaranteed that it's valid. We can't enforce this as simply as we could make our `Order` contain a non-empty array, but we can rely on a different approach: _smart constructors_.

The "smart constructor" pattern refers to creating types that have their data constructors hidden from users, so they can never be constructed directly. Instead, users have to use a specific function to construct the a value of the right type. That function usually performs some validation to ensure specific constraints are met. If you ever operate on a value of the validated type, then you can be guaranteed that it has already been vetted.

Let's try using this pattern to define a `Username` type that guarantees the string contained within is within 5 and 50 alphanumeric characters:

```purescript
-- We are only exporting the Username *type*, but we aren't exporting
-- its *constructor*. Other modules can import the type, but you can't
-- actually construct a value directly.
--
-- To export the type AND its constructors, use `Username(..)`
module MyModule (Username, mkUsername) where

-- We will restrict this type to represent strings between 5 and
-- 50 alphanumeric characters
newtype Username = Username String

-- This function will be the only way to construct a `Username`,
-- and it will ensure this guarantee is met.
mkUsername :: String -> Maybe Username
mkUsername = Username <=< inRange 5 50 <=< allAlphaNum
  where
  inRange :: Int -> Int -> String -> Maybe String
  allAlphaNum :: String -> Maybe String
```

We can now provide a guarantee to functions consuming a `Username` that the string contained within has met certain constraints, and that the function doesn't need to check or handle those potential sources of failure.

# 2. Entities & Values

Let's put these principles into practice! We'll start by figuring out what entities exist in our system; these are typically the most important. We'll design one entity along with any required supporting data types. To see how I've approached the rest of the data, see the `Data` directory in the source code.

## Entities in Conduit

Entities are data that has a persistent identity. I reviewed the [backend and API specs](https://github.com/gothinkster/realworld/tree/master/api) and discovered a few entities and how they are identified. Conduit is a social blogging platform, so these entities won't be surprising:

- **User**, an entity representing the currently-authenticated user, identified by a _username_. This is used specifically for actions that need authentication, like logging in.
- **Profile**, an entity representing public information about a user in the system, identified with a _username_. This is used all over the place to represent who wrote a particular article or comment and to keep track of whether the currently logged-in user follows them.
- **Article**, an entity representing a piece of content published by a user, identified by a _slug._ It refers to an **author**, which is the same as the **profile** we already discussed.
- **Comment**, identified by an _id_ combined with the _slug_ of a particular article. It also refers to an **author**.

The **profile** entity is the most interesting of all because it's used pervasively and is necessary to implement comments and articles as data types. We'll start there with our data design, designing other data types as necessary. Here's the data returned in JSON via our API:

```json
{
  "profile": {
    "username": "jake",
    "bio": "I work at statefarm",
    "image": "https://static.productionready.io/images/smiley-cyrus.jpg",
    "following": false
  }
}
```

Let's walk through this structure and decide how to represent the values contained within.

**Username**

Users are identified uniquely by a username, which comes back from the server as a string. Right away, I know that `String` is not the right type to represent a username. We want this value to represent a `Username` as a unique identifier for a user.

We don't have any other information about this username beyond the fact it cannot be empty. We can capture this in one of two ways:

- We can create a `Username` newtype over a `String`, and then use the smart constructor pattern to ensure only non-empty strings can become usernames. If the server sent us bad data (an empty username), then we'll treat that as an error.
- We can create a `Username` newtype over a `NonEmptyString`, which will guarantee all usernames are non-empty. We'd again treat bad data from the server as an error.

```purescript
-- option A
newtype Username = Username String

mkUsername :: String -> Maybe Username
mkUsername "" = Nothing
mkUsername s = Just (Username s)

-- option B
newtype Username = Username NonEmptyString
```

**Bio**

User profiles contain a biography, which comes back from the server as a `String` or `null`. Our spec gives almost no information about this data. Besides being aware this is an optional field we've got basically no information about it. For that reason, I'll just represent this field as a `Maybe String`.

```purescript
  { ...
  , bio :: Maybe String
  }
```

**Describing profile images**

We know that a user's profile image, stored as a string, will always be either a URL representing the location of some image on the server or `null`. Sounds like we'd better use a type to clarify what this value is used for!

SlamData maintains the excellent [purescript-uri](https://pursuit.purescript.org/packages/purescript-uri/6.0.0) library, which uses types to guarantee that any URI or URL constructed or manipulated with library functions will be valid. This _could_ be a good choice to represent these images — after all, they are urls.

On the other hand, we'll never create or manipulate this information. We won't perform any operations on it as a URI. We will simply treat the string we receive from the server as the location of the user's profile photo and leave it at that. With this in mind, the `URI` type is closer than `String`, but it's still not quite right — it doesn't quite capture how we intend to use these values.

In the end, we probably just need to reach for another newtype the way we did for `Username`. But this time there may not be a profile photo at all — it might come back from the server as `null`. PureScript gives us an elegant way to represent a possibly empty value with the `Maybe` type. So we'll say that a user _might_ have a profile photo, and we'll ensure that profile photos are never empty. That way we can either render a default photo (`Nothing`) or whatever image is specified (`NonEmptyString`).

```purescript
newtype ProfilePhoto = ProfilePhoto NonEmptyString

  { ...
  , image :: Maybe ProfilePhoto
  }
```

**Representing follow status**

Finally, we'll look at the last value associated with a user profile: whether you, the authenticated user, follow them. We're going to use this value to control what you see when you view a user profile and whether you can attempt to follow or unfollow them.

Here's how it should work:

- if you follow a user, then you should see an "unfollow" button on their profile, and you should be able to unfollow them.
- if you don't follow a user, then you should see a "follow" button on their profile, and you should be able to follow them.
- if you are viewing your own profile, then you shouldn't see a button at all, and you shouldn't be able to follow or unfollow yourself.

The first two cases are pretty straightforward and they indicate a boolean would work just fine. But the third case is a problem: when you view your own profile, there's no sense of whether you follow yourself. That means that putting a boolean flag inside our user profile type will be an illegal state when you look at your own profile — you ought not even be able to consider the case in which you follow or don't follow yourself!

We need to move the concept of "following" outside the user profile type we've been building so far. Every user has a profile, so our underlying type will remain, but we need to
augment it with a new type to represent following. Since the `following` flag is usually used in a field called `author` in the server JSON, let's re-use that as our type name:

```purescript
data Author
  = Following UserProfile
  | NotFollowing UserProfile
  | You UserProfile
```

This nicely captures the three contexts in which you can view a profile. However, we haven't finished building out our user data type, so we can't quite put this structure together, and it's not clear how to represent you, the currently-authenticated user. Let's do that now.

### Putting the user type together: a first cut

At this point we know enough to represent the **user profile** entity. Let's put the type together:

```purescript
type UserProfile =
  { username :: Username
  , bio :: Maybe String
  , image :: Maybe ProfilePhoto
  }
```

This nicely covers the concept of a user profile using the types we designed earlier. When used in conjunction with the `Author` type, this lets us model the relationship between the current user and this profile, too.

## Designing The Authenticated User Type

Our profile entity is a great starting point, but things get hazier as soon as we move on to our authenticated user entity. This entity represents the current user sending requests, viewing profiles, and so on, and it contains information necessary to perform those actions in addition to the same data involved in the user profile.

Consider the JSON returned by the server for a profile:

```json
{
  "profile": {
    "username": "jake",
    "bio": "I work at statefarm",
    "image": "https://static.productionready.io/images/smiley-cyrus.jpg",
    "following": false
  }
}
```

and the JSON returned by the server for the authenticated user:

```json
{
  "user": {
    "email": "jake@jake.jake",
    "token": "jwt.token.here",
    "username": "jake",
    "bio": "I work at statefarm",
    "image": null
  }
}
```

The authenticated user shares the `username` identifier field, as well as the `bio` and `image` fields from a user profile. It includes two additional fields — an email address and a JWT token — and lacks the `following` field. Let's start piecing together a type that could represent an authenticated user.

**Handling the new email field**

We once again see string data that ought to be represented by a more accurate type. We'll follow our same smart constructor pattern to newtype `Email` and ensure that incorrect values can't be constructed.

```purescript
newtype Email = Email String
mkEmail :: String -> Maybe Email
```

**Handling the new token field**

At first glance, the token field seems like yet another prime candidate to be newtyped and stuck as a field in a record. But this token will be used for authentication only — we don't want the value to ever be accessed outside the module we define it in. It shouldn't be possible to retrieve this value from our type, though we'd still like to define functions to save it to local storage or construct API headers. This is a hint that our `AuthUser` type won't be able to be a simple record! We want to have more control than that.

Instead of reaching for a newtype, which can only have one data constructor, we need to create our own custom product type and hide its constructors:

```purescript
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
```

What should go in the second part of the data constructor? Well, an `AuthUser` appears to be a regular user, just with an email address and a token added on top and no concept of following. We've already pulled the token and concept of following out of the profile, so at this point all we need to do is add an email address to the fields in a profile when the user is an `AuthUser`.

**Re-using the underlying profile fields**

Fortunately, PureScript's row types let us do this quite easily. We can go back and revise our earlier profile type so that its fields can be re-used for both an authenticated user and a regular user.

```purescript
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
```

Now, with this extensible record, we can simply re-use the same underlying type to create our `AuthUser` as well:

```purescript
data AuthUser
  = AuthUser Token (UserProfileFields (email :: Email))
```

### Revising our types for simplicity

This design works quite well, and given what we know at this point, it's entirely adequate. That said, the design process involves jumping back and forth between implementing data and functions over that data. Let's walk through how that might work.

**Revising the authenticated user type**

This type actually contains too much information. After reviewing the spec and how this type will be used in more detail, a few factors jump out:

- An `AuthUser` already has a profile; it's not necessary to copy all the same fields into this additional type and therefore cause them to live in two places.
- Outside of when they are being treated as any other user, an `AuthUser`'s profile details are _never used._ In other words, an `AuthUser` is necessary for requests and so on, but those actions don't use the profile information. When the profile information associated with an `AuthUser` is being used (rendering), then the token and email are not.
- We definitely need to maintain two entities — profile and authenticated user — but it's only necessary that the authenticated user maintain information for requests and local storage and other auth-related purposes. That's all it's ever used for.

For these reasons, we can actually simplify our `AuthUser` type to just two fields: the `Username` (used for identity) and the token (used for authentication).

```purescript
data AuthUser = AuthUser Username Token

-- AuthUser follows the smart constructor pattern, but we'll want to
-- be able to retrieve the username (not the token!)
username :: AuthUser -> Username
username (AuthUser u _) = u
```

**Revising the author and user profile types**

In contrast, our user profile types aren't informative enough on their own. We intentionally moved the idea of a profile and its relationship with the current user into an `Author` type. Next, lets turn to how these profiles will be used in our system. Besides viewing, we also need to support following and unfollowing.

To follow a user, we'll need both the user to follow and the token of the currently-authenticated user. We'd like to write a function like this:

```purescript
-- We can't just use a `Token` directly because it is only accessible
-- via the AuthUser type. Later, we'll examine ways to make even this
-- a pure function!
follow :: UserProfile -> AuthUser -> Effect Unit
```

This isn't quite right. You shouldn't be able to follow _any_ username. You should only be able to follow a user you don't follow already, and certainly not yourself. What we really want is a function like this:

```purescript
newtype UnfollowedAuthor = UnfollowedAuthor UserProfile
follow :: UnfollowedAuthor -> AuthUser -> Effect Unit
```

We want to restrict the inputs to our function so that it only operates on users we don't already follow. With this in mind, we should go back to our `Author` type and revise it to use these new wrappers, too.

```purescript
data Author
  = Following FollowedAuthor
  | NotFollowing UnfollowedAuthor
  | You UserProfile
```

# Wrapping Up

We've now designed two important entities in our system: user profiles and authenticated users. As we went through this process, we used our design principles to ensure our types accurately reflected what our data means and how it ought to be used. To see how the rest of the data in the system has been implemented, check out the source code!

### An Exercise

Try implementing the `Article` and `Comment` entities and their associated values, reviewing the spec as you go. Then, compare your implementation with mine in the source code. Was yours better or more accurate? Let me know by filing an issue and suggesting an improvement to the code!

# 3. - Processes & Lifecycles

We'd like to keep our data pure and stateless as much as possible. But sometimes we need to represent data in one of many states, and we might need to write functions to transition from one state to another. We can keep our code pure by writing types that capture the possible states of our data.

For example, most of the data in Conduit is loaded from the server. We're constantly going to have pages which may not have fetched some necessary data yet, or may have failed to retrieve it, or may have successfully received and parsed the data, and so on. We don't want to encode any of this state in the data itself, like an `AuthUser`; instead, we'll represent all these states in a separate type.

## A first cut at a lifecycle type: arrays

Let's consider the state of a component that needs to fetch some `Stuff` from the server. When the component is initialized, it won't have any data — it hasn't fetched it yet! We could represent this with an empty container when there is no data, like an empty array:

```purescript
type ComponentState = { stuff :: Array Stuff }

myInitialState :: ComponentState
myInitialState = { stuff: [] }
```

This model causes all kinds of problems. How do we know whether our array is empty because there was no data to load, or whether it's empty because we haven't attempted to fetch the data yet? What do we do if there was an error loading the data?

We can solve both issues with flags:

```purescript
type ComponentState =
  { stuff :: Array Stuff
  , errors :: Error
  , fetched :: Boolean
  }
```

Now we can tell whether we've fetched our data yet, and we can check if there were any errors loading the data. But this is now much too permissive! We can have errors _and_ an array of stuff, but we should really only have one or the other. We have to maintain this extra flag, `fetched`, which can fall out of sync if we aren't careful, so we could have an array of stuff and also have `fetched: false`, which shouldn't be possible. And what if we're in the middle of a request, so we don't have our data OR any errors yet?

It gets worse — now we have to create fields to maintain state on every data type that can be fetched remotely.

Let's solve this by creating a custom type. We want our type to ensure a few rules are followed:

- If we have completed a request, then we should have our data or some errors, but never both at the same time.
- If we haven't completed a request, then we shouldn't have any data or any errors.
- A request can be incomplete either because we haven't sent it yet or because it's still in progress, but never both at the same time.
- The type should be able to be used to add state to stateless data; it shouldn't require changing the underlying data at all.

## A second cut: a type for remote data

This type captures the various states our data can have within our component state:

```purescript
-- https://pursuit.purescript.org/packages/purescript-remotedata
data RemoteData err res
  = NotAsked
  | Loading
  | Failure err
  | Success res
```

If we now represent our data with this type, we can ensure that we cannot ever construct data in an inconsistent state. If we haven't performed a request, we cannot have any data or errors. Once we've completed a request, we can only have errors or our data, but not both. And we have a way to represent our initial state without resorting to an inaccurate value like an empty array! Let's update our initial state to use this type:

```purescript
type ComponentState = { stuff :: RemoteData Error (Array Stuff) }

myInitialState :: ComponentState
myInitialState = { stuff: NotAsked }
```

We'll continue to use this type to represent remote data throughout our application. It nicely captures the possible states of data requested from the server, and it allows all the rest of our data to be stateless: no other data has to worry about the concept of whether it's been loaded or not.

## Extra Benefits

This type lets us represent a running process with a type — `Loading` — which opens up further opportunities.

- What if we want to prevent the user from getting data into an inconsistent state by leaving a page that's still saving data, or writing to a text field that might have data loading into it? We could check if any data on the page is in a `Loading` state and prevent these actions.
- What if we want to only navigate to the next page once the data for _that_ page has been loaded? That way a user can continue interacting with the current page until the data for their next view is ready. We can use the same `Loading` state to ensure all data is ready before we move on and also render a loading bar across the top of the screen.
- We have kept our data types pure: there's no `Aff` or `Effect` being stored in the type, though it may be necessary to transition from state to state and there will be some effects performed while we are in our `Loading` state. We've successfully pushed effects to the edges of our data.

In the next section, we'll look at ways to model effectful processes while maintaining purity throughout the vast majority of our application.
