# 1 - Principles For Designing Data

I like to follow a number of best practices when designing data in a strongly-typed functional language like PureScript. As we create data types for Conduit I'll adhere to these principles.

# 1. Write Data To Support Business Processes

We define data in our applications so that we can process and transform it. The business processes we need to support dictate what data types we build, and the data types inform how we implement those processes as functions. Without understanding the sorts of use cases our application is meant to support it's impossible to create adequate data types. Without the proper data types our functions will be a mess. 

Throughout the rest of this guide we'll see plenty of back and forth between defining types and defining functions that operate on those types.

# 2. Use Types To Give Values Meaning

A short rule of thumb for creating custom types: **create a new type when you need to represent some data, but no existing type properly captures its semantics.**

A type can be thought of as a set of values. A `Boolean` is a set of two values: true or false. When we create a custom type, we get to choose what values are a part of that type's set. Unlike primitive types like `Boolean`, our custom types usually map to some concept in our domain that we want to precisely represent. Our rules for what set of values belongs to our type are what give that type meaning.

**Give Data An Informative Type**

As much as possible we'd like to work with types that have meaning in our domain instead of primitive types. For example, we *could* represent routes in our application as `String`s:

    home :: String
    home = "home"
    
    -- A function which will set the hash, triggering a route change
    navigate :: String -> Effect Unit
    navigate r = setHashTo r

But `String` can represent a lot more than the routes in our application (of all possible strings, an absurdly tiny number will be valid routes) and is not informative in a type signature. Reading `String → Effect Unit` could represent innumerable functions, only one of which is routing.

Let's fix this by creating a type with meaning:

    data Route 
      = Home
      | Settings
    
    navigate :: Route -> Effect Unit
    navigate Home = setHashTo "home"
    navigate Settings = setHashTo "settings"

Much better! We can now never accidentally provide a route that doesn't exist to our `navigate` function, and while the function could still perform all kinds of side effects, the fact that it takes a `Route` as an argument is a good hint as to its purpose. We'll take this approach to write our Conduit routes.

**Use Custom Types For Identification, Even If You Still Use Primitive Types Underneath**

We can also use types to give extra meaning to an existing primitive type. 

For example, we might need to represent a customer ID, which will be a positive number of arbitrary size. Our routes were finite and could be represented by a small number of data constructors, but our customer IDs make sense to represent with a `Natural` number.

Unfortunately natural numbers don't have the right meaning for this value. We won't perform arithmetic on these numbers — they're purely being used for identification! One way to prevent our new type from being treated as a true natural number instead of as an identifier is to use a lightweight type with no runtime overhead called a `newtype`: 

    newtype CustomerId = CustomerId Natural
    newtype OrderId = OrderId Natural

Now, even though we're still using the `Natural` type under the hood, we have descriptive types and can no longer:

- accidentally perform arithmetic on a customer ID
- mix up the order of arguments to a function that takes multiple `Natural` numbers, like accidentally try to check `CustomerId` and `OrderId` for equality
- use the wrong primitive value accidentally, like calling a function meant for a customer ID with an order ID because they're both `Natural` values

It isn't always easy to decide whether data deserves a new, custom type, or whether it should re-use an existing type in the PureScript ecosystem. When in doubt, consider what a type representing your data should **mean** and how the data will be **used**. 

If an existing type carries a similar meaning and values of that type are used the way your values will be used, then re-use that type. If not, create your own.

# 3. Make Illegal States Unrepresentable

One of the delightful properties of a strongly-typed language is that it lets us make guarantees about our code. We'd like to enforce at compile-time that any values in our business logic are valid and consistent. We'll  design types that make it impossible to even construct or use a value that is not allowed by the business logic or specification. 

In an article about [making illegal states unrepresentable](https://fsharpforfunandprofit.com/posts/designing-with-types-making-illegal-states-unrepresentable/), Scott Wlaschin describes modeling a user's contact information. The user can have an email address, postal address, or both on file — but they must have at least one. We can't model the type with a simple sum or product because neither adequately restricts us from making something incorrect:

    -- We can't construct a user with both an email and postal address,
    -- which we ought to be able to. Not permissive enough!
    type ContactInfo = Either Email Postal
    
    -- We could construct a user that doesn't have an email OR a postal
    -- address. Too permissive!
    type ContactInfo = Tuple (Maybe Email) (Maybe Postal)

Instead, we need to create a custom sum type that explicitly only allows valid states:

    -- Now, if we have a ContactInfo, we know it *must* be valid
    data ContactInfo
      = EmailOnly Email
      | PostalOnly Postal
      | Both Email Postal

When we can guarantee a value is valid with a type, then all of the functions that operate on that type don't have to worry about failure cases! Rather than thread potential errors and failures throughout the system, we sanitize values before they get very far.

In a lovely article, [Type Safety Back And Forth](http://www.parsonsmatt.org/2017/10/11/type_safety_back_and_forth.html), Matt Parsons describes this as *restricting the domain*. He gives the example of an `Order` type, which contains a list of items:

    type Order = { items :: Array Item }

However, in this particular domain, we know that an order *always* contains some items. You should never have an order that doesn't have any items in it. One way to address this is to expand the range of every function that operates on this type so that they handle the failure case (when the array is unexpectedly empty):

    -- We can encode the possibility of failure with Maybe, but
    -- every function operating on the type now has to result in
    -- additional cases, which will then have to be handled by the
    -- caller of the function
    processOrder :: Order -> Maybe Order

This isn't so ideal — it propagates potential failure throughout the system and keeps pushing the responsibility of handling that failure forward. You're going to have to constantly handle these failure cases when you use any function that processes an order. 

What if we prevented an invalid order from being constructed in the first place?

    type Order = { items :: NonEmptyArray Item }

It is impossible to construct an empty `NonEmptyArray`. Thus we can't even construct an invalid order and we don't have to handle the possibility of an invalid order in any of our processing functions:

    processOrder :: Order -> Order
    splitOrder :: Order -> NonEmptyArray Order

**Restricting The Domain Using Smart Constructors**

Let's turn to Conduit. We need to keep track of usernames for our users, and to do that, we could use a simple record:

    type User = { username :: String }

But this doesn't properly represent what a username can be in our system. Can usernames really be an unbounded string of any sequence of characters and any length, including an empty string? We could set this username to `"!"` and it'd compile just fine! 

This type is much too permissive. Functions that operate on usernames are going to have to account for all sorts of failure cases in case the input string isn't valid. So let's force usernames in our system to be correct. 

If we have a `Username`, then we should be guaranteed that it's valid. We can't enforce this as simply as we could make our `Order` contain a non-empty array, but we can rely on a different approach: *smart constructors*.

The "smart constructor" pattern refers to creating types that have their data constructors hidden from users, so they can never be constructed directly. Instead, users have to use a specific function to construct the a value of the right type. That function usually performs some validation to ensure specific constraints are met. If you ever operate on a value of the validated type, then you can be guaranteed that it has already been vetted.

Let's try using this pattern to define a `Username` type that guarantees the string contained within is within 5 and 50 alphanumeric characters:

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

We can now provide a guarantee to functions consuming a `Username` that the string contained within has met certain constraints, and that the function doesn't need to check or handle those potential sources of failure.