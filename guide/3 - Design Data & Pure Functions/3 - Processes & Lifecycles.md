# 3 - Processes & Lifecycles

We'd like to keep our data pure and stateless as much as possible. But sometimes we need to represent data in one of many states, and we might need to write functions to transition from one state to another. We can keep our code pure by writing types that capture the possible states of our data.

For example, most of the data in Conduit is loaded from the server. We're constantly going to have pages which may not have fetched some necessary data yet, or may have failed to retrieve it, or may have successfully received and parsed the data, and so on. We don't want to encode any of this state in the data itself, like an `AuthUser`; instead, we'll represent all these states in a separate type. 

## A first cut at a lifecycle type: arrays

Let's consider the state of a component that needs to fetch some `Stuff` from the server. When the component is initialized, it won't have any data — it hasn't fetched it yet! We could represent this with an empty container when there is no data, like an empty array:

    type ComponentState = { stuff :: Array Stuff }
    
    myInitialState :: ComponentState
    myInitialState = { stuff: [] }

This model causes all kinds of problems. How do we know whether our array is empty because there was no data to load, or whether it's empty because we haven't attempted to fetch the data yet? What do we do if there was an error loading the data?

We can solve both issues with flags:

    type ComponentState = 
      { stuff :: Array Stuff
      , errors :: Error
      , fetched :: Boolean
      }

Now we can tell whether we've fetched our data yet, and we can check if there were any errors loading the data. But this is now much too permissive! We can have errors *and* an array of stuff, but we should really only have one or the other. We have to maintain this extra flag, `fetched`, which can fall out of sync if we aren't careful, so we could have an array of stuff and also have `fetched: false`, which shouldn't be possible. And what if we're in the middle of a request, so we don't have our data OR any errors yet?

It gets worse — now we have to create fields to maintain state on every data type that can be fetched remotely.

Let's solve this by creating a custom type. We want our type to ensure a few rules are followed:

- If we have completed a request, then we should have our data or some errors, but never both at the same time.
- If we haven't completed a request, then we shouldn't have any data or any errors.
- A request can be incomplete either because we haven't sent it yet or because it's still in progress, but never both at the same time.
- The type should be able to be used to add state to stateless data; it shouldn't require changing the underlying data at all.

## A second cut: a type for remote data

This type captures the various states our data can have within our component state:

    -- https://pursuit.purescript.org/packages/purescript-remotedata
    data RemoteData err res
      = NotAsked
      | Loading
      | Failure err
      | Success res

If we now represent our data with this type, we can ensure that we cannot ever construct data in an inconsistent state. If we haven't performed a request, we cannot have any data or errors. Once we've completed a request, we can only have errors or our data, but not both. And we have a way to represent our initial state without resorting to an inaccurate value like an empty array! Let's update our initial state to use this type:

    type ComponentState = { stuff :: RemoteData Error (Array Stuff) }
    
    myInitialState :: ComponentState
    myInitialState = { stuff: NotAsked }

We'll continue to use this type to represent remote data throughout our application. It nicely captures the possible states of data requested from the server, and it allows all the rest of our data to be stateless: no other data has to worry about the concept of whether it's been loaded or not.

## Extra Benefits

This type lets us represent a running process with a type — `Loading` — which opens up further opportunities. 

- What if we want to prevent the user from getting data into an inconsistent state by leaving a page that's still saving data, or writing to a text field that might have data loading into it? We could check if any data on the page is in a `Loading` state and prevent these actions.
- What if we want to only navigate to the next page once the data for *that* page has been loaded? That way a user can continue interacting with the current page until the data for their next view is ready. We can use the same `Loading` state to ensure all data is ready before we move on and also render a loading bar across the top of the screen.
- We have kept our data types pure: there's no `Aff` or `Effect` being stored in the type, though it may be necessary to transition from state to state and there will be some effects performed while we are in our `Loading` state. We've successfully pushed effects to the edges of our data.

In the next section, we'll look at ways to model effectful processes while maintaining purity throughout the vast majority of our application.