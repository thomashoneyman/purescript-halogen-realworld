# 5 - Using Halogen Components

Finally, let's briefly discuss Halogen. Halogen is a [declarative, type-safe UI library](https://github.com/slamdata/purescript-halogen) for PureScript applications. It's the most commonly used framework for applications implemented entirely in PureScript, while projects that depend heavily on the React ecosystem tend to use `purescript-react-basic` and `purescript-react`.

_Note: This guide demonstrates how to build Halogen applications, but is not a tutorial on Halogen itself. If you don't already know how to write Halogen components, I'd recommend [reading the Halogen guide](https://github.com/slamdata/purescript-halogen/tree/v4.0.0/docs/), checking out [the example components](https://github.com/slamdata/purescript-halogen/tree/master/examples), and reading [the Halogen refresher](https://citizennet.github.io/purescript-halogen-select/tutorials/getting-started/#a-whirlwind-tour-of-our-starter-component) in the Select documentation._

Halogen applications are usually designed according to the architecture I've already described:

1. Design almost all code in a pure, functional style
2. Use type classes to represent capabilities like reading and writing to local storage, routing, and logging
3. Implement a thin layer of highly stateful code to actually perform requests, read and write to local storage, and so on — a layer which can easily be swapped out for mocks when testing without changing the functional core.
4. Store application-wide information in `ReaderT` and represent global state with a `Ref` instead of a state monad
5. Manage UI state and interactions with relatively monolithic components (compared to what you might see in React), preferring pure functions instead of components for parts of the application that don't require internal state or communication with the browser or other components

We've already discussed all but the final point. How should you implement your views in Halogen? When should you reach for a component, and when should you use pure render functions? What are some design decisions commonly made by Halogen users?

## Use Your Capabilities & Pure Functions

At this point, we've spent a lot of time carefully designing pure functions and capabilities while pushing effects to the edges of our application. Components are often a part of that effectful outer shell, but they don't need to be, and as much as possible we'd like to keep our components pure, too. For components that can't be pure, we'd still like to keep as much of the component pure as we can, just like we've done for our entire application.

A component like this one is has fully-encapsulated state and an event loop, but (until it is run by Halogen, the underlying implementation) it is pure:

```purescript
myComponent :: forall m. Component HTML Query Input Message m
```

You can't necessarily reason about its ultimate effects, because at some point it will run and interact with the DOM. However, you _can_ reason about what effects it performs directly: none! You receive the same advantages as you do from using pure functions throughout your code. For example, you can [test this component as a state machine](http://qfpl.io/posts/intro-to-state-machine-testing-1/).

We can give this component access to the capabilities we've designed by adding constraints:

```purescript
myComponent :: forall m. LocalStorage m => Component HTML Query Input Message m
```

Your component can now access your pure functions defined in your local storage capability and use it as part of its behaviors.

Render functions are pure, too, and most components will be made up of several of these pure functions. It's common to implement commonly-used parts of views (like a header or footer) as pure render functions instead of independent components:

```purescript
-- Given an user profile, render a header with their username (for example)
renderHeader :: forall p i. Profile -> HTML p i
```

## Reach For Components Last

Halogen developers don't reach for components right away. Components are useful to manage statefulness and communication with the rest of the application in addition to rendering HTML. If you just need to render some HTML or it is simple to pass in state and events, then just use a small function. Later, you can use this function within a larger component which is responsible for producing the required input.

A normal Halogen application will have components for the main views in the application and for smaller but highly stateful building blocks like typeaheads, date pickers, charts, and so on. The main views are then made up of plenty of pure functions which handle the majority of the rendering. Those pure functions may require effects to produce their inputs, and in that case the component will need to be effectful.

Ask yourself: Am I just rendering HTML? Is it simple to pass in state and events? Use small, pure functions if you can, and reach for components when only when you need state and effects.

## Go Renderless For Reusable Components

Components are reusable bundles of inputs, outputs, state, behavior, and rendering. Used here, reuse describes the ability to use the same component over and over again across different views in your application without needing to adjust the underlying component.

There is another way to look at reuse, however, and that's the ability to share some of a component's internals with other components. For example, let's consider implementing a dropdown and a typeahead. Both of these components:

- Manage a collection of items that can be selected and zero or more items that _are_ selected;
- Render some input (whether a button or text field) and a container of items and allow users to use keyboard navigation or click to select and de-select items
- Provide the ability to search the collection of items, whether by typing on a button and matching characters from the beginning of a string (dropdown) or by some possibly quite complex search algorithm (typeahead) that might allow fuzzy matching and so on
- Raise (as outputs) events like visibility changes, which items have been selected, and other important user interactions on the input, container, and items.

It seems a shame to implement all sorts of complex logic to manage which item is currently highlighted, what items a user has selected, how to insert new items into the collection (and so on) two times. It would be nice if we could share some of the underlying behavior and state management between both components!

There are only two design patterns that provide true reuse in this sense of the word: **higher-order** and **renderless** components.

- Higher-order components are a common pattern in the React community and refer to components that take a component as an argument, augment it with new behaviors and state, and return the new, augmented component.
- Renderless components have no existing render function at all, and allow you to extend a component with new behaviors and state via a parent component which provides the render function. For various reasons renderless components are more common — at least in open source.

**Building your own renderless components**

I gave [a talk about renderless components](https://www.youtube.com/watch?v=igWrktC0m7E) at Los Angeles PureScript, and another about reusable components at LambdaConf 2018. These talks expand on the motivation for these kinds of components and helps explain how to implement them.

In addition, **Renderless** is a library for [designing and implementing renderless components](https://github.com/thomashoneyman/purescript-halogen-renderless). It provides:

- helpers for working with a `Store` comonad as your component state
- minimal starting templates for your own renderless components
- guides and best practices to help you design components in this style

If you need some inspiration, try exploring [Formless](https://github.com/thomashoneyman/purescript-halogen-formless) and [Select](https://github.com/citizennet/purescript-halogen-select), two open-source Halogen components that use the renderless style.

## A Brief Review Of Performance Considerations

Halogen is a reasonably performant framework, but it's easy to cause excessive computation and re-rendering in a component. As a rule of thumb, I like to follow a few guidelines:

1. **Remember that Halogen re-renders on state updates**
   Every time the state of your component changes, Halogen will re-run your render function and use its virtual DOM to diff changes against the current state of your UI in the browser. This loop is run whether you actually changed how your page should render or not. Be careful about excessively updating your component state and try to minimize the number of times you call `modify` or `put`.

2. **Cache expensive computations in state, rather than re-run them each render**
   Some views might involve expensive computations. For example, a date picker may generate a month's worth of dates and then format their display depending on the currently-selected date. If you simply calculate these dates in your render function, then — whether or not they needed to update — they'll be recomputed from scratch every time your state updates. If you track which date is highlighted by the user then a simple mouse movement can be a real problem. A better option is to calculate the dates only when the input date changes, and in your render function just retrieve the dates from your component state.

3. **Be careful with your `receiver` function**
   The `receiver` function allows you to listen to a stream of inputs from a parent component. Every time the parent component re-renders the `receiver` will run in the child component. If your receiver writes to your component state, then every re-render in the parent will re-render the child, too. This can cause excessive rendering. Depending on how expensive it is to re-render a child component and how many there are, this can cause performance problems.
