# 4 - Push Effects To The Edges

So far we've spent all our time writing core data types and the types of functions that will operate on them. We haven't implemented any of these functions yet. We could go ahead and implement any pure functions we'd like, but we should pause before implementing any non-pure functions (functions with `Aff` or `Effect` in their type). We need to be careful about how we design effects in our system.

A core principle in purely functional programming is to separate effects and data as much as possible. This naturally leads to applications with a [functional core](https://www.destroyallsoftware.com/talks/boundaries) and [imperative shell](https://www.destroyallsoftware.com/screencasts/catalog/functional-core-imperative-shell). The vast majority of code is written as side-effect-free functions and data and only at the boundaries of the application do the effects show up. The boundaries of the application are where our core logic meets the outside world, whether via API requests, outside input, components rendering to the page, and so on.

What about Conduit? At first glance, the application is almost entirely made up of boundaries! Most of our data loads from the server or local storage, and we barely decode and transform it before we're making another request or rendering. Even so, we'll find that we can still build a substantial functional core with only a thin shell of code with effects.

In this section we'll review a common architecture for Halogen applications and then put it into practice to implement our core capabilities.

## Table of Contents

[1 - Principles for Designing Capabilities](./1-Principles-for-Designing-Capabilities-55916450-5853-4b2e-9455-16f0a6e89a65.md)

[2 - Capabilities in Conduit](./2-Capabilities-in-Conduit-c592c029-514b-4936-9a02-eb4ec8ade74b.md)

[3 - Designing & Implementing A Logging Capability](./3-Designing-Implementing-A-Logging-Capability-9680667b-1e6f-4404-ba37-ebc957ee351d.md)