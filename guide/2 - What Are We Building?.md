# 2 - What Are We Building?

Together we'll build Conduit, a social blogging site a la Medium. We're tasked with building a minimum viable product — something functionally complete and stable, but not over-engineered — with at least a few tests.

Our lovely project management team, Thinkster, has provided us with a few resources to help guide development:

- [A detailed spec](https://github.com/gothinkster/realworld/tree/master/spec) which describes the requirements for the application
- A complete API, [available in hosted version](https://conduit.productionready.io/api) and described with [a thoughtful API spec](https://github.com/gothinkster/realworld/tree/master/api)
- Pre-built Bootstrap 4 styles [available via CDN](http://demo.productionready.io/main.css) and [sample HTML templates](https://github.com/gothinkster/realworld-starter-kit/blob/master/FRONTEND_INSTRUCTIONS.md#layout) for our pages
- A [demo frontend already built in another language](https://demo.realworld.io/) to verify things are looking good

With the spec in hand we're ready to step into the design process. Pure, strongly-typed functional languages provide a wealth of delightful tools for designing applications and we'll use them to translate our requirements into type-safe, self-documenting code.

# Functional Single-Page App Design

In a perfect world, we'd sit down in a room with the Thinkster team and ensure we completely understand the problem we're solving and develop a nuanced picture of the application we're building. Strongly-typed languages make excellent choices for domain modeling and [domain-driven design](https://pragprog.com/book/swdddf/domain-modeling-made-functional). In our case, though, we'll skip that part of the design process and refer to the requirements as our source of truth.

Following functional programming principles, we'd like to design data and functions that are as stateless, side-effect free, testable, and composable as possible. Our overall application will simply compose these small building blocks together.

We'll design our application using three concepts:

- **Use Cases**
  We'll understand the capabilities that users ought to have and the tasks they ought to be able to perform (like 'follow other users')

- **Data**
  We'll model the information in the system which represents its important **entities** (like users and articles) as well as typed values like email addresses, currencies, and so on.

- **Transformations**
  We'll write functions to transform data from state to state or represent the actions users can take to change the system. These functions should be pure and small if possible.

Let's understand the use cases Conduit ought to support, and then move on to our data and transformations.

# Use Cases

Use cases ought to describe what a user should be able to do and the goal they are trying to accomplish. For Conduit, a social blogging site, a user ought to be able to:

- Create an account, so they have a persistent identity
- View resources like an article feed, articles, their profile, other users, and more
- Create and edit their own articles, so they can share their writing with the community
- Tag their own articles, so that they and other users can filter their feed by various topics
- Create and delete (their own) comments on articles, so they can share what they think about an article with the community
- Update their settings and user profile
- Favorite and unfavorite others' articles, so that favorite articles show on their profile
- Follow and unfollow other users, so that they can see their articles in their feed
- Log out and sign in, so that they can control their session

This is a small list of use cases to implement. Usually I'd spend much longer on this step to ensure my team completely understands what users want to accomplish in our application, but in our case we've simply drawn from our spec.

A quick review of [the API spec](https://github.com/gothinkster/realworld/tree/master/api) reveals that endpoints have already been created to load each of these resources and perform the CRUD operations we need. For example, we can create tags, create articles, authenticate users, and more.

In the next step, we'll design the data by modeling the entities involved in the system and how they relate to one another.
