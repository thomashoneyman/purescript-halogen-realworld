# ![RealWorld Example App](logo.png)

A PureScript + Halogen codebase containing real world examples (CRUD, auth, design patterns, routing, pagination, etc) that adheres to the [RealWorld](https://github.com/gothinkster/realworld) spec and API. 

- Use the `impl` branch to [track current progress on the guide and implementation](https://github.com/thomashoneyman/purescript-halogen-realworld/tree/impl).
- Check out the [RealWorld](https://github.com/gothinkster/realworld) project for more information on how to use this with various backends and compare it to other frameworks.
- [Read the guide](guide) to learn how to design and build applications like this in PureScript & Halogen.


> Note: This project is a work-in-progress! Please feel free to open issues, critique the guide, offer advice, or even implement part of the application.

## Learn to build a real-world Halogen application

This repository contains the full implementation of a social blogging application (a Medium.com clone) called Conduit. It also contains a thorough guide to design and build real-world applications in PureScript with the Halogen library.

[You can read the guide here](guide), which includes chapters describing:

- The requirements for the application we're building
- How to design data and pure functions
- How to push effects to the edges of the application with the ReaderT / Three Layer pattern
- How to use Halogen components effectively
- A tour of the final implementation
