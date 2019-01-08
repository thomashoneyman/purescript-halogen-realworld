# Real World Halogen

[![RealWorld Frontend](https://camo.githubusercontent.com/b507ac8f2ec6427bbef518193567c4ec6060c780/68747470733a2f2f696d672e736869656c64732e696f2f62616467652f7265616c776f726c642d66726f6e74656e642d2532333738333537382e737667)](http://realworld.io)
[![Maintainer: thomashoneyman](https://img.shields.io/badge/maintainer-thomashoneyman-lightgrey.svg)](http://github.com/thomashoneyman)

A PureScript + Halogen codebase containing real world examples (CRUD, auth, design patterns, routing, pagination, etc) that adheres to the [RealWorld](https://github.com/gothinkster/realworld) spec and API. 

- Check out the [RealWorld](https://github.com/gothinkster/realworld) project for more information on how to use this with various backends and compare it to other frameworks.
- [Read the guide](https://thomashoneyman.com/guides/real-world-halogen) to learn how to design and build applications like this in PureScript & Halogen.

## Installation

You can install and build the project with:

```sh
# clone the repository
git clone https://github.com/thomashoneyman/purescript-halogen-realworld
cd purescript-halogen-realworld

# install dependencies
yarn install

# build the project and run the local server
yarn build-serve
```

If you are doing local development, I'd recommend using `yarn watch-dev` while working, and only use `yarn watch-serve` when you need to view how your changes reflect in the application (Parcel takes some time to run).

# Learn to build a real-world Halogen application

This repository contains the full implementation of a social blogging application (a Medium.com clone) called Conduit. It also contains a thorough guide to design and build real-world applications in PureScript with the Halogen library.

The guide walks you through how to design and build a real world Halogen application in PureScript. I’ll describe the design principles I use as a professional PureScript developer to write industrial apps, and we’ll put them to work to build Conduit. [You can read the guide here](https://thomashoneyman.com/guides/real-world-halogen), which includes chapters describing:

- The requirements for the application we're building
- How to design data and pure functions
- How to push effects to the edges of the application with the ReaderT / Three Layer pattern
- How to use Halogen components effectively
- A tour of the final implementation

---

## Contributors

Several people helped this project along with code contributions, technical review, proofreading, and advice, including:

- [jfraudeau](https://github.com/jfraudeau)
- [davezuch](https://github.com/davezuch)
