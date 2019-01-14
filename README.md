# Real World Halogen

[![RealWorld Frontend](https://camo.githubusercontent.com/b507ac8f2ec6427bbef518193567c4ec6060c780/68747470733a2f2f696d672e736869656c64732e696f2f62616467652f7265616c776f726c642d66726f6e74656e642d2532333738333537382e737667)](http://realworld.io)
[![Maintainer: thomashoneyman](https://img.shields.io/badge/maintainer-thomashoneyman-lightgrey.svg)](http://github.com/thomashoneyman)

[PureScript] is a delightful purely-functional language that offers powerful, pragmatic tools to manage complexity and help you design, build, and refactor reliable apps of any size. This repository demonstrates the principles I apply at work every day in more than 2,000 lines of thoroughly commented code.

I’m a senior software engineer at [Awake Security] (and previously at [CitizenNet]). Both companies have large production PureScript applications that have remained reliable, understandable, and maintainable as they scale. I’m convinced PureScript is the best language for single-page applications available today.

This repository follows the [RealWorld] spec to implement a Medium clone called Conduit using [Halogen]. It’s large enough to demonstrate real world examples (CRUD, state management, scalable architecture, type classes, components, etc) but not so large as to be overwhelming..

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

If you are doing local development, I'd recommend running `yarn watch-dev` while working, and only use `yarn watch-serve` when you need to view how your changes reflect in the application (Parcel takes some time to run).

## Learning PureScript

This project is intended to give non-PureScript developers a taste of what a small application in the language looks like, and to give advanced beginners in PureScript a resource to feel comfortable building reliable applications of their own.

PureScript is a delightful language that becomes only more interesting and rewarding the more you use it and the larger your application becomes; if you haven’t yet tried it out, I encourage you to do so. Not convinced? [Kris Jenkins has a lovely talk about PureScript which might change your mind.]

### Resources

The PureScript community is overwhelmingly warm and helpful. If you would like some help getting started, please consider joining the [official Discourse] and [functional programming Slack] ([invite link]). You may also want to check out:

1. [PureScript by Example], which will teach you PureScript from scratch and was written by the language’s creator, Phil Freeman. While the official book has not been updated for the latest version of the compiler, this link is to an up-to-date fork of the book.
2. Jordan Martinez’s [PureScript reference], which has a broad overview of dozens of topics in PureScript and functional programming.
3. The [rough-draft handbook which explains the theory & principles at work in this application in-depth], which will be useful to help take you from advanced beginner to advanced intermediate in the language.
4. The [official Halogen guide], which will teach you how to use the Halogen framework to write components.
