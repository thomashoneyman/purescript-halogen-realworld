# Real World Halogen

[![RealWorld Frontend](https://camo.githubusercontent.com/b507ac8f2ec6427bbef518193567c4ec6060c780/68747470733a2f2f696d672e736869656c64732e696f2f62616467652f7265616c776f726c642d66726f6e74656e642d2532333738333537382e737667)](http://realworld.io)
[![CI](https://github.com/thomashoneyman/purescript-halogen-realworld/workflows/CI/badge.svg?branch=main)](https://github.com/thomashoneyman/purescript-halogen-realworld/actions?query=workflow%3ACI+branch%3Amain)
[![Maintainer: thomashoneyman](https://img.shields.io/badge/maintainer-thomashoneyman-lightgrey.svg)](http://github.com/thomashoneyman)

[PureScript](https://github.com/purescript) is a delightful purely-functional language that offers powerful, pragmatic tools to manage complexity and help you design, build, and refactor reliable apps of any size. This repository demonstrates the principles I apply at work every day in more than 2,000 lines of thoroughly commented code and has an accompanying long-form [guide to building real-world Halogen applications](https://thomashoneyman.com/guides/real-world-halogen).

> Using Halogen 4? Browse the repository at the [tag for Halogen 4](https://github.com/thomashoneyman/purescript-halogen-realworld/tree/v1.0.0). Upgrading from Halogen 4 to Halogen 5? See the [PR which migrated this repository to Halogen 5](https://github.com/thomashoneyman/purescript-halogen-realworld/pull/26).

I’m a senior software engineer at [Awake Security](https://github.com/awakesecurity) (and previously at [CitizenNet](https://citizennet.com)). Both companies have large production PureScript applications that have remained reliable, understandable, and maintainable as they scale. PureScript is an incredible language for building single-page applications.

This repository follows the [RealWorld](https://github.com/gothinkster/realworld) spec to implement a Medium clone called Conduit using [Halogen](https://github.com/slamdata/purescript-halogen). It’s large enough to demonstrate real world examples (CRUD, state management, scalable architecture, type classes, components, etc.) but not so large as to be overwhelming.

If you have found this repository and the guide useful, please consider [becoming a sponsor](https://github.com/sponsors/thomashoneyman)!

## Installation

First, clone the repository:

```sh
git clone https://github.com/thomashoneyman/purescript-halogen-realworld
cd purescript-halogen-realworld
```

Install JavaScript dependencies:

```sh
npm install
```

Ensure you have PureScript dependencies available:

```sh
# You can also install these on a per-project basis
npm install --global purescript spago
```

Next, build the project (this command will run `spago build`; see the [`package.json`](package.json) file to see
all helper scripts for the project):

```sh
npm run build
```

You can bundle the JS for production (this requires installing Zephyr, [which you can get from its releases page](https://github.com/coot/zephyr/releases). Ensure it exists in your PATH by moving it to `usr/bin/local` or some equivalent).

```sh
npm run bundle
```

And, once bundled, you can run a local server to use Conduit (defaults to [port 8080](http://127.0.0.1:8080), but if this port is already in use it will increment to 8081, etc.):

```sh
npm run serve
```

## Learning PureScript

This project is intended to give non-PureScript developers a taste of what a small application in the language looks like, and to give advanced beginners in PureScript a resource to feel comfortable building reliable applications of their own.

PureScript is a delightful language that becomes only more interesting and rewarding the more you use it and the larger your application becomes; if you haven’t yet tried it out, I encourage you to do so. Not convinced? [Kris Jenkins has a lovely talk about PureScript which might change your mind](https://www.youtube.com/watch?time_continue=22&v=5AtyWgQ3vv0).

### Resources

The PureScript community is overwhelmingly warm and helpful. If you would like some help getting started, please consider joining the [official Discourse](https://discourse.purescript.org) and [functional programming Slack](https://functionalprogramming.slack.com) ([invite link](https://fpchat-invite.herokuapp.com)). You may also want to check out:

1. [PureScript by Example](https://github.com/dwhitney/purescript-book), which will teach you PureScript from scratch and was written by the language’s creator, Phil Freeman. While the official book has not been updated for the latest version of the compiler, this link is to an up-to-date fork of the book.
2. Jordan Martinez’s [PureScript reference](https://github.com/JordanMartinez/purescript-jordans-reference), which has a broad overview of dozens of topics in PureScript and functional programming.
3. The (warning: currently in rough draft form) [handbook which explains the theory & principles at work in this application in-depth](https://thomashoneyman.com/guides/real-world-halogen), which will be useful to help take you from advanced beginner to advanced intermediate in the language.
4. The [official Halogen guide](https://github.com/slamdata/purescript-halogen), which will teach you how to use the Halogen framework to write components.

## Contributing

PRs are welcome! Any functional changes will need to remain compliant with the [RealWorld](https://github.com/gothinkster/realworld) spec, and I may re-word documentation changes to fit with the voice used throughout the repository.
