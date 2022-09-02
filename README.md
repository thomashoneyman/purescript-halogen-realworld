# Real World Halogen

[![RealWorld Frontend](https://camo.githubusercontent.com/b507ac8f2ec6427bbef518193567c4ec6060c780/68747470733a2f2f696d672e736869656c64732e696f2f62616467652f7265616c776f726c642d66726f6e74656e642d2532333738333537382e737667)](http://realworld.io)
[![CI](https://github.com/thomashoneyman/purescript-halogen-realworld/workflows/CI/badge.svg?branch=main)](https://github.com/thomashoneyman/purescript-halogen-realworld/actions?query=workflow%3ACI+branch%3Amain)
[![Maintainer: thomashoneyman](https://img.shields.io/badge/maintainer-thomashoneyman-teal.svg)](http://github.com/thomashoneyman)

[PureScript](https://github.com/purescript) is a delightful pure functional language that offers pragmatic and powerful tools to help you design, build, and refactor reliable apps of any size. This repository demonstrates the principles I apply at work every day in more than 2,000 lines of thoroughly commented code.

I’m a developer at [Awake Security](https://github.com/awakesecurity) (and previously at [CitizenNet](https://citizennet.com)). Both companies have large production PureScript applications that have remained reliable, understandable, and maintainable as they scale.

This repository follows the [RealWorld](https://github.com/gothinkster/realworld) spec to implement a Medium clone called Conduit using [Halogen](https://github.com/purescript-halogen/purescript-halogen). It’s large enough to demonstrate real world examples (CRUD, state management, scalable architecture, type classes, components, etc.) but not so large as to be overwhelming. There is also a [Real World PureScript React](https://github.com/jonasbuntinx/purescript-react-realworld) project that demonstrates the same project using React bindings from PureScript.

The source code currently covers Halogen 7 with PureScript 0.15, but there are versions for versions going back to Halogen 4:

- [PR migrating from Halogen 6 implementation to Halogen 7](https://github.com/thomashoneyman/purescript-halogen-realworld/pull/112)
- [Halogen 6 implementation](https://github.com/thomashoneyman/purescript-halogen-realworld/tree/v3.0.0) and [PR migrating from Halogen 5](https://github.com/thomashoneyman/purescript-halogen-realworld/pull/82)
- [Halogen 5 implementation](https://github.com/thomashoneyman/purescript-halogen-realworld/tree/v2.0.0) and [PR migrating from Halogen 4](https://github.com/thomashoneyman/purescript-halogen-realworld/pull/26)
- [Halogen 4 implementation](https://github.com/thomashoneyman/purescript-halogen-realworld/tree/v1.0.0)

If you have found this repository and the guide useful, please consider [becoming a sponsor](https://github.com/sponsors/thomashoneyman)!

## Installation

First, clone the repository:

```console
$ git clone https://github.com/thomashoneyman/purescript-halogen-realworld
Cloning into 'purescript-halogen-realworld'...
...
Resolving deltas: 100% (...), done.
$ cd purescript-halogen-realworld
```

You can enter a development shell with all non-JavaScript dependencies via Nix:

```console
$ nix develop
```

Alternately, you can install `purescript`, `spago`, `purs-tidy`, `purescript-psa`, and the PureScript language server from NPM.

Next, install JavaScript dependencies:

```console
$ npm install
```

## Building and running

Next, build the project (this command will run `spago build`; see the [`package.json`](package.json) file to see
all helper scripts for the project):

```console
$ npm run build
```

You can bundle the JS for production:

```console
$ npm run bundle
```

You can run a dev server to use Conduit:

```console
$ npm run serve
...
 > Local:   http://127.0.0.1:8000/
 > Network: http://192.168.1.67:8000/
```

When you reload the application in the browser, `esbuild` will re-bundle the application using the latest compiled PureScript in the `output` directory. If you're using the PureScript language server, then this directory will be updated when you save files via incremental compilation.

## Learning PureScript

This project is intended to give non-PureScript developers a taste of what a small application in the language looks like, and to give advanced beginners in PureScript a resource to feel comfortable building reliable applications of their own.

PureScript is a delightful language that becomes only more interesting and rewarding the more you use it and the larger your application becomes; if you haven’t yet tried it out, I encourage you to do so. Not convinced? [Kris Jenkins has a lovely talk about PureScript which might change your mind](https://www.youtube.com/watch?time_continue=22&v=5AtyWgQ3vv0).

### Resources

The PureScript community is warm and helpful. If you would like some help getting started, please consider joining the [official Discourse](https://discourse.purescript.org) and [official chat](https://purescript.org/chat). You may also want to check out:

1. The (warning: currently in rough draft form) [handbook which explains the theory & principles at work in this application in-depth](https://thomashoneyman.com/guides/real-world-halogen), which will be useful to help take you from advanced beginner to advanced intermediate in the language.
1. The [official Halogen guide](https://github.com/purescript-halogen/purescript-halogen), which will teach you how to use the Halogen framework to write components.
1. [PureScript by Example](https://github.com/purescript-contrib/purescript-book), which will teach you PureScript from scratch and was written by the language’s creator, Phil Freeman. While the official book has not been updated for the latest version of the compiler, this link is to an up-to-date fork of the book.
1. [Functional Programming Made Easier](https://leanpub.com/fp-made-easier), a book by Charles Scalfani that teaches functional programming using PureScript.
1. Jordan Martinez’s [PureScript reference](https://github.com/JordanMartinez/purescript-jordans-reference), which has a broad overview of dozens of topics in PureScript and functional programming.

## Contributing

PRs are welcome! Any functional changes will need to remain compliant with the [RealWorld](https://github.com/gothinkster/realworld) spec, and I may re-word documentation changes to fit with the voice used throughout the repository.
