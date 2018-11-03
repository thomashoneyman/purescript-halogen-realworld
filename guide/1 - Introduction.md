# 1 - Introduction

I spent most of 2018 working on a moderate (50k total lines of code, 30k PureScript) Halogen application at [CitizenNet](https://citizennet.com/). The PureScript community is brimming with delightful, helpful people, and my team and I received plenty of advice on best practices from commercial PureScript users at companies like SlamData, Awake Security, Lumi, and more.

Everything we learned about best practices for building a large Halogen app went straight into yet another closed-source code base. What a shame! Our community deserves a resource that demonstrates how to build real world PureScript applications.

## The RealWorld Project

Luckily, I stumbled across [the RealWorld project](https://github.com/gothinkster/realworld). The folks at Thinkster realized that building a tiny todo app differs greatly from building a fully-fledged modern single-page application. Why evaluate frameworks and languages based on toy examples when you intend to use these tools to build a large application?

The RealWorld project provides an API, markup, styles, and a light feature spec for a Medium clone called Conduit. You build the single-page app frontend for it against your choice of already-implemented backends.

This struck me as a perfect way to help beginner and intermediate PureScript developers feel confident building medium-sized applications! I decided to build [a RealWorld implementation for Halogen](https://github.com/thomashoneyman/purescript-halogen-realworld) and write about the design process and my implementation decisions along the way.

## Why Halogen?

Unlike other compile-to-JS languages like Elm, PureScript has no opinion on the design of your application and doesn’t provide built-in capabilities for building user interfaces. Most PureScript companies (as of 2018) use one of these libraries:

- [`purescript-halogen`](https://github.com/slamdata/purescript-halogen) is a component-based framework with a 100% PureScript virtual DOM implementation (it’s also used by Spork) for users who prefer a component architecture. This is the most common choice for users who do not need to hook into another ecosystem like React.
- [`purescript-react`](https://github.com/purescript-contrib/purescript-react) and [`purescript-react-basic`](https://github.com/lumihq/purescript-react-basic) enable users to write logic in PureScript, but use React as their underlying UI library and hook into the existing React ecosystem.
- [`purescript-spork`](https://github.com/natefaubion/purescript-spork) is an Elm-like framework, enabling users to opt-in to the Elm design philosophy but take advantage of PureScript language features like row types and type classes.

Most of the design and implementation decisions made in this project apply to any PureScript single-page app. However, there's plenty of Halogen-specific information here that won't translate perfectly to the other frameworks. If you work in another framework, take any Halogen-specific code or design principles with full knowledge that they may not apply to your choice of library.

## Designing the Conduit Application

There is no "correct" way to design an application like Conduit. But best practices do exist  for building applications in purely functional languages, and this guide will walk through my understanding of those best practices as they apply to single-page applications.

We'll dive in to a few design steps that apply to Halogen single-page apps. After a brief introduction to each step (with links to related reading), I'll walk through how to apply that principle to flesh out our design for the application. This is a high-level overview, so we won't actually implement the app as we go; if you'd like to see the concrete implementations, check out the source code! By the time we finish these steps we'll have a solid foundation to stand on when we start our implementation.

**A note for expert readers**

I am applying my imperfect knowledge to build Conduit in a way that demonstrates how I think Halogen applications ought to be built. Some of you will disagree with my choices. If you do, please consider filing an issue or reaching out to describe an approach you would consider more appropriate.
