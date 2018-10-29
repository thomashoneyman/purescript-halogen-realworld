# Introduction

PureScript is a powerful, strongly-typed, pure functional language. Most PureScript users compile to JavaScript and develop web applications, though [other backends exist](https://github.com/pure-c/pure-c) and it's been used to build [command line applications](https://github.com/feramhq/transity), [web servers](https://github.com/cprussin/purescript-httpure), and more.

I spent most of 2018 working on a moderate (50k total lines of code, 30k PureScript) Halogen application at [CitizenNet](https://citizennet.com/). The PureScript community is brimming with delightful, helpful people, and my team and I received plenty of advice on best practices from commercial PureScript users at companies like SlamData, Awake Security, Lumi, and more.

Unfortunately, everything we learned about best practices for building a mature industrial Halogen application went straight into yet another closed-source code base. What a shame! Our community deserves a resource that demonstrates these best practices in a real world setting.

Now it exists! Thousands of lines of PureScript demonstrating best practices for a real world Halogen application. Feel free to review the code, open issues, offer improvements, and dive in!

## RealWorld PureScript

At the end of 2018, I stumbled across [the RealWorld project](https://github.com/gothinkster/realworld). The folks at Thinkster realized that building a tiny todo app is much different from building a fully-fledged modern single-page application, and there's not much sense evaluating various frameworks or languages based on small examples when the use case is a complex web app.

The RealWorld project provides an API, markup, styles, and a light feature spec for a Medium clone called Conduit. You build the single-page app frontend for it against your choice of already-implemented backends.

This struck me as a perfect way to help beginner PureScript developers feel confident building medium-sized applications! I decided to build [a RealWorld implementation for Halogen](https://github.com/thomashoneyman/purescript-halogen-realworld) and write about the design process and my implementation decisions along the way.

## Why Halogen?

Unlike other compile-to-JS languages like Elm, PureScript has no opinion on the design of your application and doesn't provide built-in capabilities for building user interfaces. Most PureScript companies (as of 2018) use one of these libraries:

- `[purescript-halogen](https://github.com/slamdata/purescript-halogen)` is a component-based framework with a 100% PureScript virtual DOM implementation (it's also used by Spork) for users who prefer a component architecture. This is the most common choice for users who do not need to hook into another ecosystem like React.
- `[purescript-react](https://github.com/purescript-contrib/purescript-react)` and `[purescript-react-basic](https://github.com/lumihq/purescript-react-basic)` enable users to write logic in PureScript, but use React as their underlying UI library and hook into the existing React ecosystem.
- `[purescript-spork](https://github.com/natefaubion/purescript-spork)` is an Elm-like framework, enabling users to opt-in to the Elm design philosophy but take advantage of PureScript language features like row types and type classes.

Most of the design decisions I've made in this project also apply to users of other frameworks and libraries, but buyer beware: I have only used Halogen professionally, and have designed this application specifically for Halogen.

# The RealWorld Halogen Guide

Modern web applications are often tremendously difficult create *and maintain*. Functional languages like PureScript offer powerful features to manage complexity and reliably design, build, and refactor even large apps. If all you need is some example code, then [feel free to dive right in to the source](https://github.com/thomashoneyman/purescript-halogen-realworld). The rest of this guide will focus on how to design and build an application like Conduit in a language like PureScript.

1. [Introduction](1%20-%20Introduction.md)

*Note: This walkthrough may not be completely in sync with the code. It covers higher-level topics and will only be updated when there are major changes to the underlying implementation.*
