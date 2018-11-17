# Real World Halogen

Functional languages like PureScript, Reason, and Elm offer powerful features to manage complexity and reliably design, build, and refactor apps of any size.

PureScript is a strongly-typed, pure functional language. Most PureScript users compile to JavaScript and develop single-page web apps, though [other](https://github.com/andyarvanitis/purescript-native) [backends](https://github.com/pure-c/pure-c) [exist](https://github.com/paulyoung/pureswift) and it's been used to build [command line applications](https://github.com/feramhq/transity), [web servers](https://github.com/cprussin/purescript-httpure), and more. PureScript offers powerful features to manage complexity and help you reliably design, build, and refactor apps of any size.

The most popular single-page app framework in PureScript is the component-based `halogen`, though many commercial users migrating from React code bases will use `react-basic` instead, and fans of Elm might reach for `spork`.

This guide will walk you through how to design and build a real world Halogen application in PureScript. I'll describe the design principles I use as a professional PureScript developer to write industrial apps, and we'll put them to work to build Conduit, a social blogging app similar to Medium. This repo also contains the highly-commented source code for the app so you can see exactly how these principles translate to code.

PureScript is a strongly-typed, pure functional language. Most PureScript users compile to JavaScript and develop single-page web apps, though [other](https://github.com/andyarvanitis/purescript-native) [backends](https://github.com/pure-c/pure-c) [exist](https://github.com/paulyoung/pureswift) and it's been used to build [command line applications](https://github.com/feramhq/transity), [web servers](https://github.com/cprussin/purescript-httpure), and more. PureScript offers powerful features to manage complexity and help you reliably design, build, and refactor apps of any size.

1. [Introduction](1%20-%20ntroduction.md)
2. [What Are We Building?](2%20-%20What%20Are%20We%20Building%3F.md)
3. [Design Data & Pure Functions](3%20-%20Design%20Data%20%26%20Pure%20Functions.md)
4. [Push Effects To The Edges](4%20-%20Push%20Effects%20To%20The%20Edges.md)
5. [Using Halogen Components](5%20-%20Using%20Halogen%20Components.md)
6. [A Tour Of The Implementation](6%20-%20A%20Tour%20Of%20The%20Implementation.md)

**Prerequisites**

This is not a gentle introduction to PureScript or Halogen. It's intended for advanced beginners and intermediate PureScript developers who know how to build Halogen components but may not yet know how to build real world applications in the language & framework. If you are not yet familiar with the language or with Halogen, I recommend working through resources like [PureScript By Example](https://leanpub.com/purescript/read) and [the Halogen guide](https://github.com/slamdata/purescript-halogen/) first.

**A word of caution**

This walkthrough may not be completely in sync with the code. It covers higher-level topics and will only be updated when there are major changes to the underlying implementation. If you notice that something has fallen out of sync, please open an issue or pull request!
