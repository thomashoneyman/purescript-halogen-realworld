# Real World Halogen

Status: In Progress

Functional languages like PureScript, Reason, and Elm offer powerful features to manage complexity and reliably design, build, and refactor apps of any size.

PureScript is a strongly-typed, pure functional language. Most PureScript users compile to JavaScript and develop single-page web apps, though [other](https://github.com/andyarvanitis/purescript-native) [backends](https://github.com/pure-c/pure-c) [exist](https://github.com/paulyoung/pureswift) and it's been used to build [command line applications](https://github.com/feramhq/transity), [web servers](https://github.com/cprussin/purescript-httpure), and more. PureScript offers powerful features to manage complexity and help you reliably design, build, and refactor apps of any size.

The most popular single-page app framework in PureScript is the component-based `halogen`, though many commercial users migrating from React code bases will use `react-basic` instead, and fans of Elm might reach for `spork`.

This guide will walk you through how to design and build a real world Halogen application in PureScript. I'll describe the design principles I use as a professional PureScript developer to write industrial apps, and we'll put them to work to build Conduit, a social blogging app similar to Medium. This repo also contains the highly-commented source code for the app so you can see exactly how these principles translate to code.

# Table of Contents

[1 - Introduction](./1-Introduction-a55c0060-30e3-4ccf-a497-c79853a9b2fa.md)

[2 - What Are We Building?](./2-What-Are-We-Building-85794f1f-dd7d-4457-a826-5a6060595a2d.md)

[3 - Design Data & Pure Functions](./3-Design-Data-Pure-Functions-8a8f2b26-147f-444a-92be-01e5747a7054.md)

[4 - Push Effects To The Edges](./4-Push-Effects-To-The-Edges-3ea95b58-b79d-4709-bb98-daf92a419fa0.md)

[5 - Using Halogen Components](./5-Using-Halogen-Components-a0f2e400-0175-414a-a72d-2c9a009a46af.md)

[6 - A Tour Of The Implementation [TODO]](./6-A-Tour-Of-The-Implementation-TODO-9b646382-a3c9-4488-afcb-751a476fde1b.md)

**Prerequisites**

This is not a gentle introduction to PureScript or Halogen. It's intended for advanced beginners and intermediate PureScript developers who know how to build Halogen components but may not yet know how to build real world applications in the language & framework. If you are not yet familiar with the language or with Halogen, I recommend working through resources like [PureScript By Example](https://leanpub.com/purescript/read) and [the Halogen guide](https://github.com/slamdata/purescript-halogen/tree/v4.0.0/docs/) first.

**A word of caution**

This walkthrough may not be completely in sync with the code. It covers higher-level topics and will only be updated when there are major changes to the underlying implementation. If you notice that something has fallen out of sync, please open an issue or pull request!
