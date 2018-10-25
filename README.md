# ![RealWorld Example App](logo.png)

> ### PureScript + Halogen codebase containing real world examples (CRUD, auth, advanced patterns, etc) that adheres to the [RealWorld](https://github.com/gothinkster/realworld) spec and API.

This codebase was created to demonstrate a fully fledged fullstack application built with PureScript + Halogen including CRUD operations, authentication, routing, pagination, and more.

We've gone to great lengths to adhere to PureScript & Halogen best practices.

For more information on how to this works with other frontends/backends, head over to the [RealWorld](https://github.com/gothinkster/realworld) repo.


# Getting started

To get the frontend running locally:

```
# Install dependencies via NPM and Bower
yarn

# Build the project (alternately, use `npm run build`)
yarn build

# Run a local API server (alternately, use `npm run start`)
yarn start
```

# How it works

This example application is a social blogging site (ie. a Medium.com clone) called "Conduit". It uses a custom API for all requests, including authentication.

**General functionality**

- [ ] Authenticate users
- [ ] CRU* users (with a signup and settings page, no deletion required)
- [ ] CRUD articles
- [ ] CR*D comments on articles (no updating required)
- [ ] GET and display paginated lists of articles
- [ ] Favorite articles
- [ ] Follow other users

**Site structure overview**

- [ ] Home page | `/#/`
  - [ ] List of tags
  - [ ] List of articles pulled from either "Feed", "Global", or by "Tag"
  - [ ] Pagination for list of articles
- [ ] Sign up / sign in pages | `/#/login`, `/#/register`
  - [ ] Use JWT, storing the token in local storage
- [ ] Settings page | `/#/settings`
- [ ] Editor page to create / edit articles | `/#/editor`, `/#/editor/article-slug`
- [ ] Article page | `/#/article/article-slug`
  - [ ] Delete article button (only shown to article author)
  - [ ] Render markdown from server client side
  - [ ] Comment section at bottom of page
  - [ ] Delete comment button (only shown to comment author)
- [ ] Profile page | `/#/@username`, `/#/@username/favorites`
  - [ ] Show basic user info
  - [ ] List of articles populated from author's created or favorited articles
