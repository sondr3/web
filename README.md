# websites

This repository contains the source for my websites as well as the themes that
powers them. I've built the whole lot of them in
[Gatsby](https://www.gatsbyjs.org/) with
[TypeScript](https://www.typescriptlang.org/) and a whole host of
[ESLint](https://eslint.org/) plugins, [Prettier](https://prettier.io/) for
styling and [stylelint](https://stylelint.io/) for linting my SCSS. I've opted
to use CSS Modules instead of something like `styled-components` simply because
I have no idea what I'm doing.

Finally, everything is deployed with [Netlify](https://www.netlify.com/) using
some fun environment variable detection.

## Themes

### Base

The base theme and configuration for all my themes and websites, and contains
most of the basic building blocks for configuration, linting and so on.

## Sites

### [portfolio](https://www.eons.io/) [![Netlify Status](https://api.netlify.com/api/v1/badges/93a52e57-6728-49d3-bbde-4c9c9dfe7241/deploy-status)](https://app.netlify.com/sites/eonsio/deploys)

## Plugins

## `gatsby-plugin-size`

A simple fork of `gatsby-plugin-webpack-size` to use a newer version of `SizePlugin`.
