# `gatsby-plugin-size`

This is a friendly fork of [`gatsby-plugin-webpack-size`](https://github.com/axe312ger/gatsby-plugin-webpack-size),
the only thing that has changed is the removal of size output in development
mode and a newer version of `size-plugin`.

## Configuration

Add `'gatsby-plugin-size'` to the plugins in your `gatsby-config.js` file.

```js
module.exports = {
  plugins: [`gatsby-plugin-webpack-size`]
};
```

All options are passed to the [size-plugin](https://github.com/GoogleChromeLabs/size-plugin).

```js
module.exports = {
  plugins: [
    {
      resolve: `gatsby-plugin-webpack-size`,
      options: {
        // Set to true to show bundle sizes in development mode as well
        development: true
      }
    }
  ]
};
```
