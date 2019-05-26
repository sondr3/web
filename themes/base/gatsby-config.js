/* eslint-disable @typescript-eslint/no-var-requires */
const autoprefixer = require("autoprefixer");
const browserslist = require("browserslist");
const cssnano = require("cssnano");

module.exports = {
  siteMetadata: {
    siteUrl: "https://www.example.com",
    title: "Base template for my stuff",
    titleTemplate: "%s | BASE",
    description: "This is what you're going to use for everything",
    author: {
      name: "Sondre Nilsen",
      bio: "I am groot"
    },
    social: {
      github: "https://github.com"
    }
  },
  plugins: [
    {
      resolve: `gatsby-plugin-typescript`,
      options: {
        isTSX: true,
        allExtensions: true
      }
    },
    {
      resolve: `gatsby-plugin-sass`,
      options: {
        cssLoaderOptions: { localIdentName: "[local]--[hash:base64:5]" },
        postCssPlugins: [
          autoprefixer({
            browsers: browserslist(["> 1% in NO", "not ie >= 8"])
          }),
          cssnano({ preset: "default" })
        ]
      }
    },
    {
      resolve: `gatsby-plugin-manifest`,
      options: {
        name: "Base template",
        lang: "en",
        short_name: "BASE", // eslint-disable-line
        start_url: ".", // eslint-disable-line
        background_color: "#6b37bf", // eslint-disable-line
        theme_color: "#6b37bf", // eslint-disable-line
        display: "standalone",
        icon: "src/icon.png",
        cache_busting_mode: "name" // eslint-disable-line
      }
    },
    `gatsby-plugin-sitemap`,
    `gatsby-plugin-robots-txt`,
    `gatsby-plugin-offline`,
    {
      resolve: `gatsby-plugin-netlify`,
      options: {
        headers: {
          "/*": [
            "Content-Security-Policy: default-src 'self'; script-src 'self' 'unsafe-inline'; object-src 'self'; style-src 'self' 'unsafe-inline'; img-src 'self'; media-src 'self'; font-src 'self'; connect-src 'self'",
            "Feature-Policy: autoplay 'none'; camera 'none'; encrypted-media 'none'; fullscreen 'none'; geolocation 'none'; microphone 'none'; midi 'none'; payment 'none'; vr 'none'",
            "Referrer-Policy: strict-origin-when-cross-origin",
            "Strict-Transport-Security: max-age=63072000; includeSubDomains; preload",
            "X-Content-Type-Options: nosniff",
            "X-Frame-Options: DENY",
            "X-XSS-Protection: 1; mode=block"
          ]
        },
        mergeSecurityHeaders: false
      }
    },
    `gatsby-plugin-netlify-cache`,
    `gatsby-plugin-react-helmet`,
    {
      resolve: `gatsby-mdx`,
      options: {
        defaultLayouts: {
          default: require.resolve("./src/components/layout.tsx")
        }
      }
    },
    {
      resolve: `gatsby-plugin-page-creator`,
      options: {
        path: `${__dirname}/src/pages`
      }
    },
    `gatsby-plugin-size`
  ]
};
