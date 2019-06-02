module.exports = {
  siteMetadata: {
    siteUrl: "https://www.eons.io",
    title: "Sondre Nilsen",
    titleTemplate: "%s | Sondre Nilsen",
    description: `Sondre is a computer security student with a passion for safe and reliable software, open source and programming. Firm believer in applying the KISS principle and the worse is better philosophy. Enjoyer of strongly typed languages, Linux and mechanical keyboards. `,
    author: {
      name: "Sondre Nilsen",
      bio: "I make things"
    },
    social: {
      github: "https://github.com/sondr3"
    }
  },
  plugins: [
    {
      resolve: `gatsby-plugin-manifest`,
      options: {
        name: "Eons.io",
        lang: "en",
        short_name: "Eons", // eslint-disable-line
        start_url: ".", // eslint-disable-line
        background_color: "#6b37bf", // eslint-disable-line
        theme_color: "#6b37bf", // eslint-disable-line
        display: "standalone",
        icon: "assets/icon.png"
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
            "Content-Security-Policy: default-src 'self'; script-src 'self' 'unsafe-inline' 'unsafe-eval'; object-src 'self'; style-src 'self' 'unsafe-inline'; img-src 'self' data:; media-src 'self' data:; font-src 'self' data:; connect-src 'self'",
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
    `gatsby-plugin-react-helmet`,
    {
      resolve: `gatsby-mdx`,
      options: {
        defaultLayouts: {
          default: require.resolve("./src/components/layout.jsx")
        }
      }
    },
    {
      resolve: `gatsby-source-filesystem`,
      options: {
        name: "pages",
        path: `${__dirname}/content/pages`
      }
    }
  ]
};
