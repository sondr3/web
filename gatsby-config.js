module.exports = {
  siteMetadata: {
    siteUrl: "https://www.eons.io",
    title: "Sondre Nilsen",
    titleTemplate: "%s | Sondre Nilsen",
    description: `Sondre is a computer technology student with a passion for safe and reliable software, open source and programming. Firm believer in applying the KISS principle and the worse is better philosophy. Enjoyer of strongly typed languages, Linux and mechanical keyboards. `,
    author: {
      name: "Sondre Nilsen",
      intro: "I make things",
      bio: "",
    },
    social: {
      github: "https://github.com/sondr3",
    },
  },
  plugins: [
    {
      resolve: `gatsby-plugin-typescript`,
      options: {
        isTSX: true,
        allExtensions: true,
      },
    },
    {
      resolve: `gatsby-plugin-sass`,
      options: {
        implementation: require("sass"),
      },
    },
    {
      resolve: `gatsby-plugin-typography`,
      options: {
        pathToConfigModule: "src/styles/typography",
        omitGoogleFont: true,
      },
    },
    `gatsby-plugin-sitemap`,
    `gatsby-plugin-robots-txt`,

    `gatsby-plugin-react-helmet`,
    `gatsby-transformer-json`,
    {
      resolve: `gatsby-source-filesystem`,
      options: {
        name: "pages",
        path: `${__dirname}/content/pages`,
      },
    },
    {
      resolve: `gatsby-source-filesystem`,
      options: {
        name: "projects",
        path: `${__dirname}/content/projects`,
      },
    },
    {
      resolve: `gatsby-source-filesystem`,
      options: {
        name: "data",
        path: `${__dirname}/content/data`,
      },
    },
    {
      resolve: `gatsby-source-graphql`,
      options: {
        typeName: "CV",
        fieldName: "cv",
        url: "https://cv.eons.io/graphql",
      },
    },
    {
      resolve: `gatsby-plugin-mdx`,
      options: {
        defaultLayouts: {
          default: require.resolve("./src/components/common/layout.tsx"),
        },
      },
    },
    // `gatsby-plugin-webpack-bundle-analyser-v2`,
  ],
};
