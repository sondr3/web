module.exports = {
  siteMetadata: {
    siteUrl: "https://www.eons.io",
    title: "Sondre Nilsen",
    description: `Sondre is a computer security student with a passion for safe and reliable software, open source and programming. Firm believer in applying the KISS principle and the worse is better philosophy. Enjoyer of strongly typed languages, Linux and mechanical keyboards. `,
    favicon: "favicon.ico",
    author: {
      name: "Sondre Nilsen",
      bio: "I make things"
    },
    social: {
      github: "https://github.com/sondr3"
    }
  },
  __experimentalThemes: ["base"],
  plugins: [
    {
      resolve: `gatsby-plugin-manifest`,
      options: {
        name: "Eons.IO",
        short_name: "Eons" // eslint-disable-line
      }
    }
  ]
};
