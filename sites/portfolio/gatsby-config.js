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
  __experimentalThemes: ["base"],
  plugins: [
    {
      resolve: `gatsby-plugin-manifest`,
      options: {
        name: "Eons.IO",
        lang: "en",
        short_name: "Eons", // eslint-disable-line
        start_url: ".", // eslint-disable-line
        background_color: "#6b37bf", // eslint-disable-line
        theme_color: "#6b37bf", // eslint-disable-line
        display: "standalone"
      }
    }
  ]
};
