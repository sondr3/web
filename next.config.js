const withMdx = require("@next/mdx")({
  extension: /\.mdx$/,
  options: {
    remarkPlugins: [],
    rehypePlugins: [],
  },
})

module.exports = withMdx({
  pageExtensions: ["ts", "tsx", "mdx"],
  future: {
    webpack5: true,
  },
})
