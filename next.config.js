const withPWA = require("next-pwa")
const runtimeCaching = require("next-pwa/cache")

module.exports = withPWA({
  reactStrictMode: true,
  trailingSlash: true,
  pwa: {
    dest: "public",
    disable: process.env.VERCEL_ENV === "development",
    runtimeCaching,
  },
})
