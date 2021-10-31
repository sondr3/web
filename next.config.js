/** @type {import('next').NextConfig} */
module.exports = {
  reactStrictMode: true,
  trailingSlash: true,
  experimental: {
    esmExternals: true,
  },
  future: {
    strictPostcssConfiguration: true,
  },
};
