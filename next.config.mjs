/** @type {import('next').NextConfig} */
const nextConfig = {
  reactStrictMode: true,
  trailingSlash: true,
  experimental: {
    esmExternals: true,
  },
  future: {
    strictPostcssConfiguration: true,
  },
  sassOptions: {
    includePath: ["./styles"],
  }
};

export default nextConfig
