import { DefaultSeoProps } from "next-seo"

export const defaultSEOConfig: DefaultSeoProps = {
  titleTemplate: "%s => Eons :: IO ()",
  description: "The online home for Sondre Nilsen",
  canonical: "https://www.eons.io",
  openGraph: {
    url: "https://www.eons.io",
    title: "Eons :: IO ()",
    description: "The online home for Sondre Nilsen",
    site_name: "Eons :: IO ()",
  },
  twitter: {
    handle: "@sondr3",
  },
}
