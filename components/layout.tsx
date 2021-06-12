import { NextSeo } from "next-seo"
import { ReactNode } from "react"

import { FrontMatter } from "@/lib/mdx"

import { Footer, Navbar } from "."

type Props = {
  children?: ReactNode
  frontMatter: FrontMatter
}

export const Layout = ({ children, frontMatter }: Props): JSX.Element => (
  <>
    <NextSeo
      title={frontMatter.title}
      description={frontMatter.description}
      openGraph={{
        type: "website",
        title: frontMatter.title,
        description: frontMatter.description,
        site_name: "Eons :: IO ()",
      }}
    />
    <Navbar />
    {children}
    <Footer />
  </>
)
