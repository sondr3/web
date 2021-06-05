import { NextSeo } from "next-seo"
import { ReactNode } from "react"

import { FrontMatter } from "../lib"
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
        title: frontMatter.title,
        description: frontMatter.description,
      }}
    />
    <Navbar />
    {children}
    <Footer />
  </>
)
