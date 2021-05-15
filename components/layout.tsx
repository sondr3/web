import { NextSeo } from "next-seo"
import { ReactNode } from "react"

import { FrontMatter } from "../lib/mdx"
import { Footer } from "./footer"
import { Navbar } from "./navbar"

type Props = {
  children?: ReactNode
  frontMatter: FrontMatter
}

export const Layout = ({ children, frontMatter }: Props): JSX.Element => (
  <>
    <NextSeo title={frontMatter.title} />
    <Navbar />
    {children}
    <Footer />
  </>
)
