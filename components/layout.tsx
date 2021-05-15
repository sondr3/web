import Head from "next/head"
import { ReactNode } from "react"

import { Footer } from "./footer"
import { Navbar } from "./navbar"

type Props = {
  children?: ReactNode
  title?: string
}

export const Layout = ({ children, title = "This is the default title" }: Props): JSX.Element => (
  <div>
    <Head>
      <title>{title}</title>
      <meta charSet="utf-8" />
      <meta name="viewport" content="initial-scale=1.0, width=device-width" />
    </Head>
    <Navbar />
    {children}
    <Footer />
  </div>
)