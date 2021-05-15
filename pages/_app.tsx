import "tailwindcss/tailwind.css"

import { AppProps } from "next/app"
import Head from "next/head"
import { DefaultSeo } from "next-seo"

import { defaultSEOConfig } from "../utils"

const App = ({ Component, pageProps }: AppProps): JSX.Element => {
  return (
    <>
      <Head>
        <meta charSet="utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no" />
        <meta httpEquiv="X-UA-Compatible" content="IE=edge,chrome=1" />
      </Head>
      <DefaultSeo {...defaultSEOConfig} />
      <Component {...pageProps} />
    </>
  )
}

export default App
