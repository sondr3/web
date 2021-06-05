import "tailwindcss/tailwind.css"

import { AppProps } from "next/app"
import Head from "next/head"
import { DefaultSeo } from "next-seo"
import { ThemeProvider } from "next-themes"

import { defaultSEOConfig } from "@/utils"

const App = ({ Component, pageProps }: AppProps): JSX.Element => {
  return (
    <>
      <Head>
        <meta charSet="utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no" />
        <meta httpEquiv="X-UA-Compatible" content="IE=edge,chrome=1" />

        <meta name="application-name" content="Eons :: IO ()" />
        <meta name="apple-mobile-web-app-capable" content="yes" />
        <meta name="apple-mobile-web-app-status-bar-style" content="default" />
        <meta name="apple-mobile-web-app-title" content="Eons :: IO( ()" />
        <meta name="description" content="The online home for Sondre Nilsen" />
        <meta name="format-detection" content="telephone=no" />
        <meta name="mobile-web-app-capable" content="yes" />
        <meta name="theme-color" content="#ffffff" />
      </Head>
      <DefaultSeo {...defaultSEOConfig} />
      <ThemeProvider attribute="class">
        <Component {...pageProps} />
      </ThemeProvider>
    </>
  )
}

export default App
