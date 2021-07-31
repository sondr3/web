import Document, { Head, Html, Main, NextScript } from "next/document"

export default class MyDocument extends Document {
  render(): JSX.Element {
    return (
      <Html lang="en">
        <Head>
          <link rel="icon" href="/favicon.ico" />
          <link rel="icon" href="/icon.svg" sizes="any" type="image/svg+xml" />
          <link rel="alternate icon" type="image/png" href="/icon-192.png" sizes="192x192" />
          <link rel="alternate icon" type="image/png" href="/icon-512.png" sizes="512x512" />
          <link href="/apple-touch-icon.png" rel="apple-touch-icon" sizes="180x180" />
          <link href="/site.webmanifest" rel="manifest" />
        </Head>
        <body className="bg-white dark:bg-black transition-all duration-300">
          <Main />
          <NextScript />
        </body>
      </Html>
    )
  }
}
