import Head from "next/head"

import { Content } from "../components/content"
import { Layout } from "../components/layout"

export default function about(): JSX.Element {
  return (
    <Layout>
      <Head>
        <title>About me :: Eons</title>
      </Head>
      <Content header="About me">
        <p>
          Hello, I’m Sondre! I’m a master student on my first year studying algorithms at the{" "}
          <a href="https://www.uib.no/">University of Bergen</a> with a passion for safe and reliable software, open
          source and programming. I’m also a firm believer in applying the KISS principle and the worse is better
          philosophy to my projects. I enjoy and use strongly typed languages, Linux and mechanical keyboards.
        </p>
        <h2>Contact me</h2>
        <p>
          If you want to reach out, my email is <code>my-first-name</code> at this domain.
        </p>
      </Content>
    </Layout>
  )
}
