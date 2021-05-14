import Link from "next/link"

import { Layout } from "../components/layout"

const IndexPage = (): JSX.Element => (
  <Layout title="Home | Next.js + TypeScript Example">
    <h1>
      Hello Next.js{" "}
      <span role="img" aria-label="wave emoji">
        👋
      </span>
    </h1>
    <p>
      <Link href="/about">
        <a>About</a>
      </Link>
    </p>
  </Layout>
)

export default IndexPage
