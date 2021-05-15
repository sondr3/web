import { getMDXComponent } from "mdx-bundler/client"
import { GetStaticPaths, GetStaticProps } from "next"
import { useMemo } from "react"

import { Layout } from "../components/layout"
import { allContentByType, MdxContent, pagePathsToSlug, renderMDX } from "../lib/mdx"

export default function Page({ mdx, frontMatter }: MdxContent): JSX.Element {
  const Component = useMemo(() => getMDXComponent(mdx), [mdx])

  return (
    <Layout title={frontMatter.title}>
      <Component />
    </Layout>
  )
}

export const getStaticProps: GetStaticProps = async ({ params }) => {
  const { mdx, frontMatter } = await renderMDX(params?.slug as string, "page")

  return {
    props: {
      mdx,
      frontMatter,
    },
  }
}

export const getStaticPaths: GetStaticPaths = async () => {
  const paths = await allContentByType("page")
  const pages = pagePathsToSlug(paths)

  return {
    paths: pages.map((p) => ({ params: { slug: p } })),
    fallback: false,
  }
}
