import { getMDXComponent } from "mdx-bundler/client"
import { GetStaticPaths, GetStaticProps } from "next"
import React, { useMemo } from "react"

import { components, Content, Layout } from "@/components"
import { allContentByType, MdxContent, pagePathsToSlug, renderMDX } from "@/lib"

export default function Page({ mdx, frontMatter }: MdxContent): JSX.Element {
  const Component = useMemo(() => getMDXComponent(mdx), [mdx])

  return (
    <Layout frontMatter={frontMatter}>
      <Content title={frontMatter.title}>
        <Component components={components} />
      </Content>
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
