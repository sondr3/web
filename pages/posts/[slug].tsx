import { getMDXComponent } from "mdx-bundler/client"
import { GetStaticPaths, GetStaticProps } from "next"
import { useMemo } from "react"

import { Content } from "../../components/content"
import { Layout } from "../../components/layout"
import { allContentByType, findPostFromSlug, MdxContent, postPathsToSlugs, renderMDX } from "../../lib/mdx"

export default function Post({ mdx, frontMatter }: MdxContent): JSX.Element {
  const Component = useMemo(() => getMDXComponent(mdx), [mdx])

  return (
    <Layout title={frontMatter.title}>
      <Content title={frontMatter.title}>
        <Component />
      </Content>
    </Layout>
  )
}

export const getStaticProps: GetStaticProps = async ({ params }) => {
  const filePath = await findPostFromSlug(params?.slug as string)
  const { mdx, frontMatter } = await renderMDX(filePath, "post")

  return {
    props: {
      mdx,
      frontMatter,
    },
  }
}

export const getStaticPaths: GetStaticPaths = async () => {
  const posts = await allContentByType("post")
  const paths = postPathsToSlugs(posts)

  return {
    paths: paths.map((p) => ({ params: { slug: p.slug } })),
    fallback: false,
  }
}
