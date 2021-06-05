import { getMDXComponent } from "mdx-bundler/client"
import { GetStaticPaths, GetStaticProps } from "next"
import { useMemo } from "react"

import { components, Content, Layout } from "../../components"
import { allContentByType, findPostFromSlug, MdxContent, postPathsToSlugs, renderMDX } from "../../lib"

export default function Post({ mdx, frontMatter }: MdxContent): JSX.Element {
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
