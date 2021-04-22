import { GetStaticPropsContext, GetStaticPropsResult } from "next"
import Head from "next/head"

import { Content } from "../components/content"
import { Layout } from "../components/layout"
import { Content as C, getAllFiles, getFileBySlug } from "../lib/mdx"

interface PageProps {
  page: C
}

export default function Page({ page }: PageProps): JSX.Element {
  console.log(page)
  return (
    <Layout>
      <Head>
        <title>{page.title} :: Eons</title>
      </Head>
      <Content header="Blah">{page.content}</Content>
    </Layout>
  )
}

export function getStaticProps({ params }: GetStaticPropsContext): GetStaticPropsResult<unknown> {
  const page = getFileBySlug("page", params?.slug as string, ["title", "excerpt", "date", "slug", "content", "draft"])

  return {
    props: { page },
  }
}

export function getStaticPaths(): { paths: { params: C }[]; fallback: boolean } {
  const pages = getAllFiles("page", ["slug"])

  return {
    paths: pages.map((page) => {
      return {
        params: { ...page },
      }
    }),
    fallback: false,
  }
}
