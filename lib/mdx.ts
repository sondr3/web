import { promises as fs } from "fs"
import matter from "gray-matter"
import { bundleMDX } from "mdx-bundler"
import path from "path"

const CONTENT_PATH = path.join(process.cwd(), "content")
export const POSTS_PATH = path.join(CONTENT_PATH, "posts")
export const PAGES_PATH = path.join(CONTENT_PATH, "pages")

type ContentType = "page" | "post"

const CONTENT_MAP = {
  page: PAGES_PATH,
  post: POSTS_PATH,
}

export interface FrontMatter {
  title: string
}

export interface MdxContent {
  mdx: string
  frontMatter: FrontMatter
}

export const findPostFromSlug = async (slug: string): Promise<string> => {
  const posts = await allContentByType("post")
  const combined = postPathsToSlugs(posts)
  const post = combined.find((p) => p.slug == slug)

  if (post === undefined) {
    throw new Error("Could not find post")
  }

  return post.path
}

export const postPathsToSlugs = (posts: string[]): Array<{ path: string; slug: string }> => {
  const slugs = posts.map((p) => p.replace(/\.mdx$/, "").replace(/\d{4}-\d{2}-\d{2}-/, "")).map((slug) => slug)

  const combined = posts.map((p, i) => ({ slug: slugs[i], path: p }))

  return combined
}

export const allContentByType = async (type: ContentType): Promise<string[]> => {
  const path = CONTENT_MAP[type]
  return fs.readdir(path)
}

const contentPathByPath = (slug: string, type: ContentType): string => {
  switch (type) {
    case "page":
      return path.join(PAGES_PATH, `${slug}.mdx`)
    case "post":
      return path.join(POSTS_PATH, slug)
  }
}

export const renderMDX = async (slug: string, type: ContentType): Promise<MdxContent> => {
  const itemPath = contentPathByPath(slug, type)
  const source = await fs.readFile(itemPath)
  const { data, content } = matter(source)
  const { code } = await bundleMDX(content)

  return {
    mdx: code,
    frontMatter: data as FrontMatter,
  }
}
