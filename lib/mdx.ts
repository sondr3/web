import { promises as fs } from "fs"
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
  description?: string
  draft?: boolean
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

export const pagePathsToSlug = (pages: string[]): string[] => {
  return pages.map((p) => removeMDXExtension(p))
}

export const allContentByType = async (type: ContentType): Promise<string[]> => {
  const path = CONTENT_MAP[type]
  const content = await fs.readdir(path)

  const paths = []

  for (const p of content) {
    const itemPath = contentPathByPath(p, type)
    const source = await fs.readFile(itemPath)
    const { frontmatter } = await bundleMDX(source.toString())

    if (process.env.VERCEL_ENV === "production" && frontmatter.draft) {
      continue
    } else {
      paths.push(p)
    }
  }

  return paths
}

const removeMDXExtension = (filePath: string): string => {
  return filePath.replace(/\.mdx$/, "")
}

const contentPathByPath = (slug: string, type: ContentType): string => {
  const filePath = removeMDXExtension(slug)
  switch (type) {
    case "page":
      return path.join(PAGES_PATH, `${filePath}.mdx`)
    case "post":
      return path.join(POSTS_PATH, `${filePath}.mdx`)
  }
}

export const renderMDX = async (slug: string, type: ContentType): Promise<MdxContent> => {
  const itemPath = contentPathByPath(slug, type)
  const source = await fs.readFile(itemPath)
  const { code, frontmatter } = await bundleMDX(source.toString())

  return {
    mdx: code,
    frontMatter: frontmatter as FrontMatter,
  }
}
