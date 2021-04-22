import * as fs from "fs"
import matter from "gray-matter"
import path from "path"

export interface Content {
  title: string
  description: string
  slug: string
  date: Date
  categories: string[]
  content: string
  draft: boolean
  [p: string]: unknown
}

const posts = path.join(process.cwd(), "content/posts")
const pages = path.join(process.cwd(), "content/pages")

export function getPostSlugs(type: "post" | "page"): string[] {
  return fs.readdirSync(type === "post" ? posts : pages)
}

export function getFileBySlug(type: "post" | "page", slug: string, fields: string[] = []): Content {
  const realSlug = slug.replace(/\.mdx$/, "")
  const fullPath = path.join(type === "post" ? posts : pages, `${realSlug}.mdx`)
  const fileContents = fs.readFileSync(fullPath, "utf8")
  const { data, content } = matter(fileContents)

  const items: Record<string, unknown> = {}

  fields.forEach((field) => {
    if (field === "slug") items[field] = realSlug
    if (field === "content") items[field] = content
    if (data[field]) items[field] = data[field]
  })

  return items as Content
}

export function getAllFiles(type: "post" | "page", fields: string[] = []): Content[] {
  const slugs = getPostSlugs(type)
  return slugs.map((slug) => getFileBySlug(type, slug, fields)).sort((p1, p2) => (p1.date > p2.date ? -1 : 1))
}
