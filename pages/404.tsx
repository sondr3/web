import { Layout } from "@/components"
import { FrontMatter } from "@/lib/mdx"

const Custom404 = (): JSX.Element => {
  const frontmatter: FrontMatter = {
    title: "404 Not Found",
    description: "There was no such page, go back and try again",
  }

  return (
    <Layout frontMatter={frontmatter}>
      <main className="py-16">
        <section className="max-w-3xl mx-auto text-center px-8">
          <h1 className="text-4xl tracking-tight font-extrabold text-gray-900 sm:text-5xl md:text-6xl lg:text-5xl xl:text-6xl">
            <span className="inline-block dark:text-gray-400">404 - not found</span>
          </h1>
          <p className="mt-3 text-lg text-gray-500 dark:text-gray-400 sm:text-xl md:mt-5 md:max-w-3xl">
            Did not find what you were looking for :(
          </p>
        </section>
      </main>
    </Layout>
  )
}

export default Custom404
