import { Layout } from "@/components"

const frontMatter = {
  title: "Home",
}

const IndexPage = (): JSX.Element => (
  <Layout frontMatter={frontMatter}>
    <main className="py-16">
      <section className="max-w-3xl mx-auto px-8">
        <h1 className="text-4xl tracking-tight font-extrabold text-gray-900 sm:text-5xl md:text-6xl lg:text-5xl xl:text-6xl">
          <span className="inline-block dark:text-gray-400">Hello! I&apos;m</span>{" "}
          <span className="inline-block text-indigo-600">Sondre</span>
        </h1>
        <p className="mt-3 text-lg text-gray-500 dark:text-gray-400 sm:text-xl md:mt-5 md:max-w-3xl">
          I am a full time nerd with a passion for programming languages, mechanical keyboards, hoarding sideprojects
          and occasionally creating useful software.
        </p>
      </section>
    </main>
  </Layout>
)

export default IndexPage
