import { Layout } from "../components/layout"

const frontMatter = {
  title: "Home",
}

const IndexPage = (): JSX.Element => (
  <Layout frontMatter={frontMatter}>
    <div className="relative pt-4">
      <main className="lg:relative">
        <div className="mx-auto max-w-7xl w-full pt-16 pb-10 text-center lg:py-48 lg:text-left">
          <div className="px-4 lg:w-1/2 sm:px-8 xl:pr-16">
            <h1 className="text-4xl tracking-tight font-extrabold text-gray-900 sm:text-5xl md:text-6xl lg:text-5xl xl:text-6xl">
              <span className="block xl:inline">Hello! I&apos;m</span>{" "}
              <span className="block text-indigo-600 xl:inline">Sondre Nilsen</span>
            </h1>
            <p className="mt-3 max-w-md mx-auto text-lg text-gray-500 sm:text-xl md:mt-5 md:max-w-3xl">
              I am a full time nerd with a passion for programming languages, mechanical keyboards, hoarding
              sideprojects and occasionally creating useful software.
            </p>
          </div>
        </div>
        <div className="relative w-full h-64 sm:h-72 md:h-96 lg:absolute lg:inset-y-0 lg:right-12 lg:w-1/2 lg:h-full">
          <img
            className="absolute inset-0 w-full h-full object-cover lg:rounded-full"
            src="/images/pixel_me.png"
            alt="Pixelized drawn avatar of Sondre Nilsen"
          />
        </div>
      </main>
    </div>
  </Layout>
)

export default IndexPage
