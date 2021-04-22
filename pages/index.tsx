import Head from "next/head"
import Image from "next/image"

import { Menu } from "../components/menu"

export default function Home(): JSX.Element {
  return (
    <div className="relative bg-gray-50">
      <Menu />
      <Head>
        <title>Create Next App</title>
        <link rel="icon" href="/favicon.ico" />
      </Head>
      <main className="lg:relative">
        <div className="mx-auto max-w-7xl w-full pt-16 pb-20 text-center lg:py-48 lg:text-left">
          <div className="px-4 lg:w-1/2 sm:px-8 xl:pr-16">
            <h1 className="text-4xl tracking-tight font-extrabold text-gray-900 sm:text-5xl md:text-6xl lg:text-5xl xl:text-6xl">
              <span className="block xl:inline">Hi!</span>{" "}
              <span className="block text-indigo-600 xl:inline">I am Sondre</span>
            </h1>
            <p className="mt-3 max-w-md mx-auto text-lg text-gray-500 sm:text-xl md:mt-5 md:max-w-3xl">
              I make things.
            </p>
          </div>
        </div>
        <div className="relative w-full h-64 sm:h-72 md:h-96 lg:absolute lg:inset-y-0 lg:right-0 lg:w-1/2 lg:h-full">
          <Image
            className="absolute inset-0 w-full h-full object-cover"
            src="/images/me.jpg"
            alt="Picture of me"
            layout="responsive"
            width={400}
            height={400}
          />
        </div>
      </main>
    </div>
  )
}
