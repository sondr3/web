import Head from "next/head"
import Link from "next/link"
import { ReactNode } from "react"

import { FrontMatter } from "@/lib/mdx"
import styles from "@/styles/layout.module.scss"

interface Props {
  children?: ReactNode
  frontMatter: FrontMatter
}

export const Layout = ({ children, frontMatter }: Props): JSX.Element => {
  return (
    <>
      <Head>
        <title>
          {frontMatter.title} {"=>"} Eons :: IO ()
        </title>
      </Head>
      <header className={styles.header}>
        <h1 className={styles.title}>
          <Link href="/">EONS :: IO ()</Link>
        </h1>
        <nav>
          <ul className={styles.nav}>
            {/* <li><a href="/">projects</a></li> */}
            {/* <li><a href="/">articles</a></li> */}
            <li className={styles.link}>
              <Link href="/about/">about</Link>
            </li>
          </ul>
        </nav>
      </header>
      <main className={styles.main}>{children}</main>
      <footer className={styles.footer}>
        <p>
          Content (C) Sondre Nilsen, licensed under{" "}
          <a href="https://creativecommons.org/licenses/by-sa/4.0/">CC BY-SA 4.0</a>
        </p>
      </footer>
    </>
  )
}
