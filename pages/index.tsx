import { Layout } from "@/components"
import styles from "@/styles/landing.module.scss"

const IndexPage = (): JSX.Element => (
  <Layout frontMatter={{ title: "Home" }}>
    <section>
      <h1 className={styles.hello}>
        <span>Hello! I&apos;m</span> <span className={styles.blue}>Sondre</span>
      </h1>
      <p className={styles.me}>
        I am a full time nerd with a passion for programming languages, mechanical keyboards, hoarding sideprojects and
        occasionally creating useful software.
      </p>
    </section>
  </Layout>
)

export default IndexPage
