import React from "react";
import SEO from "../components/seo";
import Intro from "../components/intro";

import styles from "./index.module.css";

export default function Layout() {
  return (
    <React.StrictMode>
      <SEO />
      <div className={styles.wrapper}>
        <Intro />
      </div>
    </React.StrictMode>
  );
}
