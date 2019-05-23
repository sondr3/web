import React from "react";
import { SEO } from "base";
import styles from "./index.module.css";
import Intro from "../components/intro";

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
