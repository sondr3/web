import React from "react";
import styles from "./index.module.css";
import Intro from "../components/intro";
import Header from "../components/header";

export default function Layout() {
  return (
    <React.StrictMode>
      <Header />
      <div className={styles.wrapper}>
        <Intro />
      </div>
    </React.StrictMode>
  );
}
