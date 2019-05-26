import React from "react";
import SEO from "./seo";

import styles from "./layout.module.scss";

interface Props {
  children: React.ReactNode;
}

export default function Layout({ children }: Props) {
  return (
    <React.StrictMode>
      <SEO />
      <main className={styles.main}>{children}</main>
    </React.StrictMode>
  );
}
