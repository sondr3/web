import React from "react";
import SEO from "./seo";

import styles from "./layout.module.css";

interface Props {
  children: React.ReactNode;
}

export default function Layout({ children }: Props) {
  return (
    <React.StrictMode>
      <SEO />
      <div className={styles.main}>{children}</div>
    </React.StrictMode>
  );
}
