import React from "react";
import SEO from "./seo";
import Footer from "./footer";

import styles from "./layout.module.css";
import Header from "./header";

interface Props {
  children: React.ReactNode;
}

export default function Layout({ children }: Props) {
  return (
    <React.StrictMode>
      <div className={styles.container}>
        <SEO />
        <Header />
        {children}
        <Footer />
      </div>
    </React.StrictMode>
  );
}
