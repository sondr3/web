import React from "react";
import PropTypes from "prop-types";
import SEO from "./seo";
import Footer from "./footer";

import styles from "./layout.module.css";
import Header from "./header";

export default function Layout({ children }) {
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

Layout.propTypes = {
  children: PropTypes.element.isRequired
};
