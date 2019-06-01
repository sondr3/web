import React from "react";

import styles from "./footer.module.css";

export default function Footer() {
  return (
    <footer className={styles.footer}>
      <p>
        License{" "}
        <a href="https://creativecommons.org/licenses/by-sa/4.0/">
          CC BY-SA 4.0
        </a>
        &mdash;
        <a href="https://www.eons.io">Sondre Nilsen</a>
      </p>
    </footer>
  );
}
