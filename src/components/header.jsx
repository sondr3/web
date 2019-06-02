import React from "react";
import { Link } from "gatsby";

import styles from "./header.module.css";

export default function Header() {
  return (
    <header>
      <nav className={styles.nav}>
        <div className={styles.logo}>
          <Link to="/" className={styles.link}>
            Eons
          </Link>
        </div>
        <ul className={styles.navLinks}>
          <li>
            <Link to="/about/" className={styles.link}>
              About
            </Link>
          </li>
        </ul>
      </nav>
    </header>
  );
}
