import React from "react";
import PropTypes from "prop-types";

import styles from "./wrapper.module.css";

export default function Wrapper({ children }) {
  return <div className={styles.wrapper}>{children}</div>;
}

Wrapper.propTypes = {
  children: PropTypes.element.isRequired
};
