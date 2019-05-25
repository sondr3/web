import React from "react";
import "../styles/global.css";
import SEO from "./seo";

interface Props {
  children: React.ReactNode;
}

export default function Layout({ children }: Props) {
  return (
    <React.StrictMode>
      <SEO />
      <main>{children}</main>
    </React.StrictMode>
  );
}
