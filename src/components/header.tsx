import React from "react";
import { Helmet } from "react-helmet";

export default function Header() {
  return (
    <>
      <Helmet>
        <meta charSet="utf-8" />
        <title>Eons.io</title>
        <link rel="canonical" href="https://www.eons.io" />
      </Helmet>
    </>
  );
}
