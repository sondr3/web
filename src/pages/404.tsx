import React from "react";
import Layout from "../components/common/layout";
import SEO from "../components/common/seo";

export default function Index() {
  return (
    <Layout>
      <SEO title="404" />
      <h1>404: Page not found</h1>
      <p>Oh no :(</p>
    </Layout>
  );
}
