import React from "react";
import Layout from "../components/common/layout";
import SEO from "../components/common/seo";

const Blog: React.FC = () => {
  return (
    <Layout>
      <SEO title="Blog posts" />
      <h1>Blog posts</h1>
    </Layout>
  );
};

export default Blog;
