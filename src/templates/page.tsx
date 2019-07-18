import React from "react";
import { graphql } from "gatsby";
import MDXRenderer from "gatsby-plugin-mdx/mdx-renderer";
import SEO from "../components/common/seo";
import Layout from "../components/common/layout";

interface PageQueryData {
  data: {
    mdx: {
      id: number;
      frontmatter: {
        title: string;
        description: string;
      };
      body: string;
    };
  };
}

export default function PageTemplate({ data: { mdx } }: PageQueryData) {
  return (
    <Layout>
      <SEO
        title={mdx.frontmatter.title}
        description={mdx.frontmatter.description}
      />
      <MDXRenderer>{mdx.body}</MDXRenderer>
    </Layout>
  );
}

export const pageQuery = graphql`
  query PageQuery($id: String) {
    mdx(id: { eq: $id }) {
      id
      frontmatter {
        title
        description
      }
      body
    }
  }
`;
