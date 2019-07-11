import React from "react";
import { useStaticQuery, graphql } from "gatsby";
import MDXRenderer from "gatsby-mdx/mdx-renderer";
import Layout from "../components/layout";
import Wrapper from "../components/wrapper";
import SEO from "../components/seo";

export default function PageTemplate() {
  const { mdx: page } = useStaticQuery(graphql`
    query PageQuery($id: String) {
      mdx(id: { eq: $id }) {
        id
        frontmatter {
          title
          description
        }
        code {
          body
        }
      }
    }
  `);

  return (
    <Layout>
      <SEO
        title={page.frontmatter.title}
        description={page.frontmatter.description}
      />
      <Wrapper>
        <MDXRenderer>{page.code.body}</MDXRenderer>
      </Wrapper>
    </Layout>
  );
}