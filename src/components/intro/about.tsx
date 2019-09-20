import React from "react";
import { MDXRenderer } from "gatsby-plugin-mdx";
import { useStaticQuery, graphql } from "gatsby";

const About: React.FC = () => {
  const { mdx: about } = useStaticQuery(graphql`
    query About {
      mdx(frontmatter: { title: { eq: "About" } }) {
        frontmatter {
          title
          description
        }
        body
      }
    }
  `);

  return (
    <section>
      <h2>{about.frontmatter.description}</h2>
      <MDXRenderer>{about.body}</MDXRenderer>
    </section>
  );
};

export default About;
