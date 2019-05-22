import React from "react";
import { SEO } from "base/components";
import { useStaticQuery, graphql } from "gatsby";

export default function About() {
  const { site } = useStaticQuery(graphql`
    query {
      site {
        siteMetadata {
          author {
            bio
          }
          social {
            github
          }
        }
      }
    }
  `);

  return (
    <>
      <SEO />
      <h1>Hello!</h1>
      <h2>I&apos;m Sondre.</h2>
      <p>
        {site.siteMetadata.author.bio}. You can see them on my{" "}
        <a href={site.siteMetadata.social.github}>GitHub</a>.
      </p>
    </>
  );
}
