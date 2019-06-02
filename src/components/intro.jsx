import React from "react";
import { useStaticQuery, graphql } from "gatsby";
import Wrapper from "./wrapper";

import dev from "../../assets/developer.svg";
import styles from "./intro.module.css";

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
    <Wrapper>
      <img src={dev} alt="Hello! I'm Sondre." className={styles.img} />
      <h1>Hello!</h1>
      <h2>I&apos;m Sondre.</h2>
      <p>
        {site.siteMetadata.author.bio}. You can see them on my{" "}
        <a href={site.siteMetadata.social.github}>GitHub</a>.
      </p>
    </Wrapper>
  );
}
