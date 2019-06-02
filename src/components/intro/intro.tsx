import React from "react";
import { useStaticQuery, graphql } from "gatsby";
import styled from "styled-components";
import Wrapper from "../common/wrapper";

import dev from "../../../assets/developer.svg";

const Img = styled.img`
  width: 100%;
`;

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
      <Img src={dev} alt="Hello! I'm Sondre." />
      <h1>Hello!</h1>
      <h2>I&apos;m Sondre.</h2>
      <p>
        {site.siteMetadata.author.bio}. You can see them on my{" "}
        <a href={site.siteMetadata.social.github}>GitHub</a>.
      </p>
    </Wrapper>
  );
}
