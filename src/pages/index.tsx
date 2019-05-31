import React from "react";
import styled from "styled-components";
import SEO from "../components/seo";
import Intro from "../components/intro";

const Wrapper = styled.div`
  margin: 3rem auto;
  max-width: 600px;
`;

export default function Layout() {
  return (
    <React.StrictMode>
      <SEO />
      <Wrapper>
        <Intro />
      </Wrapper>
    </React.StrictMode>
  );
}
