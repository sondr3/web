import React from "react";
import styled from "styled-components";
import SEO from "./seo";
import Footer from "./footer";
import GlobalStyle from "../styles/global";

import Header from "./header";

interface Props {
  children: React.ReactNode;
}

const Container = styled.div`
  display: grid;
  grid-gap: 20px;
  grid-template-columns: repeat(12, [col-start] 1fr);
  margin: 0 auto;
  max-width: 1024px;
  min-height: 100vh;

  > * {
    grid-column: col-start / span 12;
  }
`;

export default function Layout({ children }: Props) {
  return (
    <React.StrictMode>
      <GlobalStyle />
      <Container>
        <SEO />
        <Header />
        {children}
        <Footer />
      </Container>
    </React.StrictMode>
  );
}
