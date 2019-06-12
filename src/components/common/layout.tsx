import React from "react";
import styled from "styled-components";
import SEO from "./seo";
import Footer from "./footer";
import GlobalStyle from "../../styles/global";

import Header from "./header";
import Wrapper from "./wrapper";

interface Props {
  children: React.ReactNode;
}

const Container = styled.main`
  display: grid;
  grid-gap: 20px;
  grid-template-columns: repeat(12, [col-start] 1fr);
  grid-template-rows: 100px auto 100px;
  margin: 0 auto;
  max-width: 1024px;
  min-height: 100vh;
`;

export default function Layout({ children }: Props) {
  return (
    <React.StrictMode>
      <GlobalStyle />
      <SEO />
      <Container>
        <Header />
        <Wrapper>{children}</Wrapper>
        <Footer />
      </Container>
    </React.StrictMode>
  );
}
