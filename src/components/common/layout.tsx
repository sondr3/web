import React from "react";
import styled from "styled-components";
import SEO from "./seo";
import Footer from "./footer";
import Header from "./header";

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

const Wrapper = styled.section`
  padding: 1rem;

  @media (min-width: 500px) {
    grid-column: col-start 4 / span 6;
    padding: 0;
  }
`;

const Layout: React.FC<Props> = ({ children }) => {
  return (
    <React.StrictMode>
      <SEO />
      <Container>
        <Header />
        <Wrapper>{children}</Wrapper>
        <Footer />
      </Container>
    </React.StrictMode>
  );
};

export default Layout;
