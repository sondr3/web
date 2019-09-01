import React from "react";
import styled from "styled-components";
import SEO from "./seo";
import Footer from "./footer";
import Header from "./header";
import c from "../../styles/constants";

interface Props {
  children: React.ReactNode;
}

const Container = styled.div`
  margin: 0 auto;
  max-width: calc(${c.contentWidth}px - ${c.spacingUnit * 2}px);
  padding: 0 ${c.spacingUnit}px;

  @media screen and (max-width: ${c.largeWidth}px) {
    max-width: calc(${c.contentWidth}px - ${c.spacingUnit}px);
    padding: 0 ${c.spacingUnit / 2}px;
  }
`;

const Layout: React.FC<Props> = ({ children }) => {
  return (
    <React.StrictMode>
      <SEO />
      <Container>
        <Header />
        {children}
        <Footer />
      </Container>
    </React.StrictMode>
  );
};

export default Layout;
