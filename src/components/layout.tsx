import React from "react";
import styled from "styled-components";
import SEO from "./seo";

const Main = styled.main`
  margin: 0 auto;
`;

interface Props {
  children: React.ReactNode;
}

export default function Layout({ children }: Props) {
  return (
    <React.StrictMode>
      <SEO />
      <Main>{children}</Main>
    </React.StrictMode>
  );
}
