import React from "react";
import styled from "styled-components";

interface Props {
  children: React.ReactNode;
}

const Wrap = styled.div`
  padding: 1rem;

  @media (min-width: 500px) {
    grid-column: col-start 4 / span 6;
    padding: 0;
  }
`;

export default function Wrapper({ children }: Props) {
  return <Wrap>{children}</Wrap>;
}
