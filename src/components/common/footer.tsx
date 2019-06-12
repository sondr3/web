import React from "react";
import styled from "styled-components";

const Wrapper = styled.footer`
  display: grid;
  grid-column: col-start / span 12;
  grid-row: 3;
  place-items: center center;
`;

export default function Footer() {
  return (
    <Wrapper>
      <p>
        License{" "}
        <a href="https://creativecommons.org/licenses/by-sa/4.0/">
          CC BY-SA 4.0
        </a>
        &mdash;
        <a href="https://www.eons.io">Sondre Nilsen</a>
      </p>
    </Wrapper>
  );
}
