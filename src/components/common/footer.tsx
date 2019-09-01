import React from "react";
import styled from "styled-components";
import c from "../../styles/constants";

const Wrapper = styled.footer`
  grid-row-end: 3;
  grid-row-start: 2;
  padding: ${c.spacingUnit}px 0;
  text-align: center;
`;

const Footer: React.FC = () => {
  return (
    <Wrapper>
      <p>
        License <a href="https://creativecommons.org/licenses/by-sa/4.0/">CC BY-SA 4.0</a>
        &mdash;
        <a href="https://www.eons.io">Sondre Nilsen</a>
      </p>
    </Wrapper>
  );
};

export default Footer;
