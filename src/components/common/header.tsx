import React from "react";
import { Link } from "gatsby";
import styled from "styled-components";

import logo from "../../../assets/logo.svg";

const Head = styled.header`
  grid-column: col-start / span 12;
  grid-row: 1;
`;

const Nav = styled.nav`
  align-items: center;
  display: grid;
  grid-auto-rows: auto;
  grid-gap: 1rem;
  grid-template-columns: repeat(5, 1fr);
  margin: 0 auto;
  padding-top: 1rem;
  text-align: center;

  @media screen and (max-width: 700px) {
    grid-template-columns: 100%;
  }
`;

const StyledLink = styled(Link)`
  color: hsla(0, 0%, 0%, 0.8);
  text-decoration: none;
  text-transform: uppercase;
`;

const Img = styled.img`
  justify-self: center;
  max-height: 80px;

  @media screen and (max-width: 700px) {
    display: none;
  }
`;

const Header: React.FC = () => {
  return (
    <Head>
      <Nav>
        <StyledLink to="/">Home</StyledLink>
        <StyledLink to="/about/">About</StyledLink>
        <Img src={logo} alt="Logo" />
        <StyledLink to="/projects/">Projects</StyledLink>
        <StyledLink to="/blog/">Blog</StyledLink>
      </Nav>
    </Head>
  );
};

export default Header;
