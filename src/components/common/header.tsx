import React from "react";
import { Link } from "gatsby";
import styled from "styled-components";

import c from "../../styles/constants";

const Head = styled.header`
  min-height: 56px;
  position: relative;
`;

const Title = styled(Link)`
  line-height: 56px;
  text-decoration: none;

  &,
  &:visited {
    color: ${c.greyDark};
  }
`;

const Nav = styled.nav`
  float: right;
  line-height: 56px;
`;

const NavLink = styled(Link)`
  color: ${c.textColor};
  line-height: 1.5;
  text-decoration: none;
  text-transform: uppercase;

  &:not(:last-child) {
    margin-right: ${c.spacingUnit}px;
  }

  &:hover,
  &:focus {
    text-decoration: underline;
  }

  @media screen and (max-width: ${c.smallWidth}) {
    margin-left: ${c.spacingUnit}px;
    padding: ${c.spacingUnit}px 0;
    &:not(:last-child) {
      margin-right: 0;
    }
  }
`;

const Header: React.FC = () => {
  return (
    <Head>
      <Title to="/">Eons</Title>
      <Nav>
        <NavLink to="/about/">About</NavLink>
        <NavLink to="/projects/">Projects</NavLink>
        <NavLink to="/blog/">Blog</NavLink>
      </Nav>
    </Head>
  );
};

export default Header;
