import React from "react";
import { Link } from "gatsby";
import styled from "styled-components";

const Nav = styled.nav`
  display: grid;
  grid-gap: 1rem;
  grid-template-columns: 1fr 1fr;
  padding-top: 1rem;
`;

const Logo = styled.div`
  align-self: center;
  grid-column: 1;
  justify-self: center;
`;

const NavLink = styled.ul`
  display: flex;
  grid-column: 2;
  justify-content: space-around;
`;

const StyledLink = styled(Link)`
  color: black;
  font-size: 18px;
  text-decoration: none;
  text-transform: uppercase;
`;

export default function Header() {
  return (
    <header>
      <Nav>
        <Logo>
          <StyledLink to="/">Eons</StyledLink>
        </Logo>
        <NavLink>
          <li>
            <StyledLink to="/about/">About</StyledLink>
          </li>
        </NavLink>
      </Nav>
    </header>
  );
}
