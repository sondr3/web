import React from "react";
import { Link } from "gatsby";

const Header: React.FC = () => {
  return (
    <header>
      <Link className="title" to="/">
        Eons
      </Link>
      <nav>
        <Link className="nav-link" to="/about/">
          About
        </Link>
        <Link className="nav-link" to="/projects/">
          Projects
        </Link>
        <Link className="nav-link " to="/blog/">
          Blog
        </Link>
      </nav>
    </header>
  );
};

export default Header;
