import React from "react";
import SEO from "./seo";
import Footer from "./footer";
import Header from "./header";

import "../../styles/index.scss";

interface Props {
  children: React.ReactNode;
}
const Layout: React.FC<Props> = ({ children }) => {
  return (
    <React.StrictMode>
      <SEO />
      <div className="container">
        <Header />
        {children}
        <Footer />
      </div>
    </React.StrictMode>
  );
};

export default Layout;
