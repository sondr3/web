import React from "react";
import Layout from "../components/common/layout";
import Intro from "../components/intro/intro";
import About from "../components/intro/about";

const Index: React.FC = () => {
  return (
    <Layout>
      <Intro />
      <About />
    </Layout>
  );
};

export default Index;
