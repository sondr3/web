import React from "react";
import Layout from "../components/common/layout";
import Intro from "../components/intro/intro";
import About from "../components/intro/about";
import FeaturedProjects from "../components/intro/featured-projects";

const Index: React.FC = () => {
  return (
    <Layout>
      <Intro />
      <About />
      <FeaturedProjects />
    </Layout>
  );
};

export default Index;
