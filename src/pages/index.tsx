import React from "react";
import Layout from "../components/common/layout";
import Intro from "../components/intro/intro";
import About from "../components/intro/about";

export default function Index() {
  return (
    <Layout>
      <Intro />
      <About />
    </Layout>
  );
}
