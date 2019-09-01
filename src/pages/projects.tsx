import React from "react";
import Layout from "../components/common/layout";
import SEO from "../components/common/seo";
import ProjectCard from "../components/intro/project-card";
import projects from "../../content/projects";

const Projects: React.FC = () => {
  return (
    <Layout>
      <SEO title="Projects" />
      <h1>Projects</h1>
      {projects.map(project => (
        <ProjectCard key={project.id} {...project} />
      ))}
    </Layout>
  );
};

export default Projects;
