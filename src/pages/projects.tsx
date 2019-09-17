import React from "react";
import Layout from "../components/common/layout";
import SEO from "../components/common/seo";
import ProjectCard from "../components/projects/project-card";
import projects from "../../content/projects";

const Projects: React.FC = () => {
  return (
    <Layout>
      <SEO title="Projects" />
      <h1>Projects</h1>
      <h2>Personal projects</h2>
      {projects.personal.map(project => (
        <ProjectCard key={project.id} {...project} />
      ))}
      <h2>Projects I contribute to</h2>
      {projects.contributor.map(project => (
        <ProjectCard key={project.id} {...project} />
      ))}
    </Layout>
  );
};

export default Projects;
