import React from "react";
import { useStaticQuery, graphql } from "gatsby";
import Layout from "../components/common/layout";
import SEO from "../components/common/seo";
import ProjectCard from "../components/projects/project-card";

export interface Project {
  id: string;
  name: string;
  description: string;
  technologies: [string];
  github: string;
}

const Projects: React.FC = () => {
  const { personal, contributor } = useStaticQuery(graphql`
    {
      personal: allProjectsJson(filter: { contributor: { eq: false } }) {
        nodes {
          name
          description
          id
          technologies
          github
        }
      }
      contributor: allProjectsJson(filter: { contributor: { eq: true } }) {
        nodes {
          name
          description
          id
          technologies
          github
        }
      }
    }
  `);

  return (
    <Layout>
      <SEO title="Projects" />
      <h1>Projects</h1>
      <h2>Personal projects</h2>
      {personal.nodes.map((project: Project) => (
        <ProjectCard
          key={project.id}
          name={project.name}
          description={project.description}
          technologies={project.technologies}
          github={project.github}
        />
      ))}
      <h2>Projects I contribute to</h2>
      {contributor.nodes.map((project: Project) => (
        <ProjectCard
          key={project.id}
          name={project.name}
          description={project.description}
          technologies={project.technologies}
          github={project.github}
        />
      ))}
    </Layout>
  );
};

export default Projects;
