import React from "react";
import { useStaticQuery, graphql } from "gatsby";
import ProjectCard from "../projects/project-card";
import Project from "../../types/project";

const FeaturedProjects: React.FC = () => {
  const { projects } = useStaticQuery(graphql`
    {
      projects: allProjectsJson(filter: { featured: { eq: true } }) {
        nodes {
          id
          name
          description
        }
      }
    }
  `);

  return (
    <section>
      <h2>Featured projects</h2>
      {projects.nodes.map((project: Project) => (
        <ProjectCard name={project.name} description={project.description} key={project.id} />
      ))}
    </section>
  );
};

export default FeaturedProjects;
