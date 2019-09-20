import React from "react";

import projs from "../../../content/projects";
import ProjectCard from "../projects/project-card";

const FeaturedProjects: React.FC = () => {
  const projects = projs.personal.filter(p => p.featured);

  return (
    <section>
      <h2>Featured projects</h2>
      {projects.map(project => (
        <ProjectCard
          key={project.id}
          id={project.id}
          featured={project.featured}
          name={project.name}
          description={project.description}
          technologies={project.technologies}
          github={project.github}
        />
      ))}
    </section>
  );
};

export default FeaturedProjects;
