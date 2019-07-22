import React from "react";
import { Project } from "../../../content/projects";

const ProjectCard: React.FC<Project> = ({ name, description, technologies, github }) => {
  return (
    <>
      <h2>{name}</h2>
      <p>{description}</p>
      <ul>
        {technologies.map(tech => (
          <li key={tech}>{tech}</li>
        ))}
      </ul>
      <a href={github}>See it on github</a>
    </>
  );
};

export default ProjectCard;
