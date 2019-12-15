import React from "react";
import { Project } from "../../../content/projects";

const ProjectCard: React.FC<Project> = ({ name, description, featured, technologies, github }) => {
  return (
    <div className="project">
      <h3>{name}</h3>
      <p>{description}</p>
      {featured ? null : (
        <>
          <div>
            <ul className="technologies">
              {technologies.map(tech => (
                <li className="technology" key={tech}>
                  {tech}
                </li>
              ))}
            </ul>
          </div>
          <a href={github}>See it on GitHub</a>
        </>
      )}
    </div>
  );
};

export default ProjectCard;
