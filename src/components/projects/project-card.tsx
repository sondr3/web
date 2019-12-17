import React from "react";
import Project from "../../types/project";

const ProjectCard: React.FC<Project> = ({ name, description, technologies, github }) => {
  return (
    <div className="project">
      <h3>{name}</h3>
      <p>{description}</p>
      {technologies && (
        <div>
          <ul className="technologies">
            {technologies.map(tech => (
              <li className="technology" key={tech}>
                {tech}
              </li>
            ))}
          </ul>
        </div>
      )}
      {github && <a href={github}>See it on GitHub</a>}
    </div>
  );
};

export default ProjectCard;
