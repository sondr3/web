import React from "react";
import PropTypes from "prop-types";

export default function ProjectCard({
  name,
  description,
  technologies,
  github
}) {
  return (
    <>
      <h2>{name}</h2>
      <p>{description}</p>
      <ul>
        {technologies.map(tech => {
          <li>{tech}</li>;
        })}
      </ul>
      <a href={github}>See it on github</a>
    </>
  );
}

ProjectCard.propTypes = {
  name: PropTypes.string.isRequired,
  description: PropTypes.string.isRequired,
  technologies: PropTypes.arrayOf(PropTypes.string).isRequired,
  github: PropTypes.string.isRequired
};
