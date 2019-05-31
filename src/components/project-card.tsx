import React from "react";
import styled from "styled-components";
import { Project } from "../projects";

export default function ProjectCard({
  name,
  description,
  technologies,
  github
}: Project) {
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
