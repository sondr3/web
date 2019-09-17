import React from "react";
import styled from "styled-components";
import { Project } from "../../../content/projects";

import c from "../../styles/constants";

const CardWrapper = styled.div`
  padding-bottom: ${c.spacingUnit}px;
`;

const StyledUl = styled.ul`
  display: inline-flex;
  flex-wrap: wrap;
  justify-content: flex-start;
  list-style: none;
`;

const StyledLi = styled.li`
  margin-right: ${c.spacingUnit / 2}px;
`;

const ProjectCard: React.FC<Project> = ({ name, description, technologies, github }) => {
  return (
    <CardWrapper>
      <h3>{name}</h3>
      <p>{description}</p>
      <div>
        <StyledUl>
          {technologies.map(tech => (
            <StyledLi key={tech}>{tech}</StyledLi>
          ))}
        </StyledUl>
      </div>
      <a href={github}>See it on github</a>
    </CardWrapper>
  );
};

export default ProjectCard;
