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
  background-color: #6cc0e5;
  border-radius: ${c.spacingUnit / 4}px;
  margin-right: ${c.spacingUnit / 2}px;
  padding: 0 ${c.spacingUnit / 3}px;
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
      <a href={github}>See it on GitHub</a>
    </CardWrapper>
  );
};

export default ProjectCard;
