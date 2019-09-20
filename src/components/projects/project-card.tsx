import React from "react";
import styled from "styled-components";
import { Project } from "../../../content/projects";

import c from "../../styles/constants";

const CardWrapper = styled.div`
  padding-bottom: ${c.size.spacingUnit}px;
`;

const StyledUl = styled.ul`
  display: inline-flex;
  flex-wrap: wrap;
  justify-content: flex-start;
  list-style: none;
  margin: 0 0;
`;

const Technology = styled.li`
  border: 1px solid ${c.color.blue};
  border-radius: ${c.size.spacingUnit / 4}px;
  color: ${c.color.blue};
  margin-right: ${c.size.spacingUnit / 2}px;
  padding: 0 ${c.size.spacingUnit / 3}px;
`;

const Title = styled.h3`
  margin-bottom: ${c.size.spacingUnit / 4}px;
`;

const ProjectCard: React.FC<Project> = ({ name, description, featured, technologies, github }) => {
  return (
    <CardWrapper>
      <Title>{name}</Title>
      <p>{description}</p>
      {featured ? null : (
        <>
          <div>
            <StyledUl>
              {technologies.map(tech => (
                <Technology key={tech}>{tech}</Technology>
              ))}
            </StyledUl>
          </div>
          <a href={github}>See it on GitHub</a>
        </>
      )}
    </CardWrapper>
  );
};

export default ProjectCard;
