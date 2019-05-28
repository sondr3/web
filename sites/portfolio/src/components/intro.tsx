import React from "react";
import { useStaticQuery, graphql } from "gatsby";
import ProjectCard from "./project-card";
import projectList from "../projects";

export default function About() {
  const { site } = useStaticQuery(graphql`
    query {
      site {
        siteMetadata {
          author {
            bio
          }
          social {
            github
          }
        }
      }
    }
  `);

  return (
    <>
      <h1>Hello!</h1>
      <h2>I&apos;m Sondre.</h2>
      <p>
        {site.siteMetadata.author.bio}. You can see them on my{" "}
        <a href={site.siteMetadata.social.github}>GitHub</a>.
      </p>
      {projectList.map(project => (
        <ProjectCard key={project.id} {...project} />
      ))}
    </>
  );
}
