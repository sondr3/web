import React from "react";
import { useStaticQuery, graphql } from "gatsby";
import Helmet from "react-helmet";

interface Props {
  description?: string;
  title: string;
}

const SEO = ({ description, title }: Props) => {
  const { site } = useStaticQuery(graphql`
    query {
      site {
        siteMetadata {
          siteUrl
          title
          description
          author {
            name
          }
        }
      }
    }
  `);

  title = title || site.siteMetadata.title;
  description = description || site.siteMetadata.description;

  return (
    <Helmet
      htmlAttributes={{ lang: "en" }}
      title={title}
      titleTemplate={`%s | ${site.siteMetadata.title}`}
      meta={[
        {
          name: `description`,
          content: description
        },
        {
          property: `og:title`,
          content: title
        },
        {
          property: `og:description`,
          content: description
        },
        {
          property: `og:type`,
          content: `website`
        },
        {
          name: `twitter:card`,
          content: `summary`
        },
        {
          name: `twitter:creator`,
          content: site.siteMetadata.author.name
        },
        {
          name: `twitter:title`,
          content: title
        },
        {
          name: `twitter:description`,
          content: description
        }
      ]}
    />
  );
};

export default SEO;
