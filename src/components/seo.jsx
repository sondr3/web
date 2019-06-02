import React from "react";
import Helmet from "react-helmet";
import PropTypes from "prop-types";
import useSiteMetadata from "../hooks/useSiteMetadata";

const SEO = ({ description, title }) => {
  const meta = useSiteMetadata();

  title = title || meta.title;
  description = description || meta.description;

  return (
    <Helmet
      htmlAttributes={{ lang: "en" }}
      title={title}
      titleTemplate={meta.titleTemplate}
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
          content: meta.author.name
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

SEO.propTypes = {
  description: PropTypes.string,
  title: PropTypes.string
};

SEO.defaultProps = {
  description: null,
  title: null
};

export default SEO;
