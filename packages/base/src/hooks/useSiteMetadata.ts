import { useStaticQuery, graphql } from "gatsby";

const useSiteMetadata = () => {
  const { site } = useStaticQuery(graphql`
    query {
      site {
        siteMetadata {
          siteUrl
          title
          titleTemplate
          description
          author {
            name
          }
        }
      }
    }
  `);

  return site.siteMetadata;
};

export default useSiteMetadata;
