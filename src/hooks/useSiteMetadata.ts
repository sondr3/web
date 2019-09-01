import { useStaticQuery, graphql } from "gatsby";

interface SiteMetadata {
  siteUrl: string;
  title: string;
  titleTemplate: string;
  description: string;
  author: {
    name: string;
  };
}

const useSiteMetadata = (): SiteMetadata => {
  const { site } = useStaticQuery(graphql`
    query SiteMetadata {
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
