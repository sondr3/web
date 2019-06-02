import { useStaticQuery, graphql } from "gatsby";

export default function useSiteMetadata() {
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
}
