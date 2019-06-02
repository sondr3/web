import { useStaticQuery, graphql } from "gatsby";

export default function useAuthorMetadata() {
  const { site } = useStaticQuery(graphql`
    query AuthorMetadata {
      site {
        siteMetadata {
          author {
            name
            intro
            bio
          }
          social {
            github
          }
        }
      }
    }
  `);

  return site.siteMetadata;
}
