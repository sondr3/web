import { useStaticQuery, graphql } from "gatsby";

interface AuthorMetadata {
  author: {
    name: string;
    intro: string;
    bio: string;
  };
  social: {
    github: string;
  };
}

const useAuthorMetadata = (): AuthorMetadata => {
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
};

export default useAuthorMetadata;
