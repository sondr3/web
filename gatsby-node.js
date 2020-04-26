/* eslint-disable @typescript-eslint/no-require-imports */
/* eslint-disable @typescript-eslint/no-var-requires */
/* eslint-disable no-console */
const kebabCase = require("lodash/kebabCase");
const { createFilePath } = require("gatsby-source-filesystem");

exports.onCreateNode = ({ node, getNode, actions }) => {
  const { createNodeField } = actions;

  if (node.internal.type !== `Mdx`) return;

  const parent = getNode(node.parent);
  createNodeField({
    name: `name`,
    node,
    value: parent.sourceInstanceName,
  });

  const value = createFilePath({ node, getNode });
  createNodeField({
    name: "slug",
    node,
    value: `/${kebabCase(value)}/`,
  });
};

exports.createPages = async ({ graphql, actions }) => {
  const { createPage } = actions;
  const content = await graphql(
    `
      fragment Content on Mdx {
        id
        fields {
          slug
        }
      }

      query {
        pages: allMdx(filter: { fields: { name: { eq: "pages" } } }) {
          nodes {
            ...Content
          }
        }
        projects: allMdx(filter: { fields: { name: { eq: "projects" } } }) {
          nodes {
            ...Content
          }
        }
      }
    `,
  );

  if (content.errors) throw content.errors;

  // [INFO]: Create pages
  content.data.pages.nodes.forEach((node) => {
    createPage({
      path: node.fields.slug,
      component: require.resolve("./src/templates/page.tsx"),
      context: { id: node.id },
    });
  });

  // [INFO]: Create projecte pages
  content.data.projects.nodes.forEach((node) => {
    createPage({
      path: `/project${node.fields.slug}`,
      component: require.resolve("./src/templates/page.tsx"),
      context: { id: node.id },
    });
  });
};
