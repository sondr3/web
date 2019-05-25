// eslint-disable-next-line @typescript-eslint/no-var-requires
const SizePlugin = require("size-plugin");

exports.onCreateWebpackConfig = ({ stage, actions }, options) => {
  if (process.env.NODE_ENV === "production" && stage === "build-javascript") {
    actions.setWebpackConfig({
      plugins: [new SizePlugin(options)]
    });
  }
};
