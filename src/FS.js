const fs = require("fs").promises;

exports._mkdir = (dir) => {
  return () => {
    return fs.mkdir(dir, { recursive: true });
  };
};
