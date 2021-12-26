const config = {
  extensions: [".svelte.md", ".md", ".svx"],
  smartypants: {
    dashes: "oldschool",
  },
  layout: {
    page: "./src/lib/layouts/page.svelte",
  },
  remarkPlugins: [],
  rehypePlugins: [],
};

export default config;
