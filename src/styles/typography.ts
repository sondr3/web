import Typography from "typography";

const typography = new Typography({
  baseFontSize: "18px",
  baseLineHeight: 1.45,
  headerFontFamily: ["Cambo", "Helvetica Neue", "Segoe UI", "Helvetica", "Arial", "sans-serif"],
  bodyFontFamily: ["Bitter", "Georgia", "serif"],
  overrideStyles: (): Record<symbol, string> => ({
    body: {
      backgroundColor: "hsla(0, 0%, 99.2%, 1)"
    },
    p: {
      hypens: "auto",
      textAlign: "justify",
      textJustify: "inter-word"
    }
  })
});

if (process.env.NODE_ENV !== "production") {
  typography.injectStyles();
}

export default typography;
