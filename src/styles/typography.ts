import Typography from "typography";

const typography = new Typography({
  baseFontSize: "18px",
  baseLineHeight: 1.45,
  headerFontFamily: [
    "Cambo",
    "Helvetica Neue",
    "Segoe UI",
    "Helvetica",
    "Arial",
    "sans-serif"
  ],
  bodyFontFamily: ["Bitter", "Georgia", "serif"],
  overrideStyles: () => ({
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
