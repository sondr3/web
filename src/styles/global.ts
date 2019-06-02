import { createGlobalStyle } from "styled-components";

const GlobalStyle = createGlobalStyle`
body {
  font-family: "Bitter", serif;
  font-feature-settings: "kern", "liga", "clig", "calt";
  font-kerning: normal;
  font-weight: 400;
  word-wrap: break-word;
}

h1,
h2,
h3,
h4,
h5,
h6 {
  font-family: "Cambo", sans-serif;
}

p {
  hyphens: auto;
  text-align: justify;
  text-justify: inter-word;
}
`;

export default GlobalStyle;
