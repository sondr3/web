import { html } from "../../html";
import { layout } from "../layouts";

const landingPage = html`
  <div>
    <h1>Hello!</h1>
  </div>
`;

export const landing = (): string => {
  return layout("EONS", landingPage);
};
