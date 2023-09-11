import { Frontmatter } from "../content.ts";
import { Base } from "./base.tsx";

interface Props {
  fm: Frontmatter;
}

export const Index = ({ fm }: Props) => {
  return (
    <Base fm={fm}>
      <main class="main">
        <section>
          <h1 id="hello">
            <span>Hello! I'm</span>
            <span class="blue">Sondre</span>
          </h1>
          <p class="me prose">
            I am a full time nerd with a passion for programming languages, mechanical keyboards, hoarding side-projects
            and occasionally creating useful software.
          </p>
        </section>
      </main>
    </Base>
  );
};
