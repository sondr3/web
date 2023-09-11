import { Frontmatter } from "../content.ts";
import { Base, BaseProps } from "./base.tsx";

interface Props extends BaseProps {
  fm: Frontmatter;
}

export const Index = ({ fm, assets }: Props) => {
  return (
    <Base fm={fm} assets={assets}>
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
