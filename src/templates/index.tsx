import { Base, TemplateProps } from "./base.tsx";

export const Index = ({ content, site }: TemplateProps) => {
  return (
    <Base content={content} site={site}>
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
