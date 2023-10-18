import { Frontmatter } from "../content.ts";
import { Base, TemplateProps } from "./base.tsx";

const renderContent = (fm: Frontmatter, content: string) => {
  return {
    __html: `<h1>${fm.title}</h1>\n${content} `,
  };
};

export const Page = ({ content, context }: TemplateProps) => {
  return (
    <Base content={content} context={context}>
      <main class="main">
        <article dangerouslySetInnerHTML={renderContent(content.frontmatter, content.rendered)} />
      </main>
    </Base>
  );
};
