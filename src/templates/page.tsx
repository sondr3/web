import { Frontmatter } from "../content.ts";
import { Base, TemplateProps } from "./base.tsx";

const renderContent = (fm: Frontmatter, content: string) => {
  return {
    __html: `<h1>${fm.title}</h1>\n${content} `,
  };
};

export const Page = ({ content, site }: TemplateProps) => {
  return (
    <Base content={content} site={site}>
      <main class="main">
        <article className="prose" dangerouslySetInnerHTML={renderContent(content.frontmatter, content.content)} />
      </main>
    </Base>
  );
};
