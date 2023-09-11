import { Frontmatter } from "../content.ts";
import { Base } from "./base.tsx";

interface Props {
  fm: Frontmatter;
  content: string;
}

const renderContent = (fm: Frontmatter, content: string) => {
  return {
    __html: `<h1>${fm.title}</h1>\n${content} `,
  };
};

export const Page = ({ fm, content }: Props) => {
  return (
    <Base fm={fm}>
      <main class="main">
        <article dangerouslySetInnerHTML={renderContent(fm, content)} />
      </main>
    </Base>
  );
};
