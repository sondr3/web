import { Frontmatter } from "../content.ts";
import { Base, BaseProps } from "./base.tsx";

interface Props extends BaseProps {
  fm: Frontmatter;
  content: string;
}

const renderContent = (fm: Frontmatter, content: string) => {
  return {
    __html: `<h1>${fm.title}</h1>\n${content} `,
  };
};

export const Page = ({ fm, content, assets }: Props) => {
  return (
    <Base fm={fm} assets={assets}>
      <main class="main">
        <article dangerouslySetInnerHTML={renderContent(fm, content)} />
      </main>
    </Base>
  );
};
