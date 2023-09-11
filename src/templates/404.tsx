import { Frontmatter } from "../content.ts";
import { Base } from "./base.tsx";

interface Props {
  fm: Frontmatter;
}

export const FourOhFour = ({ fm }: Props) => {
  return (
    <Base fm={fm}>
      <main class="main">
        <section class="four-oh-four">
          <h1>Page not found</h1>
          <p class="prose">What you're looking for does not exist :(</p>
          <div>
            <a href="/" class="blue">
              Go back home<span aria-hidden="true">&rarr;</span>
            </a>
          </div>
        </section>
      </main>
    </Base>
  );
};
