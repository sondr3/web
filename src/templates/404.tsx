import { Base, TemplateProps } from "./base.tsx";

export const FourOhFour = ({ content, context }: TemplateProps) => {
  return (
    <Base content={content} context={context}>
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
