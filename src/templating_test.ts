import { assertEquals } from "std/assert/mod.ts";
import { format } from "std/datetime/mod.ts";
import { compile, createContext } from "./templating.ts";

Deno.test("templating", () => {
  const template = "<h1>${title}</h1>\n" + "<p>${content}</p>\n" + "<time>${pubDate}</time>";
  const compiled = compile(template);
  const context = createContext({
    title: "Hello, world!",
    content: "This is a test.",
    pubDate: format(new Date(2023, 0, 1), "yyyy-MM-dd"),
  });

  const rendered = compiled(context);
  assertEquals(rendered, `<h1>Hello, world!</h1>\n<p>This is a test.</p>\n<time>2023-01-01</time>`);
});

Deno.test("test nested template", () => {
  const template = "<main>${content}</main>\n";
  const nestedTemplate = "<h1>${title}</h1>\n" + "<p>${content}</p>\n" + "<time>${pubDate}</time>";

  const compiled = compile(template);
  const nestedCompiled = compile(nestedTemplate);

  const context = createContext({
    title: "Hello, world!",
    content: "This is a test.",
    pubDate: format(new Date(2023, 0, 1), "yyyy-MM-dd"),
  });
  const innerRendered = nestedCompiled(context);
  const rendered = compiled({ ...context, content: innerRendered });

  assertEquals(
    rendered,
    `<main><h1>Hello, world!</h1>\n<p>This is a test.</p>\n<time>2023-01-01</time></main>\n`,
  );
});
