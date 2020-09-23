import { html } from "./HTML";

describe("HTML", () => {
  it("can render som HTML", () => {
    const things = [
      { name: "Test1", bool: false },
      { name: "Test2", bool: true },
      { name: "Test3", bool: true },
      { name: "Test4", bool: true },
    ];

    const document = (body: string, head: string): string =>
      html`<html lang="en">
        ${head}
        <body>
          ${body}
        </body>
      </html>`;

    const header = html`
      <head>
        <title>Hello!</title>
        <meta charset="UTF-8" />
        <meta content="width=device-width, initial-scale=1" name="viewport" />
      </head>
    `;

    const output = document(
      html`<div>
        <p>Hello, world!</p>
        <div>${things.map(({ name, bool }) => html`<span>${name} - ${bool.valueOf()}</span>`)}</div>
      </div>`,
      header,
    );

    expect(typeof output).toBe("string");
    expect(output).toMatchSnapshot();
  });
});
