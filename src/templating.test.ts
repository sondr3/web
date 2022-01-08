import { test } from "@sondr3/minitest";
import { strict as assert } from "node:assert";

import { html } from "./templating.js";

const EXPECTED = `<html lang="en">
      
    <head>
      <title>Hello!</title>
      <meta charset="UTF-8" />
      <meta content="width=device-width, initial-scale=1" name="viewport" />
    </head>
  
      <body>
        <div>
      <p>Hello, world!</p>
      <div>
        <span>Test1 - : 1</span><span>Test2 - : 2</span><span>Test3 - : 3</span><span>Test4 - : 4</span>
      </div>
    </div>
      </body>
    </html>`;

test("HTML", () => {
  const things = [
    { name: "Test1", bool: false, val: 1 },
    { name: "Test2", bool: true, val: 2 },
    { name: "Test3", bool: true, val: 3 },
    { name: "Test4", bool: true, val: 4 },
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
      <div>
        ${things.map(
          ({ name, bool, val }) => html`<span>${name} - ${bool.valueOf()}: ${val}</span>`,
        )}
      </div>
    </div>`,
    header,
  );

  assert.equal(typeof output, "string");
  assert.equal(output, EXPECTED);
});
