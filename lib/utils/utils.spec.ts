import { slugify } from "./utils"

test("slugify", () => {
  const tests = [
    ["Hello, world! It's a glorious", "hello-world-its-a-glorious"],
    ["this_ IS a % of $dollars", "this-is-a-of-dollars"],
    ["1 is equal == to ---3", "1-is-equal-to-3"],
  ]
  tests.forEach((inputs) => {
    expect(slugify(inputs[0])).toBe(inputs[1])
  })
})
