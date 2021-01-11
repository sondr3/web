import { convertDate } from "./helpers"

describe("convertDate", () => {
  it("converts a date", () => {
    expect(convertDate(new Date().toISOString())).toBeDefined()
  })

  it("returns undefined when undefined", () => {
    // eslint-disable-next-line unicorn/no-useless-undefined
    expect(convertDate(undefined)).toBeUndefined()
  })
})
