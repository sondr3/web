import { convertDate } from "./helpers"

describe("convertDate", () => {
  it("converts a date", () => {
    expect(convertDate(new Date().toISOString())).toBeDefined()
  })

  it("returns undefined when undefined", () => {
    expect(convertDate(undefined)).toBeUndefined()
  })
})
