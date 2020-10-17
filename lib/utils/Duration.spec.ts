import { Duration, prettyPrintDuration } from "./Duration"

test("Duration", () => {
  const spy = jest.spyOn(process.hrtime, "bigint")
  spy.mockReturnValue(191051479007711n)

  const duration = new Duration()

  spy.mockReturnValue(191052633396993n)
  duration.end()

  expect(duration.result()).toBe("1s 154ms")
})

describe("PrettyPrintDuration", () => {
  it("Pretty formats duration, less than 1 second", () => {
    const duration = 683
    const formatted = prettyPrintDuration(duration)

    expect(typeof formatted).toBe("string")
    expect(formatted).toBe("683ms")
  })

  it("Pretty formats duration, less than 100ms", () => {
    const duration = 85
    const formatted = prettyPrintDuration(duration)

    expect(formatted).toBe("85ms")
  })

  it("Pretty formats duration, more than 1 second", () => {
    const duration = 1632
    const formatted = prettyPrintDuration(duration)

    expect(formatted).toBe("1s 632ms")
  })
})
