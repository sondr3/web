import { logging } from "./index"

describe("LogManager", () => {
  it("can be initialized", () => {
    const loggingSpy = jest.spyOn(logging, "configure")
    logging.configure().registerConsoleLogger()

    expect(loggingSpy).toHaveBeenCalled()
    expect(loggingSpy).toHaveBeenCalledTimes(1)
  })
})
