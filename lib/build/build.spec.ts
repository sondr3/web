import { TestSite } from "../tests"
import { buildSite } from "./build"

describe("buildSite", () => {
  it("works with default config", async () => {
    const result = await buildSite(TestSite()).run()
    expect(result.isRight()).toBeTruthy()
  })
})
