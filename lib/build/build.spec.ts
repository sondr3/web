import { Site } from "../site"
import { buildSite } from "./build"

describe("buildSite", () => {
  it("works with default config", async () => {
    const result = await buildSite(new Site()).run()
    expect(result.isRight()).toBeTruthy()
  })
})
