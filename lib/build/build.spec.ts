import { Site } from "../site"
import { buildSite } from "./build"

describe("buildSite", () => {
  it("works with default config", async () => {
    const result = await buildSite(new Site(), false).run()
    expect(result.isRight()).toBeTruthy()
  })
})
