import { buildSite } from "./build"

test("buildSite", async () => {
  const res = await buildSite(false)
  expect(res).toBeUndefined()
})
