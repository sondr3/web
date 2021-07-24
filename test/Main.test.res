Test.test("Hello, world", () => {
  let out = Main.hello("world")
  Assert.stringEqual(out, "Hello world")
})
