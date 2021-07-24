import { CLI } from "./cli"

describe("CLI", () => {
  it("works without a default command", () => {
    const cli = new CLI(["/usr/bin/node", "index.js"])
    expect(cli.noisiness).toBe("none")
    expect(cli.production).toBeFalsy()
    expect(cli.command).toBe("develop")
  })

  it("works with a default command", () => {
    const cli = new CLI(["/usr/bin/node", "index.js", "build"])
    expect(cli.noisiness).toBe("none")
    expect(cli.production).toBeFalsy()
    expect(cli.command).toBe("build")
  })

  it("works when overriding noisiness", () => {
    const cli = new CLI(["/usr/bin/node", "index.js", "build", "-n=trace"])
    expect(cli.noisiness).toBe("trace")
  })

  it("works when overriding noisiness multiple times", () => {
    const cli = new CLI(["/usr/bin/node", "index.js", "build", "-n=trace", "-n=error"])
    expect(cli.noisiness).toBe("error")
  })

  it("overrides production flag", () => {
    const cli = new CLI(["/usr/bin/node", "index.js", "build", "-p"])
    expect(cli.noisiness).toBe("none")
    expect(cli.production).toBeTruthy()
  })
})
