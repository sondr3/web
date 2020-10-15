import { logging } from "./index"

describe("Logger", () => {
  describe("log", () => {
    logging.configure().registerConsoleLogger()

    it("can log", () => {
      const consoleLogSpy = jest.spyOn(console, "log").mockImplementation(() => void {})
      const logger = logging.getLogger("test")

      logger.log("Hello, world!")
      expect(consoleLogSpy).toHaveBeenCalledWith(expect.stringContaining("Hello, world!"))
    })
  })

  describe("trace", () => {
    const consoleLogSpy = jest.spyOn(console, "trace").mockImplementation(() => void {})

    beforeEach(() => {
      consoleLogSpy.mockReset()
    })

    it("no output when no log level", () => {
      const logger = logging.getLogger("test")

      logger.trace("Hello, world!")
      expect(consoleLogSpy).toHaveBeenCalledTimes(0)
    })

    it("outputs when trace level", () => {
      logging.configure({ minLevel: "trace" }).registerConsoleLogger()
      const logger = logging.getLogger("test")

      logger.trace("Hello, world!")
      expect(consoleLogSpy).toHaveBeenCalledTimes(1)
      expect(consoleLogSpy).toHaveBeenCalledWith(expect.stringContaining("[TRACE]"))
      expect(consoleLogSpy).toHaveBeenCalledWith(expect.stringContaining("Hello, world!"))
    })

    it("outputs when above debug level", () => {
      logging.configure({ minLevel: "debug" }).registerConsoleLogger()
      const logger = logging.getLogger("test")

      logger.trace("Hello, world!")
      expect(consoleLogSpy).toHaveBeenCalledTimes(1)
      expect(consoleLogSpy).toHaveBeenCalledWith(expect.stringContaining("[TRACE]"))
      expect(consoleLogSpy).toHaveBeenCalledWith(expect.stringContaining("Hello, world!"))
    })
  })

  describe("debug", () => {
    const consoleLogSpy = jest.spyOn(console, "debug").mockImplementation(() => void {})

    beforeEach(() => {
      consoleLogSpy.mockReset()
    })

    it("no output when no log level", () => {
      logging.configure({ minLevel: "none" }).registerConsoleLogger()
      const logger = logging.getLogger("test")

      logger.debug("Hello, world!")
      expect(consoleLogSpy).toHaveBeenCalledTimes(0)
    })

    it("outputs when debug level", () => {
      logging.configure({ minLevel: "debug" }).registerConsoleLogger()
      const logger = logging.getLogger("test")

      logger.debug("Hello, world!")
      expect(consoleLogSpy).toHaveBeenCalledTimes(1)
      expect(consoleLogSpy).toHaveBeenCalledWith(expect.stringContaining("[DEBUG]"))
      expect(consoleLogSpy).toHaveBeenCalledWith(expect.stringContaining("Hello, world!"))
    })

    it("outputs when above debug level", () => {
      logging.configure({ minLevel: "error" }).registerConsoleLogger()
      const logger = logging.getLogger("test")

      logger.debug("Hello, world!")
      expect(consoleLogSpy).toHaveBeenCalledTimes(1)
      expect(consoleLogSpy).toHaveBeenCalledWith(expect.stringContaining("[DEBUG]"))
      expect(consoleLogSpy).toHaveBeenCalledWith(expect.stringContaining("Hello, world!"))
    })
  })

  describe("info", () => {
    const consoleLogSpy = jest.spyOn(console, "info").mockImplementation(() => void {})

    beforeEach(() => {
      consoleLogSpy.mockReset()
    })

    it("no output when no log level", () => {
      logging.configure({ minLevel: "none" }).registerConsoleLogger()
      const logger = logging.getLogger("test")

      logger.info("Hello, world!")
      expect(consoleLogSpy).toHaveBeenCalledTimes(0)
    })

    it("outputs when info level", () => {
      logging.configure({ minLevel: "info" }).registerConsoleLogger()
      const logger = logging.getLogger("test")

      logger.info("Hello, world!")
      expect(consoleLogSpy).toHaveBeenCalledTimes(1)
      expect(consoleLogSpy).toHaveBeenCalledWith(expect.stringContaining("[INFO]"))
      expect(consoleLogSpy).toHaveBeenCalledWith(expect.stringContaining("Hello, world!"))
    })

    it("outputs when above info level", () => {
      logging.configure({ minLevel: "error" }).registerConsoleLogger()
      const logger = logging.getLogger("test")

      logger.info("Hello, world!")
      expect(consoleLogSpy).toHaveBeenCalledTimes(1)
      expect(consoleLogSpy).toHaveBeenCalledWith(expect.stringContaining("[INFO]"))
      expect(consoleLogSpy).toHaveBeenCalledWith(expect.stringContaining("Hello, world!"))
    })
  })

  describe("warn", () => {
    const consoleLogSpy = jest.spyOn(console, "warn").mockImplementation(() => void {})

    beforeEach(() => {
      consoleLogSpy.mockReset()
    })

    it("no output when no log level", () => {
      logging.configure({ minLevel: "none" }).registerConsoleLogger()
      const logger = logging.getLogger("test")

      logger.warn("Hello, world!")
      expect(consoleLogSpy).toHaveBeenCalledTimes(0)
    })

    it("outputs when warn level", () => {
      logging.configure({ minLevel: "warn" }).registerConsoleLogger()
      const logger = logging.getLogger("test")

      logger.warn("Hello, world!")
      expect(consoleLogSpy).toHaveBeenCalledTimes(1)
      expect(consoleLogSpy).toHaveBeenCalledWith(expect.stringContaining("[WARN]"))
      expect(consoleLogSpy).toHaveBeenCalledWith(expect.stringContaining("Hello, world!"))
    })

    it("outputs when above warn level", () => {
      logging.configure({ minLevel: "error" }).registerConsoleLogger()
      const logger = logging.getLogger("test")

      logger.warn("Hello, world!")
      expect(consoleLogSpy).toHaveBeenCalledTimes(1)
      expect(consoleLogSpy).toHaveBeenCalledWith(expect.stringContaining("[WARN]"))
      expect(consoleLogSpy).toHaveBeenCalledWith(expect.stringContaining("Hello, world!"))
    })
  })

  describe("error", () => {
    const consoleLogSpy = jest.spyOn(console, "error").mockImplementation(() => void {})

    beforeEach(() => {
      consoleLogSpy.mockReset()
    })

    it("no output when no log level", () => {
      logging.configure({ minLevel: "none" }).registerConsoleLogger()
      const logger = logging.getLogger("test")

      logger.error("Hello, world!")
      expect(consoleLogSpy).toHaveBeenCalledTimes(0)
    })

    it("outputs when error level", () => {
      logging.configure({ minLevel: "error" }).registerConsoleLogger()
      const logger = logging.getLogger("test")

      logger.error("Hello, world!")
      expect(consoleLogSpy).toHaveBeenCalledTimes(1)
      expect(consoleLogSpy).toHaveBeenCalledWith(expect.stringContaining("[ERROR]"))
      expect(consoleLogSpy).toHaveBeenCalledWith(expect.stringContaining("Hello, world!"))
    })
  })
})
