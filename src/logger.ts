import log4js from "log4js";

export const logConfig = log4js.configure({
	appenders: { default: { type: "console", layout: { type: "colored" } } },
	categories: { default: { level: "info", appenders: ["default"] } },
});
