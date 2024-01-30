import * as fs from "node:fs/promises";
import log from "loglevel";
// @ts-ignore: needs `node16` module resolution to not complain
import meow from "meow";
import { compressFolder } from "./compress.js";
import { PATHS } from "./constants.js";
import { Server } from "./server.js";
import { Site } from "./site.js";
import { FsEmitter, Watcher } from "./watcher.js";

log.setDefaultLevel("info");

const cli = meow(
	`
web - website generator

Options:
  -s, --server      Disable dev server
  -p, --production  Optimize output
  -v, --verbose     Verbose output
  -h, --help        This message
  
Environment variables:
  CI,PROD           Optimize output
`,
	{
		importMeta: import.meta,
		flags: {
			server: {
				type: "boolean",
				default: !process.env["CI"],
				shortFlag: "s",
			},
			production: {
				type: "boolean",
				default: !!process.env["CI"],
				shortFlag: "p",
			},
			verbose: {
				type: "boolean",
				default: false,
				shortFlag: "v",
			},
			help: {
				type: "boolean",
				default: false,
				shortFlag: "h",
			},
		},
	},
);

if (cli.flags.help) {
	console.info(cli.help);
	process.exitCode = 0;
}

try {
	await fs.rm(PATHS.out, { recursive: true });
} catch {
	/* noop */
}

const site = await Site.create(cli.flags.production ? "prod" : "dev");
await site.write();

if (cli.flags.server && !cli.flags.production) {
	const tx = new FsEmitter();
	void new Watcher(site, tx).start();
	void new Server(tx).start();
}

if (site.isProd) {
	await compressFolder(PATHS.out);
}
