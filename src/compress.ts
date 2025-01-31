import { createReadStream, createWriteStream } from "node:fs";
import { extname } from "node:path";
import { performance } from "node:perf_hooks";
import { pipeline } from "node:stream/promises";
import { createBrotliCompress, createGzip } from "node:zlib";

import { logConfig } from "./logger.js";
import { walkDir } from "./utils.js";

const logger = logConfig.getLogger("compress");

const VALID_EXTENSIONS: Array<string> = [
	".html",
	".css",
	".js",
	".xml",
	".css",
	".cjs",
	".mjs",
	".json",
	".txt",
	".svg",
	".map",
];

const isCompressible = (file: string): boolean => {
	return VALID_EXTENSIONS.some((e) => e === extname(file));
};

export const compressFolder = async (dir: string): Promise<void> => {
	const start = performance.now();

	await gzip(dir);
	await brotli(dir);

	const end = performance.now();
	logger.info(`Compression took ${(end - start).toFixed(0)}ms`);
};

const gzip = async (dir: string): Promise<void> => {
	for await (const file of walkDir(dir)) {
		if (!isCompressible(file)) continue;

		const gzip = createGzip();
		const source = createReadStream(file);
		const dest = createWriteStream(`${file}.gz`);

		await pipeline(source, gzip, dest);
	}
};

const brotli = async (dir: string): Promise<void> => {
	for await (const file of walkDir(dir)) {
		if (!isCompressible(file)) continue;

		const brotli = createBrotliCompress();
		const source = createReadStream(file);
		const dest = createWriteStream(`${file}.br`);

		await pipeline(source, brotli, dest);
	}
};
