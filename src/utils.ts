import * as fs from "node:fs/promises";
import * as path from "node:path";

export const ensureDir = async (dir: string): Promise<void> => {
	try {
		await fs.mkdir(dir, { recursive: true });
	} catch (e) {
		const error = e as NodeJS.ErrnoException;
		if (error.code !== "EEXIST") {
			throw e;
		}
	}
};

export async function fromAsyncIterable<T>(gen: AsyncIterable<T>): Promise<Array<T>> {
	const out: Array<T> = [];
	for await (const x of gen) {
		out.push(x);
	}
	return out;
}

export const ignoreDir = (dir: string): boolean => dir === "node_modules" || dir.startsWith(".");
export async function* walkDir(dir: string): AsyncGenerator<string> {
	for await (const d of await fs.opendir(dir)) {
		const entry = path.join(dir, d.name);

		if (d.isDirectory() && !ignoreDir(d.name)) {
			yield* walkDir(entry);
		} else if (d.isFile()) {
			yield entry;
		}
	}
}
