import assert from "node:assert/strict";
import { basename } from "node:path";
import test from "node:test";

import { fromAsyncIterable, ignoreDir, walkDir } from "./utils.js";

test("ignoreDir()", () => {
	const dirs = ["node_modules", ".git", ".github", ".."];
	for (const dir of dirs) {
		assert(ignoreDir(dir), `${dir} should be a valid dir`);
	}
});

test("walkDir()", async () => {
	const files = await fromAsyncIterable(walkDir(`${process.cwd()}/.github`));
	assert(files.some((f) => basename(f) === "dependabot.yml"));
	assert(files.some((f) => basename(f) === "pipeline.yml"));
});
