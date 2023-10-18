import { buildCSS } from "./asset.ts";
import { buildPages, writeAssets } from "./build.ts";
import { PATHS } from "./constants.ts";
import { collectPages, Context } from "./context.ts";

export const startWatcher = (context: Context, tx: BroadcastChannel): void => {
  (async () => await contentWatcher(context, tx))();
  (async () => await scssWatcher(context, tx))();
};

const contentWatcher = async (context: Context, tx: BroadcastChannel): Promise<void> => {
  const watcher = createWatcher(PATHS.content);
  for await (const _event of watcher) {
    const pages = await collectPages(PATHS);
    context.pages = pages;

    await buildPages(pages, context.mode, context.assets);
    tx.dispatchEvent(new MessageEvent("message", { data: { type: "reload" } }));
  }
};

const scssWatcher = async (context: Context, tx: BroadcastChannel): Promise<void> => {
  const watcher = createWatcher(PATHS.styles);
  for await (const _event of watcher) {
    const css = await buildCSS(PATHS, context.mode);
    context.assets.set("styles.css", css);

    await writeAssets(context.assets);
    tx.dispatchEvent(new MessageEvent("message", { data: { type: "reload" } }));
  }
};

async function* createWatcher(path: string): AsyncGenerator<Deno.FsEvent> {
  const watcher = Deno.watchFs(path, { recursive: true });
  for await (const event of watcher) {
    if (filterEvent(event)) {
      yield event;
    }
  }

  watcher.close();
}

const filterEvent = (event: Deno.FsEvent): boolean => {
  return event.kind === "create" || event.kind === "modify" || event.kind === "remove";
};
