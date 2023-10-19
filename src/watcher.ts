import { buildPages, writeAssets } from "./build.ts";
import { PATHS } from "./constants.ts";
import { Site } from "./site.ts";

export const startWatcher = (site: Site, tx: BroadcastChannel): void => {
  (async () => await contentWatcher(site, tx))();
  (async () => await scssWatcher(site, tx))();
};

const contentWatcher = async (site: Site, tx: BroadcastChannel): Promise<void> => {
  const watcher = createWatcher(PATHS.content);
  for await (const _event of watcher) {
    await site.collectPages();
    await buildPages(site.pages, site);
    tx.dispatchEvent(new MessageEvent("message", { data: { type: "reload" } }));
  }
};

const scssWatcher = async (site: Site, tx: BroadcastChannel): Promise<void> => {
  const watcher = createWatcher(PATHS.styles);
  for await (const _event of watcher) {
    await site.collectCSS();
    await writeAssets(site.assets);
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

const filterEvent = ({ kind }: Deno.FsEvent): boolean => {
  return kind === "create" || kind === "modify" || kind === "remove";
};
