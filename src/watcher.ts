import { buildPages, writeAssets } from "./build.ts";
import { PATHS } from "./constants.ts";
import { Site } from "./site.ts";

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

export class Watcher {
  private site: Site;
  private tx: BroadcastChannel;

  constructor(site: Site, tx: BroadcastChannel) {
    this.site = site;
    this.tx = tx;
  }

  public start = (): void => {
    (async () => await this.watchContent())();
    (async () => await this.watchSCSS())();
  };

  private async watchSCSS() {
    const watcher = createWatcher(PATHS.styles);
    for await (const _event of watcher) {
      await this.site.collectCSS();
      await writeAssets(this.site.assets);
      this.tx.dispatchEvent(new MessageEvent("message", { data: { type: "reload" } }));
    }
  }

  private async watchContent() {
    const watcher = createWatcher(PATHS.content);
    for await (const _event of watcher) {
      await this.site.collectContents();
      await buildPages(this.site.content, this.site);
      this.tx.dispatchEvent(new MessageEvent("message", { data: { type: "reload" } }));
    }
  }
}
