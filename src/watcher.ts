import { debounce } from "std/async/debounce.ts";
import * as log from "std/log/mod.ts";
import { PATHS } from "./constants.ts";
import { Content } from "./content.ts";
import { Site } from "./site.ts";
import { firstFilename } from "./utils.ts";
import { Asset } from "./asset.ts";

async function* createWatcher(path: string): AsyncGenerator<Deno.FsEvent> {
  const watcher = Deno.watchFs(path, { recursive: true });
  for await (const event of watcher) {
    if (filterEvent(event)) yield event;
  }

  watcher.close();
}

const filterEvent = ({ kind }: Deno.FsEvent): boolean => {
  return kind === "create" || kind === "modify" || kind === "remove";
};

const debounceHandler = debounce(async (fn: () => Promise<void>, tx: BroadcastChannel) => {
  await fn();
  tx.dispatchEvent(new MessageEvent("message", { data: { type: "reload" } }));
}, 200);

export class Watcher {
  private site: Site;
  private tx: BroadcastChannel;
  private logger = log.getLogger();

  constructor(site: Site, tx: BroadcastChannel) {
    this.site = site;
    this.tx = tx;
  }

  public start = (): void => {
    (async () => await this.watchContent())();
    (async () => await this.watchSCSS())();
    (async () => await this.watchJS())();
    (async () => await this.watchPublic())();
  };

  private async watchSCSS() {
    const watcher = createWatcher(PATHS.styles);
    for await (const _event of watcher) {
      debounceHandler(async () => {
        this.logger.info("Rebuilding CSS");
        const asset = await this.site.collectCSS();
        await asset.write(this.site);
      }, this.tx);
    }
  }

  private async watchJS() {
    const watcher = createWatcher(PATHS.js);
    for await (const event of watcher) {
      debounceHandler(async () => {
        this.logger.info(`Rebuilding JS ${firstFilename(event)}`);
        const asset = await Asset.fromPath(event.paths[0]);
        this.site.collectAsset(asset);
        await asset.write(this.site);
      }, this.tx);
    }
  }

  private async watchContent() {
    const watcher = createWatcher(PATHS.content);
    for await (const event of watcher) {
      debounceHandler(async () => {
        this.logger.info(`Rebuilding page ${firstFilename(event)}`);
        const content = await Content.fromPath(event.paths[0], "page", this.site.url);
        await content.write(this.site);
        this.site.collectContent(content);
        this.site.writeSitemap();
      }, this.tx);
    }
  }

  private async watchPublic() {
    const watcher = createWatcher(PATHS.public);
    for await (const event of watcher) {
      debounceHandler(async () => {
        this.logger.info(`Rebuilding static file ${firstFilename(event)}`);
        await this.site.collectStaticFiles();
        await this.site.copyStaticAssets();
      }, this.tx);
    }
  }
}
