import { Site } from "./site.ts";

/**
 * Interface for objects that can write data from a site.
 */
/**
 * Writes data from the given site.
 * @param site The site to write data from.
 * @returns A Promise that resolves when the write operation is complete.
 * @throws An error if the write operation fails.
 */
export interface WriteFromSite {
  write(site: Site): Promise<void>;
}

/**
 * Writes the given items that implements the WriteFromSite interface to the given site.
 * @param items - A map of items to write, where the keys are the item IDs and the values are the items themselves.
 * @param site - The site to write the items to.
 * @returns A Promise that resolves when all items have been written.
 */
export const write = async (
  items: Map<string, WriteFromSite>,
  site: Site,
): Promise<void> => {
  await Promise.allSettled(
    Array.from(items.values()).map(async (item) => await item.write(site)),
  );
};
