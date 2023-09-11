import { expect, test } from "@playwright/test";

test.beforeEach(async ({}, testInfo) => {
  testInfo.snapshotSuffix = "";
});

test("landing", async ({ page }) => {
  await page.goto("/");
  await expect(page).toHaveTitle("Home => Eons :: IO ()");
  await expect(page.locator("#hello")).toContainText("Hello! I'm Sondre");
  await expect(page).toHaveScreenshot({ maxDiffPixelRatio: 0.05 });
});

test("about", async ({ page }) => {
  await page.goto("/about/");
  await expect(page).toHaveTitle("About me => Eons :: IO ()");
  await expect(page.locator(".main")).toContainText("About me");
  await expect(page).toHaveScreenshot({ maxDiffPixelRatio: 0.05 });
});

test("404", async ({ page }) => {
  await page.goto("/404/");
  await expect(page).toHaveTitle("404 => Eons :: IO ()");
});
