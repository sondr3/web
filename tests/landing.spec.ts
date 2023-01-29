import { test, expect } from "@playwright/test";

test("landing", async ({ page }) => {
  await page.goto("/");
  await expect(page).toHaveTitle("Home => Eons :: IO ()");
  await expect(page.locator("#hello")).toContainText("Hello! I'm Sondre");
  await expect(page).toHaveScreenshot({ maxDiffPixelRatio: 0.05 });
});
