import { test, expect } from "@playwright/test";

test("landing", async ({ page }) => {
  await page.goto("/");
  await expect(page).toHaveTitle("Home => Eons :: IO ()");
  await expect(page.locator(".text-4xl")).toContainText("Hello! I'm Sondre");
  await expect(page).toHaveScreenshot({ maxDiffPixelRatio: 0.05 });
});
