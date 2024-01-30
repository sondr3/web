import { expect, test } from "@playwright/test";

test("has correct landing page", async ({ page }) => {
	await page.goto("/");
	await expect(page).toHaveTitle("Home => Eons :: IO ()");
	expect(page.getByText("Hello! I'm Sondre")).toBeDefined();
});

test("going to about works", async ({ page }) => {
	await page.goto("/");

	await page.getByRole("link", { name: "about" }).click();

	await expect(page).toHaveURL(/about/);
	await expect(page.getByRole("heading", { name: "About me" })).toBeVisible();
});

test("404 is correct", async ({ page }) => {
	await page.goto("/does/not/exist");

	await expect(page).toHaveURL(/does\/not\/exist/);
	await expect(page.getByRole("heading", { name: "Page not found" })).toBeVisible();
});

test("sitemap works", async ({ page }) => {
	await page.goto("/sitemap.xml");

	await expect(page).toHaveTitle("Sitemap => Eons :: IO ()");
	expect(page.getByText("An overview over all the")).toBeDefined();
});
