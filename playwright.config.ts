import { defineConfig, devices } from "@playwright/test";

const HAS_BASE_URL = process.env.PLAYWRIGHT_BASE_URL !== undefined;
const baseURL = HAS_BASE_URL
  ? process.env.PLAYWRIGHT_BASE_URL
  : "http://localhost:3000/";

const webServer = HAS_BASE_URL
  ? undefined
  : {
      command: "statisk serve",
      url: "http://localhost:3000/",
      timeout: 120 * 1000,
      reuseExistingServer: !process.env.CI,
    };

export default defineConfig({
  testDir: "./tests",
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: 3,
  workers: process.env.CI ? 1 : undefined,
  reporter: "html",
  use: {
    baseURL,
    trace: "on-first-retry",
  },
  projects: [
    {
      name: "chromium",
      use: { ...devices["Desktop Chrome"] },
    },

    {
      name: "firefox",
      use: { ...devices["Desktop Firefox"] },
    },

    {
      name: "webkit",
      use: { ...devices["Desktop Safari"] },
    },

    /* Test against mobile viewports. */
    {
      name: "Mobile Chrome",
      use: { ...devices["Pixel 5"] },
    },
    {
      name: "Mobile Safari",
      use: { ...devices["iPhone 12"] },
    },
  ],

  /* Run your local dev server before starting the tests */
  webServer,
});
