const set = (val) => document.documentElement.setAttribute("data-theme", val);
if (window.localStorage.getItem("theme")) {
  set(window.localStorage.getItem("theme"));
} else if (window.matchMedia("(prefers-color-scheme: dark)").matches) {
  set("dark");
}
window
  .matchMedia("(prefers-color-scheme: dark)")
  .addEventListener("change", (e) => set(e.matches ? "dark" : "light"));
