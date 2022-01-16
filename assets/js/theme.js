function toggle(e) {
  if (e.currentTarget.classList.contains("-light")) {
    document.documentElement.setAttribute("data-theme", "light");
    window.localStorage.setItem("theme", "light");
  } else {
    document.documentElement.setAttribute("data-theme", "dark");
    window.localStorage.setItem("theme", "dark");
  }
}

const switches = document.querySelectorAll(".theme-btn");
switches.forEach((e) => e.addEventListener("click", toggle));
