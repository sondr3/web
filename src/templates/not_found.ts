import { h } from "@sondr3/radiant";

export const notFound = () =>
	h.section(
		{ class: "four-oh-four" },
		h.h1("Page not found"),
		h.p({ class: "prose" }, "What you're looking for does not exist :("),
		h.div(h.a({ href: "/", class: "blue" }, " Go back home", h.span({ "aria-hidden": "true" }, "→"), " ")),
	);
