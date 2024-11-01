import { type ELEMENT_MAP, h } from "@sondr3/radiant";

export const home = (): ELEMENT_MAP["section"] =>
	h.section(
		h.h1({ id: "hello" }, h.span("Hello! I'm"), " ", h.span({ class: "blue" }, "Sondre")),
		h.p(
			{ class: "me prose" },
			"I am a full time nerd with a passion for programming languages, mechanical keyboards, hoarding side-projects and occasionally creating useful software.",
		),
	);
