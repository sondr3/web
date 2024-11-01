import { type ChildrenOf, h } from "@sondr3/radiant";
import { s } from "@sondr3/radiant/svg";
import type { Context } from "../content.js";

export const base = (
	{ isDev, frontmatter, canonicalUrl, css, title }: Omit<Context, "content">,
	content: ChildrenOf<"main">,
) =>
	h.document(
		h.doctype(),
		h.html(
			{ lang: "en", "data-theme": "light" },
			h.head(
				h.meta({ charset: "utf-8" }),
				h.meta({ name: "viewport", content: "width=device-width, initial-scale=1.0" }),

				h.link({ rel: "icon", type: "image/svg+xml", href: "/favicon.svg" }),
				h.link({ href: "/favicon.ico", rel: "icon" }),
				h.link({ rel: "alternate icon", href: "/icon-192.png", sizes: "192x192", type: "image/png" }),
				h.link({ rel: "alternate icon", href: "/icon-512.png", sizes: "512x512", type: "image/png" }),
				h.link({ href: "/apple-touch-icon.png", rel: "apple-touch-icon", sizes: "180x180" }),

				h.link({ href: `/${css?.dest.filename}`, rel: "stylesheet" }),

				h.link({ href: "/humans.txt", rel: "author" }),
				h.meta({ content: "Sondre Aasemoen", name: "author" }),
				h.link({ rel: "me", href: "https://fosstodon.org/@sondre" }),

				h.link({
					as: "font",
					crossorigin: "anonymous",
					href: "/fonts/Piazzolla.woff2",
					rel: "preload",
					type: "font/woff2",
				}),
				h.link({
					as: "font",
					crossorigin: "anonymous",
					href: "/fonts/Inconsolata.woff2",
					rel: "preload",
					type: "font/woff2",
				}),

				h.title(title),
				h.meta({ content: frontmatter.description, name: "description" }),
				h.link({ href: canonicalUrl, rel: "canonical" }),

				h.meta({ content: "en", property: "og:locale" }),
				h.meta({ content: "website", property: "og:type" }),
				h.meta({ content: title, property: "og:title" }),
				h.meta({ content: frontmatter.description, property: "og:description" }),
				h.meta({ content: canonicalUrl, property: "og:url" }),

				h.addIf(isDev, h.script({ src: "/livereload.js" })),

				h.script(`
        if (
          localStorage.theme === "dark" ||
          (!("theme" in localStorage) && window.matchMedia("(prefers-color-scheme: dark)").matches)
        ) {
          document.documentElement.setAttribute("data-theme", "dark");
        }
      `),
			),
			h.body(
				{ class: "root" },
				h.header(
					{ class: "header" },
					h.h1({ class: "title" }, h.a({ href: "/" }, "EONS :: IO ()")),
					h.nav(
						{ class: "nav" },
						h.ul({ class: "nav__links" }, h.li({ class: "nav__link" }, h.a({ href: "/about/" }, "about"))),
					),
					h.button(
						{ class: "theme-toggle theme-btn -light", "aria-label": "Toggle dark mode" },
						h.svg(
							{
								_xmlns: "http://www.w3.org/2000/svg",
								_fill: "none",
								_viewBox: "0 0 24 24",
								"_stroke-width": "1.5",
								_stroke: "currentColor",
								width: "24",
							},
							s.path({
								"stroke-linecap": "round",
								"stroke-linejoin": "round",
								d: "M21.752 15.002A9.718 9.718 0 0118 15.75c-5.385 0-9.75-4.365-9.75-9.75 0-1.33.266-2.597.748-3.752A9.753 9.753 0 003 11.25C3 16.635 7.365 21 12.75 21a9.753 9.753 0 009.002-5.998z",
							}),
						),
					),
					h.button(
						{ class: "theme-toggle theme-btn -dark", "aria-label": "Toggle light mode" },
						h.svg(
							{
								_id: "dark",
								_xmlns: "http://www.w3.org/2000/svg",
								_fill: "none",
								_viewBox: "0 0 24 24",
								"_stroke-width": "1.5",
								_stroke: "currentColor",
								width: "24",
							},
							s.path({
								"stroke-linecap": "round",
								"stroke-linejoin": "round",
								d: "M12 3v2.25m6.364.386l-1.591 1.591M21 12h-2.25m-.386 6.364l-1.591-1.591M12 18.75V21m-4.773-4.227l-1.591 1.591M5.25 12H3m4.227-4.773L5.636 5.636M15.75 12a3.75 3.75 0 11-7.5 0 3.75 3.75 0 017.5 0z",
							}),
						),
					),
				),
				h.main({ class: "main" }, content),
				h.footer(
					{ class: "footer" },
					h.section(
						{ class: "footer__about prose" },
						h.p(
							"© Sondre Aasemoen, content licensed under ",
							h.a({ href: "https://creativecommons.org/licenses/by-sa/4.0/", class: "grey" }, "CC BY-SA 4.0"),
						),
					),
					h.section(
						{ class: "footer__icons" },
						h.a(
							{
								class: "footer__icon",
								target: "_blank",
								rel: "noopener noreferrer",
								href: "https://fosstodon.org/@sondre",
							},
							h.svg(
								{ viewBox: "0 0 24 24", _fill: "currentColor", width: "24" },
								s.path({
									"fill-rule": "evenodd",
									"clip-rule": "evenodd",
									d: "M21.258 13.99c-.274 1.41-2.456 2.955-4.962 3.254-1.306.156-2.593.3-3.965.236-2.243-.103-4.014-.535-4.014-.535 0 .218.014.426.04.62.292 2.215 2.196 2.347 4 2.41 1.82.062 3.44-.45 3.44-.45l.076 1.646s-1.274.684-3.542.81c-1.25.068-2.803-.032-4.612-.51-3.923-1.039-4.598-5.22-4.701-9.464-.031-1.26-.012-2.447-.012-3.44 0-4.34 2.843-5.611 2.843-5.611 1.433-.658 3.892-.935 6.45-.956h.062c2.557.02 5.018.298 6.451.956 0 0 2.843 1.272 2.843 5.61 0 0 .036 3.201-.397 5.424zm-2.956-5.087c0-1.074-.273-1.927-.822-2.558-.567-.631-1.308-.955-2.229-.955-1.065 0-1.871.41-2.405 1.228l-.518.87-.519-.87C11.276 5.8 10.47 5.39 9.405 5.39c-.921 0-1.663.324-2.229.955-.549.631-.822 1.484-.822 2.558v5.253h2.081V9.057c0-1.075.452-1.62 1.357-1.62 1 0 1.501.647 1.501 1.927v2.79h2.07v-2.79c0-1.28.5-1.927 1.5-1.927.905 0 1.358.545 1.358 1.62v5.1h2.08V8.902z",
								}),
							),
						),
						h.a(
							{
								class: "footer__icon",
								target: "_blank",
								rel: "noopener noreferrer",
								href: "https://github.com/sondr3",
							},
							h.svg(
								{ _fill: "currentColor", viewBox: "0 0 24 24", width: "24" },
								s.path({
									"fill-rule": "evenodd",
									"clip-rule": "evenodd",
									d: "M12 2C6.477 2 2 6.484 2 12.017c0 4.425 2.865 8.18 6.839 9.504.5.092.682-.217.682-.483 0-.237-.008-.868-.013-1.703-2.782.605-3.369-1.343-3.369-1.343-.454-1.158-1.11-1.466-1.11-1.466-.908-.62.069-.608.069-.608 1.003.07 1.531 1.032 1.531 1.032.892 1.53 2.341 1.088 2.91.832.092-.647.35-1.088.636-1.338-2.22-.253-4.555-1.113-4.555-4.951 0-1.093.39-1.988 1.029-2.688-.103-.253-.446-1.272.098-2.65 0 0 .84-.27 2.75 1.026A9.564 9.564 0 0112 6.844c.85.004 1.705.115 2.504.337 1.909-1.296 2.747-1.027 2.747-1.027.546 1.379.202 2.398.1 2.651.64.7 1.028 1.595 1.028 2.688 0 3.848-2.339 4.695-4.566 4.943.359.309.678.92.678 1.855 0 1.338-.012 2.419-.012 2.747 0 .268.18.58.688.482A10.019 10.019 0 0022 12.017C22 6.484 17.522 2 12 2z",
								}),
							),
						),
					),
				),
				h.script(`
        function toggle() {
          if (document.documentElement.getAttribute("data-theme") === "dark") {
            document.documentElement.setAttribute("data-theme", "light");
            window.localStorage.setItem("theme", "light");
          } else {
            document.documentElement.setAttribute("data-theme", "dark");
            window.localStorage.setItem("theme", "dark");
          }
        }
        document.querySelectorAll(".theme-toggle").forEach((e) => e.addEventListener("click", toggle));
      `),
			),
		),
	);
