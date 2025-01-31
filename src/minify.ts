import htmlnano /*, { type HtmlnanoOptions } */ from "htmlnano";
import { browserslistToTargets, transform } from "lightningcss";

export const minifyCSS = (css: string): string => {
	const { code } = transform({
		filename: "styles.css",
		code: new TextEncoder().encode(css),
		minify: true,
		targets: browserslistToTargets(["> .5% and last 5 versions"]),
	});

	return new TextDecoder().decode(code);
};

export const minifyHTML = async (html: string): Promise<string> => {
	const options /* : HtmlnanoOptions */ = {
		skipConfigLoading: true,
		minifyCss: false,
		minifyJs: true,
	};

	const res = await htmlnano.process<string>(html, options, htmlnano.presets.safe);
	return res.html;
};
