import htmlnano, { type HtmlnanoOptions } from "htmlnano";
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
	const options: HtmlnanoOptions = {
		skipConfigLoading: true,
		minifyCss: false,
		minifyJs: true,
	};

	const res = await htmlnano.process<string>(html, options, htmlnano.presets.safe);
	return res.html;
};

// TODO: re-enable when minify-html works on Apple Silicon
// export const minifyHTML = (html: string): Buffer => {
// 	const minified = minify(Buffer.from(html), {
// 		ensure_spec_compliant_unquoted_attribute_values: true,
// 		keep_spaces_between_attributes: true,
// 		keep_closing_tags: true,
// 		keep_comments: false,
// 		keep_html_and_head_opening_tags: true,
// 		// keep_input_type_text_attr: true,
// 		keep_ssi_comments: false,
// 		minify_css: true,
// 		do_not_minify_doctype: true,
// 		minify_js: true,
// 		preserve_brace_template_syntax: false,
// 		preserve_chevron_percent_template_syntax: false,
// 		remove_bangs: true,
// 		remove_processing_instructions: true,
// 	});

// 	return minified;
// };
