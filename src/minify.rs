use minify_html::Cfg;

pub struct Minifier {
    cfg: Cfg,
}

impl Minifier {
    pub fn new() -> Self {
        let cfg = Cfg {
            minify_js: true,
            minify_css: true,
            keep_comments: false,
            keep_html_and_head_opening_tags: true,
            remove_bangs: false,
            remove_processing_instructions: false,
            ..Cfg::spec_compliant()
        };
        Minifier { cfg }
    }

    pub fn minify(&self, html: &str) -> Vec<u8> {
        minify_html::minify(html.as_bytes(), &self.cfg)
    }
}
