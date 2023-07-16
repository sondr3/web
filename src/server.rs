use std::{
    ffi::OsStr,
    net::{Ipv4Addr, SocketAddr},
    str::FromStr,
};

use anyhow::{anyhow, Result};
use tiny_http::Request;

pub fn create_sync(root: &std::path::Path) -> Result<()> {
    let addr = SocketAddr::from((Ipv4Addr::LOCALHOST, 3000));
    let server = tiny_http::Server::http(addr).map_err(|_| anyhow!("Failed to start server"))?;

    for req in server.incoming_requests() {
        if let Err(e) = handle_static_file(root, req) {
            tracing::error!("Error handling request: {}", e);
        }
    }

    Ok(())
}

fn handle_static_file(root: &std::path::Path, req: Request) -> Result<()> {
    let mut req_path = req.url().to_string();

    if let Some(pos) = req_path.rfind('?') {
        req_path.truncate(pos);
    }

    let path = root.to_path_buf().join(&req_path[1..]);
    let serve_path = if path.is_file() {
        path
    } else {
        path.join("index.html")
    };

    if serve_path.exists() {
        let content = std::fs::File::open(&serve_path)?;
        let mut res = tiny_http::Response::from_file(content);
        let mime = match_extension(serve_path.extension());
        let content_type = format!("Content-Type: {mime}");
        let content_type =
            tiny_http::Header::from_str(&content_type).map_err(|_| anyhow!("Invalid header"))?;
        res.add_header(content_type);

        req.respond(res)?;
    } else {
        req.respond(tiny_http::Response::empty(404))?;
    }

    Ok(())
}

fn match_extension(extension: Option<&OsStr>) -> &'static str {
    let extension = extension.and_then(OsStr::to_str);
    match extension {
        Some("atom") => "application/atom+xml",
        Some("css") => "text/css; charset=utf8",
        Some("csv") => "text/csv; charset=utf8",
        Some("gif") => "image/gif",
        Some("gz") => "application/x-gzip",
        Some("html") => "text/html; charset=utf8",
        Some("ico") => "image/x-icon",
        Some("jpeg") => "image/jpeg",
        Some("jpg") => "image/jpeg",
        Some("js") => "application/javascript",
        Some("json") => "application/json",
        Some("mjs") => "application/javascript",
        Some("mp4") => "video/mp4",
        Some("mpeg") => "video/mpeg",
        Some("mpg") => "video/mpeg",
        Some("png") => "image/png",
        Some("sitemap") => "application/xml",
        Some("svg") => "image/svg+xml",
        Some("txt") => "text/plain; charset=utf8",
        Some("woff") => "application/font-woff",
        Some("woff2") => "application/font-woff2",
        Some("xml") => "application/xml",
        Some("xsl") => "application/xml",
        Some("xss") => "application/xml",
        _ => "application/octet-stream",
    }
}
