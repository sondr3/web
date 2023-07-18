use std::{
    ffi::OsStr,
    io::Write,
    net::{Ipv4Addr, SocketAddr},
    path::Path,
    str::FromStr,
    thread,
};

use anyhow::{anyhow, Result};
use crossbeam_channel::Receiver;
use tiny_http::{HTTPVersion, Header, Request};

use crate::Event;

pub fn create(root: &Path, rx: Receiver<Event>) -> Result<()> {
    let addr = SocketAddr::from((Ipv4Addr::LOCALHOST, 3000));
    let server = tiny_http::Server::http(addr).map_err(|_| anyhow!("Failed to start server"))?;
    let root = root.canonicalize()?;

    for req in server.incoming_requests() {
        if let Err(e) = if dbg!(req.url()) == "/sse" {
            let rx = rx.clone();
            handle_sse(req, &rx)
        } else {
            handle_static_file(&root, req)
        } {
            tracing::error!("Error handling request: {}", e);
        }
    }

    Ok(())
}

fn handle_sse(req: Request, rx: &Receiver<Event>) -> Result<()> {
    dbg!(&req);
    let response = tiny_http::Response::empty(tiny_http::StatusCode(200));
    let headers = [
        "Content-Type: text/event-stream".parse::<Header>().unwrap(),
        "Cache-Control: no-cache".parse::<Header>().unwrap(),
    ];

    let resp = "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nCache-Control: no-cache\n\n";

    let mut stream = req.into_writer();
    stream.write_all(resp.as_bytes())?;
    stream.flush()?;

    while let Ok(event) = rx.recv() {
        match event {
            Event::Reload => {
                stream.write_all(b"data: reload\n\n")?;
            }
            Event::Shutdown => {
                stream.write_all(b"data: reload\n\n")?;
            }
        }

        stream.flush()?;
    }

    drop(stream);
    println!("not here");

    Ok(())
}

fn handle_static_file(root: &Path, req: Request) -> Result<()> {
    let path = root.join(&req.url()[1..]);
    let serve_path = if path.is_file() {
        path
    } else {
        path.join("index.html")
    };

    tracing::debug!("Serving {:?}", serve_path);

    if serve_path.exists() {
        let content = std::fs::File::open(&serve_path)?;
        let mut res = tiny_http::Response::from_file(content);
        let mime = match_extension(serve_path.extension());
        let content_type = format!("Content-Type: {mime}");
        let content_type = Header::from_str(&content_type).unwrap();
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
        Some("jpeg" | "jpg") => "image/jpeg",
        Some("js" | "cjs" | "mjs") => "application/javascript",
        Some("json") => "application/json",
        Some("mp4") => "video/mp4",
        Some("mpeg" | "mpg") => "video/mpeg",
        Some("png") => "image/png",
        Some("svg") => "image/svg+xml",
        Some("txt") => "text/plain; charset=utf8",
        Some("woff") => "application/font-woff",
        Some("woff2") => "application/font-woff2",
        Some("sitemap" | "xml" | "xsl" | "xss") => "application/xml",
        _ => "application/octet-stream",
    }
}
