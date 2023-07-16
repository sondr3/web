use std::{
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
        if let Some(mime) = mime_guess::from_path(&serve_path).first_raw() {
            let content_type = format!("Content-Type: {mime}");
            let content_type = tiny_http::Header::from_str(&content_type)
                .map_err(|_| anyhow!("Invalid header"))?;
            res.add_header(content_type);
        }

        req.respond(res)?;
    } else {
        req.respond(tiny_http::Response::empty(404))?;
    }

    Ok(())
}
