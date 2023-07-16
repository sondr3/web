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
use data_encoding::BASE64;
use sha1_smol::Sha1;
use tiny_http::{Header, Request};

use crate::Event;

pub fn create_sync(root: &Path, rx: &Receiver<Event>) -> Result<()> {
    let addr = SocketAddr::from((Ipv4Addr::LOCALHOST, 3000));
    let server = tiny_http::Server::http(addr).map_err(|_| anyhow!("Failed to start server"))?;
    let root = root.canonicalize()?;

    for req in server.incoming_requests() {
        if let Err(e) = if req.url() == "/ws" {
            let rx = rx.clone();
            thread::spawn(move || handle_websocket(req, &rx));
            Ok(())
        } else {
            handle_static_file(&root, req)
        } {
            tracing::error!("Error handling request: {}", e);
        }
    }

    Ok(())
}

fn convert_key(input: &[u8]) -> String {
    let bytes = b"258EAFA5-E914-47DA-95CA-C5AB0DC85B11";

    let mut sha1 = Sha1::default();
    sha1.update(input);
    sha1.update(bytes);

    BASE64.encode(&sha1.digest().bytes())
}

// https://github.com/tomaka/rouille/blob/536fa0eacaeef93168f3378a0296e3f99da7b3af/src/websocket/websocket.rs#L338
fn write_ws_frame<W: Write>(mut stream: W, data: &[u8]) -> Result<()> {
    // 0x80 = FIN, 0x1 = text
    let first_byte = 0x80 | 0x1;
    stream.write_all(&[first_byte])?;

    if data.len() >= 65536 {
        stream.write_all(&[127u8])?;
        let len = data.len() as u64;
        stream.write_all(&len.to_be_bytes())?;
    } else if data.len() >= 126 {
        stream.write_all(&[126u8])?;
        let len = u16::try_from(data.len())?;
        stream.write_all(&len.to_be_bytes())?;
    } else {
        stream.write_all(&[u8::try_from(data.len())?])?;
    }

    stream.write_all(data)?;
    stream.flush()?;
    Ok(())
}

fn handle_websocket(req: Request, rx: &Receiver<Event>) -> Result<()> {
    let Some(key) = req
        .headers()
        .iter()
        .find(|h| h.field.equiv("Sec-WebSocket-Key"))
        .map(|h| h.value.as_bytes())
    else {
        panic!("Missing Sec-WebSocket-Key header");
    };

    let response = tiny_http::Response::new_empty(tiny_http::StatusCode(101))
        .with_header("Upgrade: websocket".parse::<Header>().unwrap())
        .with_header("Connection: Upgrade".parse::<Header>().unwrap())
        .with_header(
            format!("Sec-WebSocket-Accept: {}", convert_key(key))
                .parse::<Header>()
                .unwrap(),
        );

    let mut stream = req.upgrade("websocket", response);

    loop {
        if let Ok(event) = rx.recv() {
            match event {
                Event::Reload => write_ws_frame(&mut stream, b"reload")?,
                Event::Shutdown => {
                    write_ws_frame(&mut stream, b"shutdown")?;
                    break;
                }
            }
        }
    }

    drop(stream);

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

#[cfg(test)]
mod tests {
    use super::convert_key;
    use crate::server::write_ws_frame;

    #[test]
    fn test_convert_key() {
        assert_eq!(
            convert_key(b"dGhlIHNhbXBsZSBub25jZQ=="),
            "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
        );
    }

    #[test]
    fn test_hello_frame() {
        let expected = vec![0x81, 0x05, 0x48, 0x65, 0x6c, 0x6c, 0x6f];
        let mut actual = Vec::new();
        write_ws_frame(&mut actual, b"Hello").unwrap();

        assert_eq!(expected, actual);
    }
}
