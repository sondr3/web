use std::{
    io::Write,
    net::{Ipv4Addr, SocketAddr},
    thread,
};

use anyhow::{anyhow, Result};
use crossbeam_channel::Receiver;
use tiny_http::{HTTPVersion, Header, Request};

use crate::Event;

pub fn create(rx: &Receiver<Event>) -> Result<()> {
    let addr = SocketAddr::from((Ipv4Addr::LOCALHOST, 3001));
    let server = tiny_http::Server::http(addr).map_err(|_| anyhow!("Failed to start server"))?;

    for req in server.incoming_requests() {
        dbg!(&req);
        let rx = rx.clone();
        thread::spawn(move || handle_sse(req, &rx));
    }

    Ok(())
}

fn handle_sse(req: Request, rx: &Receiver<Event>) -> Result<()> {
    let response = tiny_http::Response::empty(tiny_http::StatusCode(200));
    let headers = [
        "Content-Type: text/event-stream".parse::<Header>().unwrap(),
        "Cache-Control: no-cache".parse::<Header>().unwrap(),
    ];

    let mut stream = req.into_writer();
    response.raw_print(&mut stream, HTTPVersion(1, 0), &headers, true, None)?;

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
