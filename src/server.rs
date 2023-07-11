use crate::{AppState, Event};
use anyhow::{Context, Result};
use axum::{
    extract::{
        ws::{Message, WebSocket},
        ConnectInfo, State, WebSocketUpgrade,
    },
    response::IntoResponse,
    routing::get,
    Router,
};
use std::{
    net::{Ipv4Addr, SocketAddr},
    sync::Arc,
};
use tower_http::{services::ServeDir, trace::TraceLayer};

pub async fn create_server(state: Arc<AppState>) -> Result<()> {
    let addr = SocketAddr::from((Ipv4Addr::LOCALHOST, 3000));
    axum::Server::bind(&addr)
        .serve(
            router(state)
                .layer(TraceLayer::new_for_http())
                .into_make_service_with_connect_info::<SocketAddr>(),
        )
        .await
        .context("Failed to start server")
}

fn router(state: Arc<AppState>) -> Router {
    Router::new()
        .fallback_service(ServeDir::new("dist").append_index_html_on_directories(true))
        .route("/ws", get(ws_handler))
        .with_state(state)
}

async fn ws_handler(
    ws: WebSocketUpgrade,
    ConnectInfo(addr): ConnectInfo<SocketAddr>,
    State(state): State<Arc<AppState>>,
) -> impl IntoResponse {
    ws.on_upgrade(move |socket| handle_socket(socket, state, addr))
}

async fn handle_socket(mut socket: WebSocket, state: Arc<AppState>, addr: SocketAddr) {
    println!("{} connected", addr);
    let mut rx = state.tx.subscribe();

    while let Ok(event) = rx.recv().await {
        match event {
            Event::Reload => socket
                .send(Message::Text("reload".to_string()))
                .await
                .unwrap(),
            Event::Shutdown => socket
                .send(Message::Text("shutdown".to_string()))
                .await
                .unwrap(),
        }
    }
}
