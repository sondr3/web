use anyhow::Result;
use askama::Template;
use axum::{
    body::{Bytes, Full},
    extract::Path,
    handler::get,
    http::{header::CONTENT_TYPE, Response, StatusCode},
    response::{Headers, Html, IntoResponse},
    Router,
};
use std::{convert::Infallible, net::SocketAddr, time::Duration};
use tower::{BoxError, ServiceBuilder};
use tower_http::{
    compression::CompressionLayer, decompression::DecompressionLayer, trace::TraceLayer,
};

const FAVICON: &[u8] = include_bytes!("../public/favicon.ico");
const APPLE_ICON: &[u8] = include_bytes!("../public/apple-touch-icon.png");
const ICON_192: &[u8] = include_bytes!("../public/icon-192.png");
const ICON_512: &[u8] = include_bytes!("../public/icon-512.png");
const ICON_SVG: &[u8] = include_bytes!("../public/icon.svg");
const ROBOTS: &str = include_str!("../public/robots.txt");
const HUMANS: &str = include_str!("../public/humans.txt");

struct HtmlTemplate<T>(T);

impl<T> IntoResponse for HtmlTemplate<T>
where
    T: Template,
{
    type Body = Full<Bytes>;
    type BodyError = Infallible;

    fn into_response(self) -> Response<Self::Body> {
        match self.0.render() {
            Ok(html) => Html(html).into_response(),
            Err(err) => Response::builder()
                .status(StatusCode::INTERNAL_SERVER_ERROR)
                .body(Full::from(format!(
                    "Failed to render template. Error: {}",
                    err
                )))
                .unwrap(),
        }
    }
}

async fn greet(Path(name): Path<String>) -> impl IntoResponse {
    let template = HelloTemplate { name };
    HtmlTemplate(template)
}

struct Favicon;

impl Favicon {
    async fn favicon() -> impl IntoResponse {
        Bytes::from(FAVICON)
    }

    async fn apple() -> impl IntoResponse {
        Bytes::from(APPLE_ICON)
    }

    async fn icon_192() -> impl IntoResponse {
        Bytes::from(ICON_192)
    }

    async fn icon_512() -> impl IntoResponse {
        Bytes::from(ICON_512)
    }

    async fn svg() -> impl IntoResponse {
        Bytes::from(ICON_SVG)
    }
}

async fn robots() -> impl IntoResponse {
    (
        Headers(vec![(CONTENT_TYPE, "text/plain; charset=utf-8")]),
        ROBOTS,
    )
}

async fn humans() -> impl IntoResponse {
    (
        Headers(vec![(CONTENT_TYPE, "text/plain; charset=utf-8")]),
        HUMANS,
    )
}

#[derive(Template)]
#[template(path = "hello.html")]
struct HelloTemplate {
    name: String,
}

#[tokio::main]
async fn main() -> Result<(), BoxError> {
    if std::env::var("RUST_LOG").is_err() {
        std::env::set_var("RUST_LOG", "web=debug,tower_http=debug");
    }

    tracing_subscriber::fmt::init();

    let app = Router::new()
        .route("/greet/:name", get(greet))
        .route("/favicon.ico", get(Favicon::favicon))
        .route("/apple-touch-icon.png", get(Favicon::apple))
        .route("/icon-192.png", get(Favicon::icon_192))
        .route("/icon-512.png", get(Favicon::icon_512))
        .route("/icon.svg", get(Favicon::svg))
        .route("/robots.txt", get(robots))
        .route("/humans.txt", get(humans))
        .layer(
            ServiceBuilder::new()
                .timeout(Duration::from_secs(10))
                .layer(TraceLayer::new_for_http())
                .layer(CompressionLayer::new())
                .layer(DecompressionLayer::new())
                .into_inner(),
        )
        .handle_error(|error: BoxError| {
            let result = if error.is::<tower::timeout::error::Elapsed>() {
                Ok(StatusCode::REQUEST_TIMEOUT)
            } else {
                Err((
                    StatusCode::INTERNAL_SERVER_ERROR,
                    format!("Unhandled internal error: {}", error),
                ))
            };

            Ok::<_, Infallible>(result)
        })
        .check_infallible();

    // let app = app.or(not_found.into_service());

    let addr = SocketAddr::from(([0, 0, 0, 0], 8080));
    tracing::debug!("Listening on http://{}", addr);

    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await?;

    Ok(())
}
