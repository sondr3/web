use anyhow::Result;
use askama::Template;
use axum::{
    body::{Bytes, Full},
    handler::get,
    http::{
        header::{self, CONTENT_TYPE},
        HeaderValue, Response, StatusCode,
    },
    response::{Headers, IntoResponse},
    Router,
};
use md5::{Digest, Md5};
use std::{convert::Infallible, net::SocketAddr, time::Duration};
use tower::{BoxError, ServiceBuilder};
use tower_http::{
    compression::CompressionLayer, decompression::DecompressionLayer, trace::TraceLayer,
};

#[cfg(not(debug_assertions))]
use minify_html::{minify, Cfg};

const FAVICON: &[u8] = include_bytes!("../public/favicon.ico");
const APPLE_ICON: &[u8] = include_bytes!("../public/apple-touch-icon.png");
const ICON_192: &[u8] = include_bytes!("../public/icon-192.png");
const ICON_512: &[u8] = include_bytes!("../public/icon-512.png");
const ICON_SVG: &[u8] = include_bytes!("../public/icon.svg");
const ROBOTS: &str = include_str!("../public/robots.txt");
const HUMANS: &str = include_str!("../public/humans.txt");
const STYLES: &str = include_str!("../public/tailwind.css");

#[cfg(not(debug_assertions))]
fn minify_html(input: String) -> String {
    String::from_utf8(minify(input.as_bytes(), &Cfg::default())).expect("Minified HTML broke")
}

#[cfg(debug_assertions)]
const fn minify_html(input: String) -> String {
    input
}

pub struct Html<T>(pub T);

impl<T> IntoResponse for Html<T>
where
    T: Into<Full<Bytes>>,
{
    type Body = Full<Bytes>;
    type BodyError = Infallible;

    fn into_response(self) -> Response<Self::Body> {
        let mut res = Response::new(self.0.into());
        res.headers_mut().insert(
            header::CONTENT_TYPE,
            HeaderValue::from_static("text/html; charset=utf-8"),
        );
        res
    }
}

struct HtmlTemplate<T>(T);

impl<T> IntoResponse for HtmlTemplate<T>
where
    T: Template,
{
    type Body = Full<Bytes>;
    type BodyError = Infallible;

    fn into_response(self) -> Response<Self::Body> {
        match self.0.render() {
            Ok(html) => Html(minify_html(html)).into_response(),
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

async fn styles() -> impl IntoResponse {
    (
        Headers(vec![(CONTENT_TYPE, "text/css; charset=utf-8")]),
        STYLES,
    )
}

struct Page {
    etag: String,
    cache_control: String,
    content: String,
}

impl IntoResponse for Page {
    type Body = Full<Bytes>;
    type BodyError = Infallible;

    fn into_response(self) -> Response<Self::Body> {
        let mut res = Response::new(self.content.into());
        res.headers_mut().insert(
            header::CONTENT_TYPE,
            HeaderValue::from_static("text/html; charset=utf-8"),
        );
        res.headers_mut().insert(
            header::ETAG,
            HeaderValue::from_str(&self.etag).expect("Invalid ETAG value"),
        );
        res.headers_mut().insert(
            header::CACHE_CONTROL,
            HeaderValue::from_str(&self.cache_control).unwrap(),
        );
        res
    }
}

impl Page {
    fn new<T: Template>(content: T, max_age: usize) -> Self {
        let content = content.render().expect("Could not render template");
        Page {
            etag: hash_content(&content),
            cache_control: format!("max-age={}, public, immutable", max_age),
            content: minify_html(content),
        }
    }
}

fn hash_content(content: &str) -> String {
    format!("{:x}", Md5::digest(content.as_bytes()))
}

#[derive(Template)]
#[template(path = "index.html")]
struct IndexTemplate;

async fn index() -> impl IntoResponse {
    Page::new(IndexTemplate, 300)
}

#[tokio::main]
async fn main() -> Result<(), BoxError> {
    if std::env::var("RUST_LOG").is_err() {
        std::env::set_var("RUST_LOG", "web=debug,tower_http=debug");
    }

    tracing_subscriber::fmt::init();

    let app = Router::new()
        .route("/", get(index))
        .route("/favicon.ico", get(Favicon::favicon))
        .route("/apple-touch-icon.png", get(Favicon::apple))
        .route("/icon-192.png", get(Favicon::icon_192))
        .route("/icon-512.png", get(Favicon::icon_512))
        .route("/icon.svg", get(Favicon::svg))
        .route("/robots.txt", get(robots))
        .route("/humans.txt", get(humans))
        .route("/tailwind.css", get(styles))
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

    let port = if let Ok(val) = std::env::var("WEB_PORT") {
        val.parse().unwrap()
    } else {
        8080
    };

    let addr = SocketAddr::from(([0, 0, 0, 0], port));
    tracing::info!("Listening on http://{}", addr);

    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .with_graceful_shutdown(shutdown_signal())
        .await?;

    Ok(())
}

async fn shutdown_signal() {
    use std::io;
    use tokio::signal::unix::SignalKind;

    async fn terminate() -> io::Result<()> {
        tokio::signal::unix::signal(SignalKind::terminate())?
            .recv()
            .await;
        Ok(())
    }

    tokio::select! {
        _ = terminate() => {},
        _ = tokio::signal::ctrl_c() => {},
    }
    tracing::info!("signal received, starting graceful shutdown")
}
