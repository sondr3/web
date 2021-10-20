use anyhow::Result;
use askama::Template;
use axum::{
    body::{Bytes, Full, HttpBody},
    handler::get,
    http::{header, HeaderValue, Response, StatusCode},
    response::IntoResponse,
    Router,
};
use md5::{Digest, Md5};
use std::{convert::Infallible, net::SocketAddr, time::Duration};
use tower::{BoxError, ServiceBuilder};
use tower_http::{
    compression::CompressionLayer, decompression::DecompressionLayer, services::ServeDir,
    set_header::SetResponseHeaderLayer, trace::TraceLayer,
};

#[cfg(not(debug_assertions))]
use minify_html::{minify, Cfg};

const FAVICON: &[u8] = include_bytes!("../public/favicon.ico");
const APPLE_ICON: &[u8] = include_bytes!("../public/apple-touch-icon.png");
const ICON_192: &[u8] = include_bytes!("../public/icon-192.png");
const ICON_512: &[u8] = include_bytes!("../public/icon-512.png");
const ICON_SVG: &[u8] = include_bytes!("../public/icon.svg");
const ROBOTS: &[u8] = include_bytes!("../public/robots.txt");
const HUMANS: &[u8] = include_bytes!("../public/humans.txt");
const STYLES: &[u8] = include_bytes!("../public/tailwind.css");

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
        Asset::new(FAVICON, "image/x-icon", 31536000)
    }

    async fn apple() -> impl IntoResponse {
        Asset::new(APPLE_ICON, "image/png", 31536000)
    }

    async fn icon_192() -> impl IntoResponse {
        Asset::new(ICON_192, "image/png", 31536000)
    }

    async fn icon_512() -> impl IntoResponse {
        Asset::new(ICON_512, "image/png", 31536000)
    }

    async fn svg() -> impl IntoResponse {
        Asset::new(ICON_SVG, "image/svg+xml", 31536000)
    }
}

async fn robots() -> impl IntoResponse {
    Asset::new(ROBOTS, "text/plain; charset=utf-8", 31536000)
}

async fn humans() -> impl IntoResponse {
    Asset::new(HUMANS, "text/plain; charset=utf-8", 31536000)
}

async fn styles() -> impl IntoResponse {
    Asset::new(STYLES, "text/css; charset=utf-8", 31536000)
}

struct Asset<'a, T>
where
    T: ?Sized + Copy + Into<&'a [u8]>,
{
    etag: String,
    cache_control: String,
    asset_kind: &'a str,
    content: T,
}

impl<'a, T> Asset<'a, T>
where
    T: Copy,
    &'a [u8]: From<T>,
{
    fn new(content: T, asset_kind: &'a str, max_age: usize) -> Self {
        Asset {
            etag: hash_content(content.into()),
            cache_control: format!("max-age={}, public, immutable", max_age),
            asset_kind,
            content,
        }
    }
}

impl<T> IntoResponse for Asset<'static, T>
where
    T: Into<Full<Bytes>> + Copy,
    &'static [u8]: From<T>,
{
    type Body = Full<Bytes>;
    type BodyError = Infallible;

    fn into_response(self) -> Response<Self::Body> {
        let mut res = Response::new(self.content.into());
        res.headers_mut().insert(
            header::CONTENT_TYPE,
            HeaderValue::from_str(self.asset_kind).expect("Invalid Content-Type"),
        );
        res.headers_mut().insert(
            header::ETAG,
            HeaderValue::from_str(&self.etag).expect("Invalid ETAG value"),
        );
        res.headers_mut().insert(
            header::CACHE_CONTROL,
            HeaderValue::from_str(&self.cache_control).unwrap(),
        );
        if let Some(size) = res.body().size_hint().exact() {
            res.headers_mut().insert(
                header::CONTENT_LENGTH,
                HeaderValue::from_str(&size.to_string()).unwrap(),
            );
        }
        res
    }
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
            etag: hash_content(content.as_bytes()),
            cache_control: format!("max-age={}, public, immutable", max_age),
            content: minify_html(content),
        }
    }
}

fn hash_content(content: &[u8]) -> String {
    format!("{:x}", Md5::digest(content))
}

#[derive(Template)]
#[template(path = "index.html")]
struct IndexTemplate;

async fn index() -> impl IntoResponse {
    Page::new(IndexTemplate, 300)
}

fn content_length_from_response<B: HttpBody>(response: &Response<B>) -> Option<HeaderValue> {
    response
        .body()
        .size_hint()
        .exact()
        .map(|size| HeaderValue::from_str(&size.to_string()).unwrap())
}

#[tokio::main]
async fn main() -> Result<(), BoxError> {
    if std::env::var("RUST_LOG").is_err() {
        std::env::set_var("RUST_LOG", "web=debug,tower_http=debug");
    }

    tracing_subscriber::fmt::init();

    let services = ServiceBuilder::new()
        .timeout(Duration::from_secs(10))
        .layer(TraceLayer::new_for_http())
        .layer(CompressionLayer::new())
        .layer(DecompressionLayer::new())
        .layer(SetResponseHeaderLayer::if_not_present(
            header::CONTENT_LENGTH,
            content_length_from_response,
        ));

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
        .nest(
            "/static",
            axum::service::get(ServeDir::new("static")).handle_error(|error: std::io::Error| {
                Ok::<_, Infallible>((
                    StatusCode::INTERNAL_SERVER_ERROR,
                    format!("unhandled internal error: {}", error),
                ))
            }),
        )
        .layer(services.into_inner())
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
