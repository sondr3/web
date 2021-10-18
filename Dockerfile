FROM node:16 as styles

WORKDIR /app

COPY package.json package-lock.json ./
RUN npm ci

COPY postcss.config.js tailwind.config.js ./

COPY templates templates
COPY public/styles.css public/styles.css

RUN npm run styles:prod

FROM rust:1.55-alpine as builder

WORKDIR /app

RUN apk add --no-cache musl-dev

COPY Cargo.lock Cargo.toml ./
RUN mkdir src
RUN echo 'fn main() { println!("Hello, world!"); }' > src/main.rs
RUN cargo build --release

COPY public public
COPY --from=styles /app/public/tailwind.css /app/public/tailwind.css
COPY templates templates
COPY src src

RUN touch ./src/main.rs

RUN cargo build --release

FROM alpine:3

WORKDIR /web

COPY --from=builder /app/target/release/web web

RUN chmod +x web

ENV WEB_PORT=8082

CMD ["./web"]