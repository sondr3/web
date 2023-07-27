alias b := build
alias d := dev

default: dev

build:
    cargo run -- -s

dev: build
    dev-serve dist

deps:
    brew install just watchexec dev-serve