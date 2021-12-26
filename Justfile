alias r := run
alias b := build
alias f := fmt
alias l := lint

flags := "--lock lock.json --unstable --import-map import_map.json"
config := "--config deno.json"

run COMMAND="" *FLAGS="":
  deno run {{flags}} {{config}} -A main.ts {{COMMAND}} {{FLAGS}}

build:
  deno compile {{flags}} {{config}} main.ts

test:
  deno test {{flags}} {{config}} --allow-all --lock-write

fmt:
  deno fmt {{config}}

lint:
  deno lint

lock:
  deno cache {{flags}} {{config}} --lock-write main.ts
