alias b := build
alias r := run
alias l := lock
alias t := test

flags := "--lock lock.json --unstable --import-map import_map.json"

build:
    deno compile {{flags}} -A -o site main.ts

run COMMAND *FLAGS:
    deno run {{flags}} -A main.ts {{COMMAND}} {{FLAGS}}

lock:
    deno cache {{flags}} --lock-write main.ts

fmt:
    deno fmt src main.ts

lint:
    deno lint

test:
    deno test -A --unstable --import-map import_map.json