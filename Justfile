alias b := build
alias r := run
alias l := lock
alias t := test

flags := "--lock lock.json --unstable --import-map import_map.json"

build: lock
    deno compile {{flags}} -A -o site main.ts

run COMMAND *FLAGS: lock
    deno run {{flags}} -A main.ts {{COMMAND}} {{FLAGS}}

lock:
    deno cache {{flags}} --lock-write main.ts src/**/*.ts

fmt: lock
    deno fmt src main.ts

lint: lock
    deno lint src main.ts

test: lock
    deno test -A -q {{flags}}