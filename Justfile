alias r := run
alias f := fmt
alias l := lint

flags := "--lock lock.json --unstable"
config := "--config deno.json"

run COMMAND="" *FLAGS="":
  deno run {{flags}} {{config}} -A main.ts {{COMMAND}} {{FLAGS}}

test:
  deno test {{flags}} {{config}}

fmt:
  deno fmt {{config}}

lint:
  deno lint

lock:
  deno cache {{flags}} {{config}} --lock-write main.ts
