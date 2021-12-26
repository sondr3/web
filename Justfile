alias f := fmt
alias l := lint

flags := "--lock lock.json --unstable --import-map imports.json"
config := "--config deno.json"

fmt:
  deno fmt {{config}}

lint:
  deno lint

lock:
  deno cache {{flags}} --lock-write main.ts
