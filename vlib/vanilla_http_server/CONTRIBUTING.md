# Contributing

## Rules

- Don't slow down performance
- Always try to keep abstraction to a minimum
- Don't complicate it

## Benchmarking & Testing

### CURL

```sh
curl -X GET --verbose http://localhost:3000/ &&
curl -X POST --verbose http://localhost:3000/user &&
curl -X GET --verbose http://localhost:3000/user/1

```

### WRK

```sh
wrk  -H 'Connection: "keep-alive"' --connection 512 --threads 16 --duration 10s http://localhost:3000
```

### Valgrind

```sh
# Race condition check
v -prod -gc none .
valgrind --tool=helgrind ./vanilla
```
