### Run the SSE server

```sh
v -prod run examples/sse
```

### Serve the front-end

```sh
v -e 'import net.http.file; file.serve(folder: "examples/sse/front-end")'
```

### Send notification

```sh
curl -X POST -v http://localhost:3001/notification
```
