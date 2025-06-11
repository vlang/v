### Running the Server

To run the example server in production mode, use the following command:

```sh
v -prod run examples/etag
```

### Serving the Front-End

To serve the front-end files, execute:

```sh
v -e 'import net.http.file; file.serve(folder: "examples/etag/front-end")'
```

### Testing with ETag

You can test the server's ETag functionality using `curl`:

1. Fetch a resource:

   ```sh
   curl -v http://localhost:3001/user/1
   ```

2. Test with a specific ETag:
   ```sh
   curl -v -H "If-None-Match: c4ca4238a0b923820dcc509a6f75849b" http://localhost:3001/user/1
   ```

### Benchmarking with `wrk`

You can benchmark the server's performance using `wrk`. For example:

```sh
wrk -t16 -c512 -d30s http://localhost:3001/user/1
```

### Benchmarking with ETag Header

To benchmark the server's performance while including an ETag header, use the following `wrk` command:

```sh
wrk -t16 -c512 -d30s -H "If-None-Match: c4ca4238a0b923820dcc509a6f75849b" http://localhost:3001/user/1
```
