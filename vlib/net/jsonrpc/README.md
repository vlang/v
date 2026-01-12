# JSONRPC
[JSON-RPC 2.0](https://www.jsonrpc.org/specification) client+server implementation in pure V.

## Limitaions
- Request/Response use only string id
- JSON-RPC 1.0 incompatible

## Features
- Request/Response single/batch json encoding/decoding
- Server to work with any io.ReaderWriter
- Server automatically manages batch Requests and builds batch Response
- Client to work with any io.ReaderWriter
- Interceptors for custom events, raw Request, Request, Response, raw Response

## Usage
### Request/Response operations
For both Request/Response constructors are provided and must be used for initialization.
```v
import jsonrpc

// jsonrpc.new_request(method, params, id)
mut req := jsonrpc.new_request("kv.create", {"key": "key", "value": "value"}, "kv.create.1")

println(req.encode())
// '{"jsonrpc":"2.0","method":"kv.create","params":{"key":"key","value":"value"},"id":"kv.create.1"}'

// jsonrpc.new_response(result, error, id)
mut resp := jsonrpc.new_response({"key": "key", "value": "value"}, jsonrpc.ResponseError{}, "kv.create.1")

println(resp.encode())
// '{"jsonrpc":"2.0","result":{"key":"key","value":"value"},"id":"kv.create.1"}'
```
To create a Notification, pass empty string as `Request.id` (`jsonrpc.Empty{}.str()` or `jsonrpc.empty.str()` can be used) (e.g. `jsonrpc.new_reponse('method', 'params', jsonrpc.empty.str())`).
To omit Response.params in encoded json string pass `jsonrpc.Empty{}` or `jsonrpc.empty` as value in constructor (e.g. `jsonrpc.new_reponse('method', jsonrpc.empty, 'id')`).
For Response only result or error fields can exist at the same time and not both simultaniously.
If error passed to Response constructor then passed result value will be ignored on json string encoding.
The error field not generated in Response if `jsonrpc.ResponseError{}` provided as value into constructor (e.g. `jsonrpc.new_response("result", jsonrpc.ResponseError{}, "id")`).
If the empty string passed as `Result.id` it will use `jsonrpc.null` as id (translates to json null)

### Client
For full usage check client in [example](examples/client.v)
```v
import net
import jsonrpc as jsonrpc

addr := '127.0.0.1:42228'
mut stream := net.dial_tcp(addr)!
mut c := jsonrpc.new_client(jsonrpc.ClientConfig{ 
    stream: stream
})

c.notify("kv.create", {
    "key": "bazz"
    "value": "barr"
})!
```
Client can work with any `io.ReaderWriter` provided into stream field value.

### Server
For ready key/value im-memory storage realized with server check this [example](examples/main.v)
```v
import net
import jsonrpc as jsonrpc

fn handle_test(req &jsonrpc.Request, mut wr jsonrpc.ResponseWriter) {
	p := req.decode_params[string]() or {
		wr.write_error(jsonrpc.invalid_params)
		return
	}

	wr.write(p)
}

fn handle_conn(mut conn net.TcpConn, h jsonrpc.Handler) {
	defer { conn.close() or {} }

	mut srv := jsonrpc.new_server(jsonrpc.ServerConfig{
		stream: conn
		handler: handle_test
	})

	srv.start()
}

addr := '127.0.0.1:42228'
mut l := net.listen_tcp(.ip, addr)!
println('TCP JSON-RPC server on ${addr} (Content-Length framing)')

for {
    mut c := l.accept()!
    println("Accepted")
    go handle_conn(mut c)
}
```
Server can work with any `io.ReaderWriter` provided into stream field value.
Server requires `jsonrpc.Handler = fn(req &jsonrpc.Request, mut wr jsonrpc.ResponseWriter)`
to pass decoded `jsonrpc.Request` and to write `jsonrpc.Response` into `jsonrpc.ResponseWriter`.
On Notification Server does call `jsonrpc.Handler` but it ingores written `jsonrpc.Response`.

### Handler
`jsonrpc.Handler = fn(req &jsonrpc.Request, mut wr jsonrpc.ResponseWriter)` is the function that
operates the decoded `jsonrpc.Request` and writes `jsonrpc.Response` into `jsonrpc.ResponseWriter`.
Before every return `wr.write()` or `wr.write_error()` must be called so the server do not stuck
waiting for `jsonrpc.Response` to be written. Also only `wr.write()` or `wr.write_error()` must
be called before return and not both.

### Router
The simple `jsonrpc.Router` is provided to register `jsonrpc.Handler` to handle specific `jsonrpc.Request.method`.
The `jsonrpc.Router.handle_jsonrpc` must be passed into `jsonrpc.Server.handler` to handle requests.
If `jsonrpc.Request.method` has no registered `jsonrpc.Handler`, the router will respond with `jsonrpc.method_not_found` error

### Interceptors
Both `jsonrpc.Client` and `jsonrpc.Server` support `jsonrpc.Interceptors` - the collection of on event
handlers. There is implementation of all supported interceptors called `jsonrpc.LoggingInterceptor`.