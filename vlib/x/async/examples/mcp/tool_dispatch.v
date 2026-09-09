import context
import mcp
import x.async as xasync

struct EchoArgs {
pub:
	text string
}

fn dispatch_tool(raw string) !mcp.Response {
	req := mcp.decode_request(raw)!
	if req.method != 'tools/call' {
		return error('unsupported MCP method')
	}
	args := req.decode_params[EchoArgs]()!
	result := mcp.tool_text_result('echo: ${args.text}')
	return mcp.new_response(1, result, mcp.ResponseError{})
}

fn main() {
	request := mcp.new_request(1, 'tools/call', EchoArgs{
		text: 'hello'
	})
	mut task := xasync.run[mcp.Response](fn [request] (mut ctx context.Context) !mcp.Response {
		done := ctx.done()
		select {
			_ := <-done {
				return ctx.err()
			}
			else {}
		}
		return dispatch_tool(request.encode())!
	})!

	response := task.wait()!
	println(response.encode())
}
