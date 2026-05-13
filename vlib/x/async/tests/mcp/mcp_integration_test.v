import context
import mcp
import x.async as xasync

struct TestEchoArgs {
pub:
	text string
}

fn test_mcp_task_dispatches_in_memory_request() {
	request := mcp.new_request(1, 'tools/call', TestEchoArgs{
		text: 'hello'
	})
	mut task := xasync.run[mcp.Response](fn [request] (mut ctx context.Context) !mcp.Response {
		_ = ctx
		req := mcp.decode_request(request.encode())!
		if req.method != 'tools/call' {
			return error('unexpected MCP method')
		}
		args := req.decode_params[TestEchoArgs]()!
		result := mcp.tool_text_result(args.text)
		return mcp.new_response(1, result, mcp.ResponseError{})
	})!

	response := task.wait()!
	result := response.decode_result[mcp.ToolResult]()!
	assert result.content.contains('hello')
}
