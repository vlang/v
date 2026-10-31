import mcp

fn main() {
	mut server := mcp.new_server(name: 'listen-regression', version: '1')
	server.add_tool(mcp.Tool{ name: 'ping' }, fn (_ mcp.Context, _ string) !mcp.ToolResult {
		return mcp.tool_text_result('pong')
	})!
	server.serve_stdio()!
}
