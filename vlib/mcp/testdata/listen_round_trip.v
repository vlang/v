import mcp
import os

fn main() {
	match os.args[1] {
		'server' { serve()! }
		'client' { listen()! }
		else { panic('expected server or client mode') }
	}
}

fn serve() ! {
	mut server := mcp.new_server(name: 'listen-regression', version: '1')
	server.add_tool(mcp.Tool{ name: 'ping' }, fn (_ mcp.Context, _ string) !mcp.ToolResult {
		return mcp.tool_text_result('pong')
	})!
	server.serve_stdio()!
}

fn listen() ! {
	mut client := mcp.connect_stdio(os.executable(), ['server'], mcp.ClientConfig{
		protocol_version: mcp.protocol_version_2026_07_28
	})!
	defer { client.close() }
	filter := client.listen(mcp.SubscriptionListenParams{
		notifications: mcp.SubscriptionFilter{ tools_list_changed: true }
	})!
	assert filter.tools_list_changed
	notifications := client.take_notifications()
	assert notifications.len == 1
	assert mcp.subscription_id_of(notifications[0]) or { '' } == '2'
	response := client.request_message('tools/list', mcp.empty_object)!
	assert response.error.code == 0
	assert response.result.contains('ping')
}
