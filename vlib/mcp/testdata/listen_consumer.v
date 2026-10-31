import mcp
import os

fn main() {
	mut client := mcp.connect_stdio(os.args[1], [], mcp.ClientConfig{
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
