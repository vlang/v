module main

import json2 as json
import mcp
import os

fn test_help_resource_template_topics_can_be_read_over_stdio() {
	root := os.join_path(os.vtmp_dir(), 'mcp help resources review ${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	mut topics := ['default', 'test', 'fmt', 'build-c', 'install']
	mut requests := '{"jsonrpc":"2.0","id":2,"method":"resources/list"}\n' +
		'{"jsonrpc":"2.0","id":3,"method":"resources/templates/list"}\n'
	for i, topic in topics {
		requests += '{"jsonrpc":"2.0","id":${i + 4},"method":"resources/read","params":{"uri":"v://help/${topic}"}}\n'
	}
	requests += '{"jsonrpc":"2.0","id":99,"method":"resources/read","params":{"uri":"v://help/../../outside"}}\n'
	for read_only in [false, true] {
		mut output := run_help_resource_requests(root, requests, read_only)!
		mut listed := []string{}
		mut cursors := []string{}
		mut cursor := ''
		mut received := []int{}
		// Help topics span multiple pages; follow the server's opaque cursor.
		for {
			for line in output.trim_space().split_into_lines() {
				response := json.decode[map[string]json.Any](line, strict: true)!
				id := response['id']!.int()
				received << id
				if id == 99 {
					assert 'error' in response, line
					continue
				}
				result := response['result']!.as_map()
				if id == 2 {
					for resource in result['resources']!.as_array() {
						listed << resource.as_map()['uri']!.str()
					}
					cursor = if next := result['nextCursor'] { next.str() } else { '' }
				} else if id == 3 {
					templates := result['resourceTemplates']!.as_array()
					assert templates.any(it.as_map()['uriTemplate']!.str() == 'v://help/{topic}'), line
				} else if id >= 4 && id < 4 + topics.len {
					contents := result['contents']!.as_array()
					assert contents.len == 1, line
					content := contents[0].as_map()
					topic := topics[id - 4]
					assert content['uri']!.str() == 'v://help/${topic}', line
					paths := os.walk_ext(os.join_path(@VEXEROOT, 'vlib', 'v', 'help'), '.txt')
					path := paths.filter(os.file_name(it) == '${topic}.txt')[0]
					assert content['text']!.str() == os.read_file(path)!, line
				}
			}
			if cursor == '' { break }
			assert cursor !in cursors, 'resource pagination repeated cursor ${cursor}'
			cursors << cursor
			output = run_help_resource_requests(root,
				'{"jsonrpc":"2.0","id":2,"method":"resources/list","params":{"cursor":${json.encode(cursor)}}}\n',
				read_only)!
		}
		for topic in topics {
			assert 'v://help/${topic}' in listed, listed.str()
		}
		for id in [2, 3, 4, 5, 6, 7, 8, 99] {
			assert id in received, output
		}
	}
}

fn run_help_resource_requests(root string, requests string, read_only bool) !string {
	input := os.join_path(root, 'requests.jsonl')
	os.write_file(input, '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"${mcp.protocol_version}","capabilities":{},"clientInfo":{"name":"help-resource-test","version":"1"}}}\n' +
		'{"jsonrpc":"2.0","method":"notifications/initialized"}\n' + requests)!
	mut args := ['mcp', 'serve', '--root', root]
	if read_only { args << '--read-only' }
	mut process := os.new_process(@VEXE)
	process.set_args(args)
	process.set_redirect_stdio_merged()
	process.set_stdin_path(input)
	process.run()
	output := process.stdout_slurp()
	process.wait()
	code := process.code
	process.close()
	assert code == 0, output
	return output
}
