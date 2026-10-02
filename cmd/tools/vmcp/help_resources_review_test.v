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
	mut requests := '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"${mcp.protocol_version}","capabilities":{},"clientInfo":{"name":"help-resource-test","version":"1"}}}\n' +
		'{"jsonrpc":"2.0","method":"notifications/initialized"}\n' +
		'{"jsonrpc":"2.0","id":2,"method":"resources/list"}\n' +
		'{"jsonrpc":"2.0","id":3,"method":"resources/templates/list"}\n'
	for i, topic in topics {
		requests += '{"jsonrpc":"2.0","id":${i + 4},"method":"resources/read","params":{"uri":"v://help/${topic}"}}\n'
	}
	requests += '{"jsonrpc":"2.0","id":99,"method":"resources/read","params":{"uri":"v://help/../../outside"}}\n'
	input := os.join_path(root, 'requests.jsonl')
	os.write_file(input, requests)!
	for read_only in [false, true] {
		mut args := ['mcp', 'serve', '--root', root]
		if read_only {
			args << '--read-only'
		}
		mut process := os.new_process(@VEXE)
		process.set_args(args)
		process.set_redirect_stdio_merged()
		process.set_stdin_path(input)
		process.run()
		output := process.stdout_slurp()
		process.wait()
		assert process.code == 0, output
		process.close()
		mut received := []int{}
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
				mut listed := []string{}
				for resource in result['resources']!.as_array() {
					listed << resource.as_map()['uri']!.str()
				}
				for topic in topics {
					assert 'v://help/${topic}' in listed, line
				}
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
		for id in [2, 3, 4, 5, 6, 7, 8, 99] {
			assert id in received, output
		}
	}
}
