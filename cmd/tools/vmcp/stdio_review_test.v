module main

import json2 as json
import mcp
import os
import time

const stdio_review_probe = '--vmcp-test-stdio-serve'

fn testsuite_begin() {
	if os.args.len > 1 && os.args[1] == stdio_review_probe {
		serve(os.args[2..])
		exit(0)
	}
}

fn test_run_receives_stdin_eof_without_hanging_the_mcp_server() {
	root := os.join_path(os.vtmp_dir(), 'mcp stdin EOF review ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main\nimport os\nfn main() {\n\tprintln("read: " + os.input(""))\n}\n')!
	input := os.join_path(root, 'requests.jsonl')
	os.write_file(input, '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"${mcp.protocol_version}","capabilities":{},"clientInfo":{"name":"stdin-eof-test","version":"1"}}}\n' +
		'{"jsonrpc":"2.0","method":"notifications/initialized"}\n' +
		'{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"v_run","arguments":{"target":"main.v"}}}\n')!
	mut process := os.new_process(os.executable())
	process.set_args([stdio_review_probe, '--root', root])
	mut environment := os.environ()
	environment['VEXE'] = @VEXE
	process.set_environment(environment)
	process.set_redirect_stdio_merged()
	process.set_stdin_path(input)
	process.use_pgroup = true
	process.run()
	watch := time.new_stopwatch()
	mut timed_out := false
	for process.is_alive() {
		if watch.elapsed() > 30 * time.second {
			timed_out = true
			process.signal_pgkill()
			break
		}
		time.sleep(10 * time.millisecond)
	}
	process.wait()
	if timed_out {
		process.close()
		assert false, 'stdin read hung for 30 seconds'
		return
	}
	output := process.stdout_slurp()
	exit_code := process.code
	process.close()
	assert exit_code == 0, output
	mut received := false
	for line in output.trim_space().split_into_lines() {
		response := json.decode[map[string]json.Any](line, strict: true)!
		if response['id']!.int() != 2 { continue }
		received = true
		result := response['result']!.as_map()
		content := result['content']!.as_array()[0].as_map()
		payload := json.decode[map[string]json.Any](content['text']!.str(), strict: true)!
		assert payload['exit_code']!.int() == 0, line
		assert payload['output']!.str() == 'read: <EOF>', line
	}
	assert received, output
}

fn test_help_resource_template_topics_can_be_read_over_stdio() {
	root := os.join_path(os.vtmp_dir(), 'mcp help resources review ${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	topics := ['default', 'test', 'fmt', 'build-c', 'install']
	help_paths := os.walk_ext(os.join_path(@VEXEROOT, 'vlib', 'v', 'help'), '.txt')
	mut expected_text := map[string]string{}
	for topic in topics {
		path := help_paths.filter(os.file_name(it) == '${topic}.txt')[0]
		expected_text[topic] = os.read_file(path)!
	}
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
					assert content['text']!.str() == expected_text[topic], line
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
	mut args := [stdio_review_probe, '--root', root]
	if read_only { args << '--read-only' }
	mut process := os.new_process(os.executable())
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
