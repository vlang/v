module main

import json2 as json
import mcp
import os
import time

fn test_run_receives_stdin_eof_without_hanging_the_mcp_server() {
	root := os.join_path(os.vtmp_dir(), 'mcp stdin EOF review ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main\nimport os\nfn main() {\n\tprintln("read: " + os.input(""))\n}\n')!
	// Build the server before the watchdog: the `v mcp` wrapper may rebuild a
	// cached tool before dispatching a request, even after a warm-up invocation.
	ext := $if windows { '.exe' } $else { '' }
	server := os.join_path(root, 'vmcp' + ext)
	build := os.exec([@VEXE, '-o', server, os.join_path(@VEXEROOT, 'cmd', 'tools', 'vmcp')])
	assert build.exit_code == 0, build.output
	input := os.join_path(root, 'requests.jsonl')
	os.write_file(input, '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"${mcp.protocol_version}","capabilities":{},"clientInfo":{"name":"stdin-eof-test","version":"1"}}}\n' +
		'{"jsonrpc":"2.0","method":"notifications/initialized"}\n' +
		'{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"v_run","arguments":{"target":"main.v"}}}\n')!
	mut process := os.new_process(server)
	process.set_args(['serve', '--root', root])
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
