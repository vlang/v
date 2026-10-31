import os

fn test_exec_captures_each_pipe_chunk_once() {
	result := os.exec(['cmd', '/d', '/c', 'for /L %i in (1,1,2500) do @echo v-output-line'])
	assert result.exit_code == 0, result.output
	assert result.output.replace('\r\n', '\n') == 'v-output-line\n'.repeat(2500)
}

fn test_exec_empty_output_does_not_append_unread_bytes() {
	result := os.exec(['cmd', '/d', '/c', 'exit /b 0'])
	assert result.exit_code == 0, result.output
	assert result.output == ''
}
