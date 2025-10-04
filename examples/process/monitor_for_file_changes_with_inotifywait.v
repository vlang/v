import os

os.find_abs_path_of_executable('inotifywait') or {
	eprintln('This program uses the inotifywait executable, which is missing.')
	exit(1)
}
mut cmd := os.start_new_command('inotifywait -q -r -m -e move,modify,create,delete .')!
defer { cmd.close() or {} }
for !cmd.eof {
	line := cmd.read_line()
	if line == '' && cmd.eof {
		continue
	}
	eprintln('> notification: ${line}')
}
