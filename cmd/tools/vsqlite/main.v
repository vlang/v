module main

import db.sqlite
import os
import readline
import time

const version = '0.1.0'

const help_text = 'vsqlite ${version} - SQLite CLI written in V

Usage:
  v sqlite <database>               Open database in interactive mode
  v sqlite <database> <stmt>        Execute a single SQL statement
  v sqlite <database> -f <file>     Execute SQL from a file
  v sqlite --help                   Show this help
  v sqlite --version                Show version

Interactive mode commands:
  .tables                          List all tables
  .schema [table]                  Show schema for table(s)
  .mode [table|csv|line|box|       Change output mode (default: table)
        markdown|json|html|
        insert [tbl]|quote]
  .headers [on|off]                Toggle column headers
  .nullvalue [str]                 Set NULL display string (default: NULL)
  .separator [sep]                 Set column separator (default: ,)
  .width [N1 N2 ...]               Set per-column widths (0 = reset to auto)
  .output [file|-]                 Redirect output to file (no arg = stdout)
  .once <file>                     Redirect next query result to file
  .timer [on|off]                  Toggle query execution timing (default: off)
  .explain <stmt>                  Show EXPLAIN QUERY PLAN tree for a statement
  .import <file> <table>           Import CSV/TSV into table (auto-detects .tsv/.tab)
  .export <file>                   Export last query to CSV
  .dump [file]                     Dump full SQL schema+data to stdout or file
  .load <file> [entry]             Load a SQLite extension (.so/.dylib)
  .bail [on|off]                   Stop on first error (default: off)
  .echo [on|off]                   Echo each statement before executing (default: off)
  .log [file|off]                  Log all statements to a file; off to disable
  .changes [on|off]                Show row-change count after each statement (default: off)
  .open <file>                     Open (or switch to) a different database file
  .databases                       List attached databases
  .indexes [table]                 List indexes
  .size                            Show database file size
  .read <file>                     Execute SQL file from within the REPL
  .show                            Print snapshot of all current settings
  .print [string...]               Print literal text (blank line if no args)
  .prompt MAIN [CONTINUE]          Set REPL prompt strings
  .eqp [on|off]                    Auto-EXPLAIN QUERY PLAN before each SELECT
  .trace [file|stderr|off]         Trace all SQL statements as they execute
  .timeout <ms>                    Set busy-wait timeout (milliseconds)
  .shell <cmd> / .system <cmd>     Run an OS command
  .backup <file>                   Backup database using VACUUM INTO
  .fullschema                      Full schema including sqlite_stat tables
  .dbinfo                          Database file info (page count, encoding, etc.)
  .stats                           Page and table statistics
  .lint                            Report missing indexes on FK columns
  .cd [directory]                  Change or show working directory
  .quit / .exit / Ctrl+D           Exit'

const history_file = os.join_path(os.home_dir(), '.vsqlite_history')

struct App {
mut:
	db           sqlite.DB
	rl           readline.Readline
	mode         OutputMode = .table
	headers      bool       = true
	last_rows    []sqlite.Row
	nullvalue    string = 'NULL'
	separator    string = ','
	col_widths   map[int]int
	output_path  string
	output_once  bool
	insert_table string = 'tbl'
	timer        bool
	bail         bool
	echo         bool
	log_path     string
	changes      bool
	db_path      string
	eqp          bool
	trace_path   string
	prompt       string = 'vsqlite> '
	prompt2      string = '     ...> '
}

fn main() {
	// When launched via `v sqlite`, os.args[1] is the subcommand name 'sqlite'.
	// Skip it so the remaining args start with the database path.
	raw := os.args[1..]
	args := if raw.len > 0 && raw[0] == 'sqlite' { raw[1..] } else { raw }

	if args.len == 0 || args[0] in ['--help', '-h'] {
		println(help_text)
		return
	}

	if args[0] in ['--version', '-v'] {
		println('vsqlite ${version}')
		return
	}

	mut db := sqlite.connect(args[0]) or {
		eprintln('Error: cannot open "${args[0]}": ${err}')
		exit(1)
	}
	db.exec_none('PRAGMA journal_mode=WAL')
	db.exec_none('PRAGMA foreign_keys=ON')

	mut app := App{
		db:      db
		db_path: args[0]
	}

	if args.len == 1 {
		app.interactive_mode()
	} else if args[1] == '-f' {
		if args.len < 3 {
			eprintln('Error: -f requires a file path')
			exit(1)
		}
		app.exec_file(args[2])
	} else {
		cmd := args[1..].join(' ')
		if cmd.starts_with('.') {
			app.dot_cmd(cmd)
		} else {
			app.run(cmd)
		}
	}
}

fn (mut app App) interactive_mode() {
	println('vsqlite ${version} - Type .help for commands, .quit to exit')
	println('')
	app.rl.skip_empty = true
	app.load_history()
	app.refresh_completions()
	mut buf := []string{}
	for {
		prompt := if buf.len == 0 { app.prompt } else { app.prompt2 }
		line := app.rl.read_line(prompt) or {
			if buf.len > 0 {
				eprintln('Incomplete statement discarded.')
			}
			break
		}
		trimmed := line.trim_space()
		if trimmed == '' {
			continue
		}
		if buf.len == 0 {
			if trimmed in ['.quit', '.exit', 'quit', 'exit'] {
				break
			}
			if trimmed.starts_with('.') {
				app.dot_cmd(trimmed)
				continue
			}
		}
		buf << line
		if stmt_complete(buf) {
			full := buf.join('\n').trim_space()
			for s in split_statements(full) {
				if s != '' {
					app.run(s)
				}
			}
			buf = []string{}
		}
	}
	app.save_history()
	println('\nBye!')
}

fn stmt_complete(lines []string) bool {
	return lines.join('\n').trim_space().ends_with(';')
}

fn (mut app App) load_history() {
	history_load(mut app.rl, history_file)
}

fn (app App) save_history() {
	history_save(app.rl, history_file)
}

fn history_load(mut rl readline.Readline, path string) {
	content := os.read_file(path) or { return }
	for line in content.split('\n') {
		if line.len > 0 {
			rl.previous_lines << line.runes()
		}
	}
}

fn history_save(rl readline.Readline, path string) {
	lines := rl.previous_lines.map(it.string()).filter(it.len > 0)
	os.write_file(path, lines.join('\n') + '\n') or {}
}

fn (app App) make_format_opts() FormatOptions {
	return FormatOptions{
		mode:       app.mode
		headers:    app.headers
		nullvalue:  app.nullvalue
		separator:  app.separator
		col_widths: app.col_widths
		table_name: app.insert_table
	}
}

fn (mut app App) write_out(s string) {
	if app.output_path != '' {
		os.write_file(app.output_path, os.read_file(app.output_path) or { '' } + s + '\n') or {
			eprintln('Error: cannot write to "${app.output_path}": ${err}')
		}
	} else {
		println(s)
	}
}

fn (mut app App) finish_output() {
	if app.output_once {
		app.output_path = ''
		app.output_once = false
	}
}

fn (mut app App) log_stmt(stmt string) {
	if app.log_path == '' {
		return
	}
	existing := os.read_file(app.log_path) or { '' }
	os.write_file(app.log_path, existing + stmt + ';\n') or {}
}

fn (mut app App) trace_stmt(stmt string) {
	if app.trace_path == '' {
		return
	}
	if app.trace_path == 'stderr' {
		eprintln(stmt)
		return
	}
	existing := os.read_file(app.trace_path) or { '' }
	os.write_file(app.trace_path, existing + stmt + ';\n') or {}
}

fn (mut app App) run(stmt string) bool {
	if app.echo {
		println(stmt)
	}
	app.log_stmt(stmt)
	app.trace_stmt(stmt)
	upper := stmt.trim_space().to_upper()
	is_query := upper.starts_with('SELECT') || upper.starts_with('PRAGMA')
		|| upper.starts_with('EXPLAIN') || upper.starts_with('WITH') || upper.starts_with('VALUES')

	if app.eqp && is_query && !upper.starts_with('EXPLAIN') {
		app.run_explain(stmt)
	}

	t0 := time.now()

	if is_query {
		rows := app.db.exec(stmt) or {
			eprintln('Error: ${err}')
			if app.bail {
				exit(1)
			}
			return false
		}
		elapsed := time.since(t0)
		if rows.len == 0 {
			app.finish_output()
			if app.timer {
				println('Run time: ${format_duration(elapsed)}')
			}
			return true
		}
		app.last_rows = rows
		app.write_out(format_opts(rows, app.make_format_opts()))
		app.write_out('(${rows.len} row${if rows.len == 1 { '' } else { 's' }})')
		app.finish_output()
		if app.timer {
			println('Run time: ${format_duration(elapsed)}')
		}
	} else {
		code := app.db.exec_none(stmt)
		if code != 101 && code != 100 {
			// sqlite_done=101, sqlite_row=100; anything else is an error
			eprintln('Error: exec failed (code ${code})')
			if app.bail {
				exit(1)
			}
			return false
		}
		elapsed := time.since(t0)
		affected := app.db.get_affected_rows_count()
		last_id := app.db.last_insert_rowid()
		if upper.starts_with('INSERT') {
			println('Inserted 1 row (rowid: ${last_id})')
		} else if upper.starts_with('UPDATE') || upper.starts_with('DELETE') {
			println('${affected} row${if affected == 1 { '' } else { 's' }} affected')
		} else {
			println('OK')
		}
		if app.changes {
			println('Changes: ${affected}')
		}
		if app.timer {
			println('Run time: ${format_duration(elapsed)}')
		}
		if upper.starts_with('CREATE') || upper.starts_with('DROP') || upper.starts_with('ALTER') {
			app.refresh_completions()
		}
	}
	return true
}

fn (mut app App) exec_file(path string) {
	content := os.read_file(path) or {
		eprintln('Error reading "${path}": ${err}')
		exit(1)
	}
	mut count := 0
	for stmt in split_statements(content) {
		trimmed := stmt.trim_space()
		// Strip comment-only lines to decide if the chunk has real SQL,
		// but pass the original (with comments) to SQLite so it can parse them.
		sql_content :=
			trimmed.split('\n').filter(!it.trim_space().starts_with('--')).join('\n').trim_space()
		if sql_content == '' {
			continue
		}
		app.run(trimmed)
		count++
	}
	println('Executed ${count} statement(s) from ${path}')
}

fn (mut app App) read_file_repl(path string) {
	content := os.read_file(path) or {
		eprintln('Error: cannot read "${path}": ${err}')
		return
	}
	mut count := 0
	for stmt in split_statements(content) {
		trimmed := stmt.trim_space()
		sql_content :=
			trimmed.split('\n').filter(!it.trim_space().starts_with('--')).join('\n').trim_space()
		if sql_content == '' {
			continue
		}
		app.run(trimmed)
		count++
	}
	println('Executed ${count} statement(s) from ${path}')
}

fn (mut app App) dot_cmd(cmd string) {
	parts := cmd.split(' ').filter(it != '')
	match parts[0] {
		'.help' {
			println(help_text)
		}
		'.tables' {
			tables := app.db.tables() or {
				eprintln('Error: ${err}')
				return
			}
			if tables.len == 0 {
				println('(no tables)')
			} else {
				println(tables.join('\n'))
			}
		}
		'.schema' {
			table := if parts.len > 1 { parts[1] } else { '' }
			result := app.db.schema(table) or {
				eprintln('Error: ${err}')
				return
			}
			if result == '' {
				println('(no schema found)')
			} else {
				println(result)
			}
		}
		'.mode' {
			if parts.len < 2 {
				println('Current mode: ${app.mode}')
				return
			}
			match parts[1] {
				'table' {
					app.mode = .table
				}
				'csv' {
					app.mode = .csv
				}
				'line' {
					app.mode = .line
				}
				'box' {
					app.mode = .box
				}
				'markdown' {
					app.mode = .markdown
				}
				'json' {
					app.mode = .json
				}
				'html' {
					app.mode = .html
				}
				'insert' {
					app.mode = .insert
					if parts.len > 2 {
						app.insert_table = parts[2]
					}
				}
				'quote' {
					app.mode = .quote
				}
				else {
					eprintln('Unknown mode: ${parts[1]}. Use: table, csv, line, box, markdown, json, html, insert [tbl], quote')
					return
				}
			}

			println('Mode set to: ${app.mode}')
		}
		'.headers' {
			if parts.len < 2 {
				println('Headers: ${if app.headers { 'on' } else { 'off' }}')
				return
			}
			match parts[1] {
				'on' { app.headers = true }
				'off' { app.headers = false }
				else { eprintln('Use: .headers on|off') }
			}
		}
		'.nullvalue' {
			if parts.len < 2 {
				println('NULL display: "${app.nullvalue}"')
				return
			}
			app.nullvalue = parts[1]
			println('NULL value set to "${app.nullvalue}"')
		}
		'.separator' {
			if parts.len < 2 {
				println('Separator: "${app.separator}"')
				return
			}
			mut sep := parts[1]
			sep = sep.replace('\\t', '\t').replace('\\n', '\n')
			app.separator = sep
			println('Separator set to "${app.separator}"')
		}
		'.width' {
			if parts.len < 2 {
				if app.col_widths.len == 0 {
					println('Column widths: auto')
				} else {
					mut pairs := []string{}
					for k, v in app.col_widths {
						pairs << 'col${k}=${v}'
					}
					println('Column widths: ${pairs.join(', ')}')
				}
				return
			}
			if parts.len == 2 && parts[1] == '0' {
				app.col_widths = map[int]int{}
				println('Column widths reset to auto')
				return
			}
			app.col_widths = map[int]int{}
			for i, part in parts[1..] {
				w := part.int()
				if w > 0 {
					app.col_widths[i] = w
				}
			}
			println('Column widths set')
		}
		'.output' {
			if parts.len < 2 || parts[1] in ['stdout', '-'] {
				app.output_path = ''
				app.output_once = false
				println('Output reset to stdout')
			} else {
				os.write_file(parts[1], '') or {
					eprintln('Error: cannot open "${parts[1]}": ${err}')
					return
				}
				app.output_path = parts[1]
				app.output_once = false
				println('Output redirected to ${parts[1]}')
			}
		}
		'.once' {
			if parts.len < 2 {
				eprintln('Usage: .once <file>')
				return
			}
			os.write_file(parts[1], '') or {
				eprintln('Error: cannot open "${parts[1]}": ${err}')
				return
			}
			app.output_path = parts[1]
			app.output_once = true
		}
		'.timer' {
			if parts.len < 2 {
				println('Timer: ${if app.timer { 'on' } else { 'off' }}')
				return
			}
			match parts[1] {
				'on' {
					app.timer = true
					println('Timer: on')
				}
				'off' {
					app.timer = false
					println('Timer: off')
				}
				else {
					eprintln('Use: .timer on|off')
				}
			}
		}
		'.explain' {
			if parts.len < 2 {
				eprintln('Usage: .explain <sql statement>')
				return
			}
			stmt := parts[1..].join(' ')
			app.run_explain(stmt)
		}
		'.import' {
			if parts.len < 3 {
				eprintln('Usage: .import <file> <table>')
				return
			}
			app.import_csv(parts[1], parts[2])
		}
		'.export' {
			if parts.len < 2 {
				eprintln('Usage: .export <file>')
				return
			}
			if app.last_rows.len == 0 {
				eprintln('No previous query results to export')
				return
			}
			write_csv(parts[1], app.last_rows, app.headers) or {
				eprintln('Error: ${err}')
				return
			}
			println('Exported ${app.last_rows.len} rows to ${parts[1]}')
		}
		'.dump' {
			text := dump_database(mut app.db)
			if parts.len > 1 {
				os.write_file(parts[1], text) or {
					eprintln('Error: cannot write "${parts[1]}": ${err}')
					return
				}
				println('Dumped to ${parts[1]}')
			} else {
				println(text)
			}
		}
		'.load' {
			if parts.len < 2 {
				eprintln('Usage: .load <file> [entry-point]')
				return
			}
			entry := if parts.len > 2 { parts[2] } else { '' }
			ep := if entry != '' { ", '${entry.replace("'", "''")}'" } else { '' }
			escaped_path := parts[1].replace("'", "''")
			_ = app.db.exec("SELECT load_extension('${escaped_path}' ${ep})") or {
				eprintln('Error: ${err}')
				return
			}
			println('Extension loaded: ${parts[1]}')
		}
		'.bail' {
			if parts.len < 2 {
				println('Bail: ${if app.bail { 'on' } else { 'off' }}')
				return
			}
			match parts[1] {
				'on' {
					app.bail = true
					println('Bail: on')
				}
				'off' {
					app.bail = false
					println('Bail: off')
				}
				else {
					eprintln('Use: .bail on|off')
				}
			}
		}
		'.echo' {
			if parts.len < 2 {
				println('Echo: ${if app.echo { 'on' } else { 'off' }}')
				return
			}
			match parts[1] {
				'on' {
					app.echo = true
					println('Echo: on')
				}
				'off' {
					app.echo = false
					println('Echo: off')
				}
				else {
					eprintln('Use: .echo on|off')
				}
			}
		}
		'.log' {
			if parts.len < 2 {
				if app.log_path == '' {
					println('Log: off')
				} else {
					println('Log: ${app.log_path}')
				}
				return
			}
			if parts[1] == 'off' {
				app.log_path = ''
				println('Logging off')
			} else {
				app.log_path = parts[1]
				println('Logging to ${app.log_path}')
			}
		}
		'.changes' {
			if parts.len < 2 {
				println('Changes: ${if app.changes { 'on' } else { 'off' }}')
				return
			}
			match parts[1] {
				'on' {
					app.changes = true
					println('Changes: on')
				}
				'off' {
					app.changes = false
					println('Changes: off')
				}
				else {
					eprintln('Use: .changes on|off')
				}
			}
		}
		'.open' {
			if parts.len < 2 {
				eprintln('Usage: .open <database-file>')
				return
			}
			mut new_db := sqlite.connect(parts[1]) or {
				eprintln('Error: cannot open "${parts[1]}": ${err}')
				return
			}
			new_db.exec_none('PRAGMA journal_mode=WAL')
			new_db.exec_none('PRAGMA foreign_keys=ON')
			app.db = new_db
			app.db_path = parts[1]
			app.refresh_completions()
			println('Opened ${parts[1]}')
		}
		'.databases' {
			rows := app.db.exec('PRAGMA database_list') or { return }
			for row in rows {
				println('${row.vals[1]}: ${row.vals[2]}')
			}
		}
		'.indexes' {
			filter := if parts.len > 1 {
				escaped := parts[1].replace("'", "''")
				"AND tbl_name='${escaped}'"
			} else {
				''
			}
			rows := app.db.exec("SELECT name, tbl_name FROM sqlite_master WHERE type='index' ${filter} ORDER BY tbl_name, name") or {
				return
			}
			if rows.len == 0 {
				println('(no indexes found)')
			} else {
				for row in rows {
					println('${row.vals[1]}.${row.vals[0]}')
				}
			}
		}
		'.size' {
			sz := app.db.db_size() or {
				eprintln('Error: ${err}')
				return
			}
			println('Database size: ${format_bytes(sz)}')
		}
		'.read' {
			if parts.len < 2 {
				eprintln('Usage: .read <file>')
				return
			}
			app.read_file_repl(parts[1])
		}
		'.show' {
			log_disp := if app.log_path == '' { 'off' } else { app.log_path }
			trace_disp := if app.trace_path == '' { 'off' } else { app.trace_path }
			output_disp := if app.output_path == '' { 'stdout' } else { app.output_path }
			println('       bail: ${if app.bail { 'on' } else { 'off' }}')
			println('    changes: ${if app.changes { 'on' } else { 'off' }}')
			println('       echo: ${if app.echo { 'on' } else { 'off' }}')
			println('        eqp: ${if app.eqp { 'on' } else { 'off' }}')
			println('    headers: ${if app.headers { 'on' } else { 'off' }}')
			println('   log_path: ${log_disp}')
			println('       mode: ${app.mode}')
			println('  nullvalue: "${app.nullvalue}"')
			println('     output: ${output_disp}')
			println('     prompt: "${app.prompt}"')
			println('  separator: "${app.separator}"')
			println('      timer: ${if app.timer { 'on' } else { 'off' }}')
			println('      trace: ${trace_disp}')
		}
		'.print' {
			if parts.len < 2 {
				println('')
			} else {
				println(parts[1..].join(' '))
			}
		}
		'.prompt' {
			if parts.len < 2 {
				eprintln('Usage: .prompt MAIN [CONTINUE]')
				return
			}
			app.prompt = parts[1]
			if parts.len > 2 {
				app.prompt2 = parts[2]
			}
		}
		'.eqp' {
			if parts.len < 2 {
				println('EQP: ${if app.eqp { 'on' } else { 'off' }}')
				return
			}
			match parts[1] {
				'on' {
					app.eqp = true
					println('EQP: on')
				}
				'off' {
					app.eqp = false
					println('EQP: off')
				}
				'full' {
					app.eqp = true
					println('EQP: on (full)')
				}
				else {
					eprintln('Use: .eqp on|off')
				}
			}
		}
		'.trace' {
			if parts.len < 2 || parts[1] == 'off' {
				app.trace_path = ''
				println('Tracing off')
			} else if parts[1] == 'stderr' {
				app.trace_path = 'stderr'
				println('Tracing to stderr')
			} else {
				app.trace_path = parts[1]
				println('Tracing to ${app.trace_path}')
			}
		}
		'.timeout' {
			if parts.len < 2 {
				eprintln('Usage: .timeout <ms>')
				return
			}
			ms := parts[1].int()
			app.db.busy_timeout(ms)
			println('Timeout: ${ms}ms')
		}
		'.shell', '.system' {
			if parts.len < 2 {
				eprintln('Usage: ${parts[0]} <command>')
				return
			}
			shell_cmd := parts[1..].join(' ')
			result := os.execute(shell_cmd)
			if result.output.len > 0 {
				print(result.output)
				if !result.output.ends_with('\n') {
					println('')
				}
			}
			if result.exit_code != 0 {
				eprintln('Exit code: ${result.exit_code}')
			}
		}
		'.backup' {
			if parts.len < 2 {
				eprintln('Usage: .backup <file>')
				return
			}
			escaped := parts[1].replace("'", "''")
			app.db.exec("VACUUM INTO '${escaped}'") or {
				eprintln('Error: ${err}')
				return
			}
			println('Backed up to ${parts[1]}')
		}
		'.fullschema' {
			full_schema := app.db.schema('') or {
				eprintln('Error: ${err}')
				return
			}
			if full_schema == '' {
				println('(no schema found)')
			} else {
				println(full_schema)
			}
			for stat_tbl in ['sqlite_stat1', 'sqlite_stat2', 'sqlite_stat3', 'sqlite_stat4'] {
				rows := app.db.exec('SELECT * FROM ${stat_tbl}') or { continue }
				if rows.len == 0 {
					continue
				}
				println('')
				println('/* contents of ${stat_tbl} */')
				for row in rows {
					println(row.vals.join('|'))
				}
			}
		}
		'.dbinfo' {
			db_list_rows := app.db.exec('PRAGMA database_list') or { []sqlite.Row{} }
			mut filename := app.db_path
			for row in db_list_rows {
				if row.vals.len > 1 && row.vals[1] == 'main' {
					filename = if row.vals.len > 2 { row.vals[2] } else { app.db_path }
					break
				}
			}
			ver_rows := app.db.exec('SELECT sqlite_version()') or { []sqlite.Row{} }
			sqlite_ver := if ver_rows.len > 0 && ver_rows[0].vals.len > 0 {
				ver_rows[0].vals[0]
			} else {
				'unknown'
			}
			pc_rows := app.db.exec('PRAGMA page_count') or { []sqlite.Row{} }
			ps_rows := app.db.exec('PRAGMA page_size') or { []sqlite.Row{} }
			enc_rows := app.db.exec('PRAGMA encoding') or { []sqlite.Row{} }
			jm_rows := app.db.exec('PRAGMA journal_mode') or { []sqlite.Row{} }
			fl_rows := app.db.exec('PRAGMA freelist_count') or { []sqlite.Row{} }
			ai_rows := app.db.exec('PRAGMA application_id') or { []sqlite.Row{} }
			uv_rows := app.db.exec('PRAGMA user_version') or { []sqlite.Row{} }
			page_count := if pc_rows.len > 0 { pc_rows[0].vals[0] } else { '0' }
			page_size := if ps_rows.len > 0 { ps_rows[0].vals[0] } else { '0' }
			encoding := if enc_rows.len > 0 { enc_rows[0].vals[0] } else { 'unknown' }
			journal_mode := if jm_rows.len > 0 { jm_rows[0].vals[0] } else { 'unknown' }
			freelist_count := if fl_rows.len > 0 { fl_rows[0].vals[0] } else { '0' }
			application_id := if ai_rows.len > 0 { ai_rows[0].vals[0] } else { '0' }
			user_version := if uv_rows.len > 0 { uv_rows[0].vals[0] } else { '0' }
			file_size := page_count.i64() * page_size.i64()
			println('       filename: ${filename}')
			println(' sqlite_version: ${sqlite_ver}')
			println('     page_count: ${page_count}')
			println('      page_size: ${page_size}')
			println('      file_size: ${format_bytes(file_size)}')
			println('       encoding: ${encoding}')
			println('   journal_mode: ${journal_mode}')
			println(' freelist_count: ${freelist_count}')
			println(' application_id: ${application_id}')
			println('   user_version: ${user_version}')
		}
		'.stats' {
			pc_rows := app.db.exec('PRAGMA page_count') or { []sqlite.Row{} }
			ps_rows := app.db.exec('PRAGMA page_size') or { []sqlite.Row{} }
			fl_rows := app.db.exec('PRAGMA freelist_count') or { []sqlite.Row{} }
			cs_rows := app.db.exec('PRAGMA cache_size') or { []sqlite.Row{} }
			page_count := if pc_rows.len > 0 { pc_rows[0].vals[0] } else { '0' }
			page_size := if ps_rows.len > 0 { ps_rows[0].vals[0] } else { '0' }
			freelist_count := if fl_rows.len > 0 { fl_rows[0].vals[0] } else { '0' }
			cache_size := if cs_rows.len > 0 { cs_rows[0].vals[0] } else { '0' }
			pc := page_count.i64()
			ps := page_size.i64()
			fl := freelist_count.i64()
			total_size := pc * ps
			used_size := (pc - fl) * ps
			free_size := fl * ps
			println('   page_count: ${page_count}')
			println('    page_size: ${page_size}')
			println('   total_size: ${format_bytes(total_size)}')
			println('    used_size: ${format_bytes(used_size)}')
			println('    free_size: ${format_bytes(free_size)}')
			println('   cache_size: ${cache_size}')
			tables := app.db.tables() or { []string{} }
			if tables.len > 0 {
				println('')
				println('Table row counts:')
				for tbl in tables {
					escaped_tbl := tbl.replace('"', '""')
					cnt_rows := app.db.exec('SELECT COUNT(*) FROM "${escaped_tbl}"') or { continue }
					cnt := if cnt_rows.len > 0 { cnt_rows[0].vals[0] } else { '0' }
					println('  ${tbl}: ${cnt}')
				}
			}
		}
		'.lint' {
			tables := app.db.tables() or { []string{} }
			mut issue_count := 0
			for tbl in tables {
				escaped_tbl := tbl.replace('"', '""')
				fk_rows := app.db.exec('PRAGMA foreign_key_list("${escaped_tbl}")') or { continue }
				if fk_rows.len == 0 {
					continue
				}
				mut indexed_cols := map[string]bool{}
				ti_rows := app.db.exec('PRAGMA table_info("${escaped_tbl}")') or { []sqlite.Row{} }
				for ti_row in ti_rows {
					if ti_row.vals.len > 5 && ti_row.vals[5] != '0' {
						indexed_cols[ti_row.vals[1]] = true
					}
				}
				il_rows := app.db.exec('PRAGMA index_list("${escaped_tbl}")') or { []sqlite.Row{} }
				for il_row in il_rows {
					if il_row.vals.len < 2 {
						continue
					}
					idx_name := il_row.vals[1].replace('"', '""')
					ii_rows := app.db.exec('PRAGMA index_info("${idx_name}")') or { continue }
					for ii_row in ii_rows {
						if ii_row.vals.len > 2 {
							indexed_cols[ii_row.vals[2]] = true
						}
					}
				}
				for fk_row in fk_rows {
					if fk_row.vals.len < 4 {
						continue
					}
					fk_col := fk_row.vals[3]
					ref_table := fk_row.vals[2]
					ref_col := fk_row.vals[4]
					if fk_col !in indexed_cols {
						println('${tbl}: FK column "${fk_col}" -> ${ref_table}(${ref_col}) has no covering index')
						issue_count++
					}
				}
			}
			if issue_count == 0 {
				println('No issues found.')
			} else {
				println('${issue_count} issue${if issue_count == 1 { '' } else { 's' }} found.')
			}
		}
		'.cd' {
			if parts.len < 2 {
				cwd := os.getwd()
				println(cwd)
				return
			}
			os.chdir(parts[1]) or {
				eprintln('Error: cannot change to "${parts[1]}": ${err}')
				return
			}
			println('Changed to ${parts[1]}')
		}
		else {
			eprintln('Unknown command: ${parts[0]}. Type .help for help.')
		}
	}
}

fn (mut app App) import_csv(file string, table string) {
	sep := if file.ends_with('.tsv') || file.ends_with('.tab') { u8(`\t`) } else { u8(`,`) }
	headers, rows := read_csv_sep(file, sep) or {
		eprintln('Error: ${err}')
		return
	}
	col_list := headers.join(',')
	app.db.exec_none('BEGIN TRANSACTION')
	mut imported := 0
	for vals in rows {
		quoted := vals.map("'${it.replace("'", "''")}'")
		code := app.db.exec_none('INSERT INTO ${table} (${col_list}) VALUES (${quoted.join(',')})')
		if code != 101 && code != 100 {
			app.db.exec_none('ROLLBACK')
			eprintln('Error: import failed on row ${imported + 1} (code ${code})')
			return
		}
		imported++
	}
	app.db.exec_none('COMMIT')
	println('Imported ${imported} rows into ${table}')
}

fn (mut app App) refresh_completions() {
	dot_cmds := ['.tables', '.schema', '.mode', '.headers', '.nullvalue', '.separator', '.width',
		'.output', '.once', '.timer', '.explain', '.import', '.export', '.dump', '.load', '.bail',
		'.echo', '.log', '.changes', '.open', '.databases', '.indexes', '.size', '.help', '.quit',
		'.exit', '.read', '.show', '.print', '.prompt', '.eqp', '.trace', '.timeout', '.shell',
		'.system', '.backup', '.fullschema', '.dbinfo', '.stats', '.lint', '.cd']
	kws := ['SELECT', 'FROM', 'WHERE', 'INSERT', 'INTO', 'UPDATE', 'SET', 'DELETE', 'CREATE', 'TABLE',
		'DROP', 'ALTER', 'JOIN', 'LEFT', 'RIGHT', 'INNER', 'OUTER', 'ON', 'ORDER', 'BY', 'GROUP',
		'HAVING', 'LIMIT', 'OFFSET', 'AND', 'OR', 'NOT', 'NULL', 'VALUES', 'PRIMARY', 'KEY',
		'INTEGER', 'TEXT', 'REAL', 'BLOB', 'UNIQUE', 'DEFAULT', 'AUTOINCREMENT', 'PRAGMA', 'INDEX',
		'DISTINCT', 'COUNT', 'SUM', 'MIN', 'MAX', 'AVG', 'AS', 'LIKE', 'IN', 'IS', 'BETWEEN', 'CASE',
		'WHEN', 'THEN', 'ELSE', 'END', 'BEGIN', 'TRANSACTION', 'COMMIT', 'ROLLBACK']
	mut words := []string{}
	words << dot_cmds
	words << kws
	tables := app.db.tables() or { []string{} }
	words << tables
	for t in tables {
		words << (app.db.columns(t) or { []string{} })
	}
	app.rl.completion_callback = make_completer(words)
}

fn make_completer(words []string) fn (string) []string {
	return fn [words] (line string) []string {
		if line.len == 0 {
			return []string{}
		}
		if line.starts_with('.') && !line.contains(' ') {
			return words.filter(it.starts_with(line))
		}
		tok_start := last_token_start(line)
		pre := line[..tok_start]
		tok := line[tok_start..]
		if tok.len == 0 {
			return []string{}
		}
		tok_upper := tok.to_upper()
		mut results := []string{}
		for w in words {
			if w.to_upper().starts_with(tok_upper) {
				results << pre + w
			}
		}
		return results
	}
}

fn last_token_start(line string) int {
	for i := line.len - 1; i >= 0; i-- {
		b := line[i]
		if b == 32 || b == 9 || b == 40 || b == 44 {
			return i + 1
		}
	}
	return 0
}

// dump_database generates a SQL script that recreates the entire database.
fn dump_database(mut db sqlite.DB) string {
	mut lines := []string{}
	lines << 'BEGIN TRANSACTION;'
	lines << ''
	schema_rows := db.exec("SELECT type, name, sql FROM sqlite_master WHERE sql IS NOT NULL AND name NOT LIKE 'sqlite_%' ORDER BY CASE type WHEN 'table' THEN 0 ELSE 1 END, name") or {
		lines << 'COMMIT;'
		return lines.join('\n')
	}
	for row in schema_rows {
		if row.vals[0] != 'table' {
			continue
		}
		obj_name := row.vals[1]
		escaped := obj_name.replace('"', '""')
		lines << row.vals[2] + ';'
		col_rows := db.exec('PRAGMA table_info("${escaped}")') or { continue }
		if col_rows.len == 0 {
			lines << ''
			continue
		}
		col_names := col_rows.map(it.vals[1])
		col_list := col_names.map('"${it.replace('"', '""')}"').join(', ')
		// Use quote() to correctly distinguish NULL from empty string.
		quoted_cols := col_names.map('quote("${it.replace('"', '""')}")').join(', ')
		data_rows := db.exec('SELECT ${quoted_cols} FROM "${escaped}"') or { continue }
		for drow in data_rows {
			lines << 'INSERT INTO "${escaped}"(${col_list}) VALUES(${drow.vals.join(', ')});'
		}
		lines << ''
	}
	for row in schema_rows {
		if row.vals[0] == 'table' {
			continue
		}
		lines << row.vals[2] + ';'
		lines << ''
	}
	lines << 'COMMIT;'
	return lines.join('\n')
}

fn format_bytes(n i64) string {
	if n < 1024 {
		return '${n} B'
	} else if n < 1024 * 1024 {
		return '${n / 1024} KB'
	} else if n < 1024 * 1024 * 1024 {
		return '${n / (1024 * 1024)} MB'
	} else {
		return '${n / (1024 * 1024 * 1024)} GB'
	}
}

fn format_duration(d time.Duration) string {
	us := d.microseconds()
	if us < 1000 {
		return '${us} µs'
	}
	ms := us / 1000
	frac := us % 1000
	frac_str := if frac < 10 {
		'00${frac}'
	} else if frac < 100 {
		'0${frac}'
	} else {
		'${frac}'
	}
	return '${ms}.${frac_str} ms'
}

// split_statements splits SQL on semicolons, respecting quoted strings and
// BEGIN...END blocks (e.g. CREATE TRIGGER bodies).
fn split_statements(src string) []string {
	mut stmts := []string{}
	mut start := 0
	mut in_single := false
	mut in_double := false
	mut begin_depth := 0
	for i := 0; i < src.len; i++ {
		c := src[i]
		if c == `'` && !in_double {
			in_single = !in_single
		} else if c == `"` && !in_single {
			in_double = !in_double
		} else if !in_single && !in_double {
			// Track BEGIN/END nesting for trigger bodies.
			if c == `;` && begin_depth == 0 {
				s := src[start..i].trim_space()
				if s != '' {
					stmts << s
				}
				start = i + 1
			} else if is_keyword_at(src, i, 'BEGIN') {
				// Only count as a trigger-body BEGIN if there is already content
				// in this statement. A standalone 'BEGIN'/'BEGIN TRANSACTION'
				// starts a new statement and must not suppress semicolon splitting.
				if src[start..i].trim_space() != '' {
					begin_depth++
				}
			} else if is_keyword_at(src, i, 'END') && begin_depth > 0 {
				begin_depth--
			}
		}
	}
	s := src[start..].trim_space()
	if s != '' {
		stmts << s
	}
	return stmts
}

// is_keyword_at checks if keyword appears at position i as a whole word
// (not part of a larger identifier).
fn is_keyword_at(src string, i int, keyword string) bool {
	if i + keyword.len > src.len {
		return false
	}
	// Must not be preceded by a word character.
	if i > 0 && is_word_char(src[i - 1]) {
		return false
	}
	// Must not be followed by a word character.
	end := i + keyword.len
	if end < src.len && is_word_char(src[end]) {
		return false
	}
	return src[i..end].to_upper() == keyword
}

fn is_word_char(c u8) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_`
}

fn (mut app App) run_explain(stmt string) {
	rows := app.db.exec('EXPLAIN QUERY PLAN ${stmt}') or {
		eprintln('Error: ${err}')
		return
	}
	if rows.len == 0 {
		println('(no query plan)')
		return
	}
	println('QUERY PLAN')
	app.print_eqp_tree(rows, 0, 0)
}

fn (mut app App) print_eqp_tree(rows []sqlite.Row, parent_id int, depth int) {
	indent := '  '.repeat(depth)
	for row in rows {
		if row.vals.len < 4 {
			continue
		}
		if row.vals[1].int() == parent_id {
			println('${indent}|--${row.vals[3]}')
			app.print_eqp_tree(rows, row.vals[0].int(), depth + 1)
		}
	}
}
