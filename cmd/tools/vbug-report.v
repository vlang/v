module main

import db.sqlite
import flag
import json2
import os
import rand
import time
import vbugreport
import veb

const default_port = 8080
const max_report_body_bytes = 256 * 1024

struct ReportLine {
pub:
	line int
	text string
}

struct BugReport {
pub:
	kind           string
	v_version      string
	target_os      string
	target_backend string
	arch           string
	ccompiler      string
	build_options  string
	c_error        string
	c_file         string
	c_line         int
	c_context      []ReportLine
	v_file         string
	v_line         int
	v_context      []ReportLine
	v_source       string
}

struct CreateBugReportResponse {
pub:
	id           string
	delete_url   string
	delete_token string
	message      string
}

struct DeleteBugReportResponse {
pub:
	id      string
	deleted bool
	message string
}

struct ServerConfig {
	host       string
	port       int
	db_path    string
	public_url string
}

pub struct App {
pub:
	public_url string
pub mut:
	db sqlite.DB
}

pub struct Context {
	veb.Context
}

fn args_without_command() []string {
	args := os.args#[1..]
	if args.len > 0 && args[0] == 'bug-report' {
		return args[1..]
	}
	return args
}

fn db_path_from_env() string {
	db_path := os.getenv('V_BUG_REPORT_DB')
	if db_path != '' {
		return db_path
	}
	report_dir := os.getenv('V_BUG_REPORT_DIR')
	if report_dir != '' {
		return os.join_path(report_dir, 'bug_reports.sqlite')
	}
	return os.join_path(os.getwd(), 'bug_reports.sqlite')
}

fn report_host_from_env() string {
	host := os.getenv('V_BUG_REPORT_HOST')
	if host != '' {
		return host
	}
	return 'localhost'
}

fn report_port_from_env() int {
	bug_report_port := os.getenv('V_BUG_REPORT_PORT')
	if bug_report_port != '' {
		return bug_report_port.int()
	}
	port := os.getenv('PORT')
	if port != '' {
		return port.int()
	}
	return default_port
}

fn public_url_from_config(flag_url string, host string, port int) string {
	url := if flag_url != '' { flag_url } else { os.getenv('V_BUG_REPORT_PUBLIC_URL') }
	if url != '' {
		return url.trim_space().trim_right('/')
	}
	public_host := if host == '' || host == '0.0.0.0' { 'localhost' } else { host }
	return 'http://${public_host}:${port}/bug-report'
}

fn config_from_args() !ServerConfig {
	mut fp := flag.new_flag_parser(args_without_command())
	fp.application('v bug-report')
	fp.version('0.0.1')
	fp.description('Run the V compiler bug report receiver.')
	fp.arguments_description('')
	show_help := fp.bool('help', `h`, false, 'Show this help screen.')
	host := fp.string('host', 0, report_host_from_env(), 'Host to listen on.')
	port := fp.int('port', `p`, report_port_from_env(), 'Port to listen on.')
	db_path := fp.string('db', 0, db_path_from_env(), 'SQLite database path.')
	public_url_arg := fp.string('public-url', 0, '', 'Public /bug-report endpoint URL.')
	if show_help {
		println(fp.usage())
		exit(0)
	}
	remaining := fp.finalize()!
	if remaining.len > 0 {
		return error('unexpected arguments: ${remaining.join(' ')}')
	}
	return ServerConfig{
		host:       host
		port:       port
		db_path:    db_path
		public_url: public_url_from_config(public_url_arg, host, port)
	}
}

fn safe_id(id string) bool {
	if id.len < 8 || id.len > 80 {
		return false
	}
	for ch in id {
		if !ch.is_letter() && !ch.is_digit() && ch != `-` && ch != `_` {
			return false
		}
	}
	return true
}

fn new_report_id() string {
	return rand.uuid_v4()
}

fn (app App) delete_url(id string, token string) string {
	return '${app.public_url}/${id}?token=${token}'
}

fn delete_token_from_request(ctx Context) string {
	if token := ctx.query['token'] {
		return token
	}
	return ctx.get_custom_header('X-Delete-Token') or { '' }
}

fn ensure_db_parent(db_path string) ! {
	if db_path == ':memory:' || db_path.starts_with('file:') {
		return
	}
	parent_dir := os.dir(db_path)
	if parent_dir != '' && parent_dir != '.' {
		os.mkdir_all(parent_dir)!
	}
}

fn connect_db(db_path string) !sqlite.DB {
	ensure_db_parent(db_path)!
	mut db := sqlite.connect_full(db_path, [.readwrite, .create, .fullmutex], '')!
	db.busy_timeout(3000)
	return db
}

fn init_db(db sqlite.DB) ! {
	db.exec("create table if not exists bug_reports (
			id text primary key,
			delete_token text not null,
			created_at text not null,
			remote_ip text not null,
				user_agent text not null,
				c_file_name text not null,
				target_os text not null,
				ccompiler text not null,
				error_string text not null,
				lines text not null,
				v_lines text not null default '',
				arch text not null default '',
				build_options text not null default '',
				v_source text not null default '',
				v_version text not null default ''
			)")!
	// Add columns that were introduced after the table already existed, so older
	// databases pick them up without losing their stored reports.
	ensure_column(db, 'bug_reports', 'v_lines', "text not null default ''")!
	ensure_column(db, 'bug_reports', 'arch', "text not null default ''")!
	ensure_column(db, 'bug_reports', 'build_options', "text not null default ''")!
	ensure_column(db, 'bug_reports', 'v_source', "text not null default ''")!
	ensure_column(db, 'bug_reports', 'v_version', "text not null default ''")!
	db.exec('create index if not exists idx_bug_reports_created_at
			on bug_reports(created_at)')!
}

// ensure_column adds `column` to `table` when it is not present yet. SQLite has no
// `add column if not exists`, so the existing columns are inspected first.
fn ensure_column(db sqlite.DB, table string, column string, definition string) ! {
	rows := db.exec('pragma table_info(${table})')!
	for row in rows {
		// pragma table_info columns are: cid, name, type, notnull, dflt_value, pk
		if row.vals.len > 1 && row.vals[1] == column {
			return
		}
	}
	db.exec('alter table ${table} add column ${column} ${definition}')!
}

// create stores a compiler bug report and returns its deletion token.
@['/bug-report'; post]
pub fn (mut app App) create(mut ctx Context) veb.Result {
	if ctx.req.data.len == 0 {
		return ctx.request_error('empty report body')
	}
	if ctx.req.data.len > max_report_body_bytes {
		ctx.res.set_status(.bad_request)
		return ctx.text('report body is too large')
	}
	report := json2.decode[BugReport](ctx.req.data) or {
		return ctx.request_error('invalid report JSON: ${err}')
	}
	if report.kind != 'v-c-compiler-error' {
		return ctx.request_error('unsupported report kind')
	}
	stored_report := vbugreport.new_stored_c_error_report(report.c_file, report.target_os,
		report.ccompiler, report.v_version, report.arch, report.build_options, report.c_error,
		report.c_context.map(it.text), report.v_context.map(it.text), report.v_source)
	id := new_report_id()
	delete_token := rand.uuid_v4()
	app.db.exec_param_many('insert into bug_reports (
				id, delete_token, created_at, remote_ip, user_agent,
				c_file_name, target_os, ccompiler, error_string, lines, v_lines,
				arch, build_options, v_source, v_version
			) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)', [
		id,
		delete_token,
		time.utc().format_rfc3339(),
		ctx.ip(),
		ctx.user_agent(),
		stored_report.c_file_name,
		stored_report.target_os,
		stored_report.ccompiler,
		stored_report.error_string,
		stored_report.lines,
		stored_report.v_lines,
		stored_report.arch,
		stored_report.build_options,
		stored_report.v_source,
		stored_report.v_version,
	]) or { return ctx.server_error('could not store report') }
	return ctx.json(CreateBugReportResponse{
		id:           id
		delete_url:   app.delete_url(id, delete_token)
		delete_token: delete_token
		message:      'created'
	})
}

// delete removes a compiler bug report when the deletion token matches.
@['/bug-report/:id'; delete]
pub fn (mut app App) delete(mut ctx Context, id string) veb.Result {
	if !safe_id(id) {
		return ctx.request_error('invalid report id')
	}
	rows := app.db.exec_param('select delete_token from bug_reports where id = ?', id) or {
		return ctx.server_error('could not read report')
	}
	if rows.len == 0 {
		return ctx.not_found()
	}
	if delete_token_from_request(ctx) != rows[0].vals[0] {
		ctx.res.set_status(.forbidden)
		return ctx.text('invalid delete token')
	}
	app.db.exec_param('delete from bug_reports where id = ?', id) or {
		return ctx.server_error('could not delete report')
	}
	return ctx.json(DeleteBugReportResponse{
		id:      id
		deleted: true
		message: 'deleted'
	})
}

// healthz returns a basic readiness response for local checks.
@['/healthz'; get]
pub fn (mut app App) healthz(mut ctx Context) veb.Result {
	return ctx.text('ok')
}

fn main() {
	config := config_from_args() or {
		eprintln('v bug-report: ${err}')
		exit(1)
	}
	mut db := connect_db(config.db_path) or { panic(err) }
	init_db(db) or { panic(err) }
	mut app := &App{
		public_url: config.public_url
		db:         db
	}
	veb.run_at[App, Context](mut app, host: config.host, port: config.port, family: .ip) or {
		panic(err)
	}
}
