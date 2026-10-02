// `v skills` installs the agent skills that ship with the V compiler.
//
// A skill is a directory with a `SKILL.md` entry point, the layout coding agents
// already read: opencode, Claude Code and others look for skills under
// `.agents/skills`. A project install lands in the repository and is shared with
// the team; a `--global` install lands in the user's home directory and applies
// to every project on the machine.
//
// Usage:
//   v skills list                    list the bundled catalog and what is installed
//   v skills add <name>              install one bundled skill into this project
//   v skills add <name> --global     install it for the current user
//   v skills remove <name>           uninstall it from this project
//   v skills path <name>             print where a skill is installed
module main

import os
import v.skills

const usage = 'Usage: v skills list [--global]\n' +
	'       v skills add <name> [--global] [--force] [--dry-run]\n' +
	'       v skills remove <name> [--global] [--dry-run]\n' +
	'       v skills path <name> [--global]\n' +
	'\n' +
	'Options:\n' +
	'  --global      use ~/.agents/skills instead of the project directory\n' +
	'  --force       overwrite an already installed skill of the same name\n' +
	'  --dry-run     report what would happen without writing anything\n' +
	'  -h, --help    show this help and exit\n' +
	'\n' +
	'Skills are installed into .agents/skills/<name>/, which the coding agents\n' +
	'read from a project, or ~/.agents/skills/<name>/ with --global.\n'

// Output is what one run reported.
//
// The subcommands build their answer instead of printing it, so the command line
// can be tested in process and so a caller embedding this logic gets the text
// rather than a side effect.
struct Output {
pub mut:
	// lines go to standard output.
	lines []string
	// errors go to standard error.
	errors []string
	// code is the process exit code.
	code int
}

// text is everything the run said, which is what a test asserts on.
pub fn (o Output) text() string {
	mut all := o.lines.clone()
	all << o.errors
	return all.join('\n')
}

// fn main runs the command line and prints what the subcommand reported.
fn main() {
	passed := os.args[1..].filter(it != '--')
	args := if passed.len > 0 && passed[0] == 'skills' { passed[1..] } else { passed }
	if args.len == 0 || args[0] in ['-h', '--help', 'help'] {
		print(usage)
		exit(if args.len == 0 { 1 } else { 0 })
	}
	vroot := find_vroot(os.getenv_opt('VEXE') or { os.executable() }) or {
		find_vroot(@VEXE) or {
			eprintln('v skills: could not find the V source tree from `${os.executable()}`')
			exit(1)
		}
	}
	out := run_at(vroot, os.getwd(), args)
	for line in out.lines {
		println(line)
	}
	for line in out.errors {
		eprintln(line)
	}
	if out.code != 0 && out.errors.len == 0 {
		eprint(usage)
	}
	exit(out.code)
}

// run_at runs one subcommand against the bundled skills in `vroot`, with `base` as
// the project root.
fn run_at(vroot string, base string, args []string) Output {
	// The flags are read from the whole command line, so `--help` works without a
	// subcommand in front of it.
	if args.len > 0 && args[0] in ['-h', '--help', 'help'] {
		return Output{
			lines: usage.split_into_lines()
		}
	}
	opts := parse_options(args[1..], base)
	if opts.help {
		return Output{
			lines: usage.split_into_lines()
		}
	}
	if opts.bad_flag != '' {
		return Output{
			errors: ['v skills: unknown option `${opts.bad_flag}`']
			code:   1
		}
	}
	match args[0] {
		'list' {
			return list(vroot, opts)
		}
		'add' {
			return add(vroot, opts)
		}
		'remove' {
			return remove(opts)
		}
		'path' {
			return path_of(vroot, opts)
		}
		else {
			return Output{
				errors: ['v skills: unknown subcommand `${args[0]}`']
				code:   1
			}
		}
	}
}

// Options are the flags shared by the subcommands.
struct Options {
pub mut:
	// scope is where the skill is installed.
	scope skills.Scope
	// base is the project root, used for a project install.
	base string
	// force overwrites an installed skill.
	force bool
	// dry_run reports without writing.
	dry_run bool
	// names are the skill names the user named.
	names []string
	// help is set when the user asked for the usage text.
	help bool
	// bad_flag holds an unrecognised option, which is an error rather than a
	// silently ignored argument: a typo in `--dry-run` must not become a real
	// install.
	bad_flag string
}

// parse_options reads the flags and the skill names out of `args`.
fn parse_options(args []string, base string) Options {
	mut opts := Options{
		scope: .project_root
		base:  base
	}
	for arg in args {
		match arg {
			'--global' {
				opts.scope = .home_dir
			}
			'--force' {
				opts.force = true
			}
			'--dry-run' {
				opts.dry_run = true
			}
			'-h', '--help' {
				opts.help = true
			}
			else {
				if arg.starts_with('-') {
					if opts.bad_flag == '' {
						opts.bad_flag = arg
					}
					continue
				}
				opts.names << arg
			}
		}
	}
	return opts
}

// list prints the bundled catalog and where each skill stands in both scopes.
fn list(vroot string, opts Options) Output {
	project_dir := skills.target_dir(.project_root, opts.base)
	global_dir := skills.target_dir(.home_dir, '')
	catalog := skills.catalog(vroot)
	if catalog.len == 0 {
		return Output{
			errors: ['v skills: no bundled skills found under `${skills.bundled_root(vroot)}`']
			code:   1
		}
	}
	stale := skills.out_of_date(vroot, project_dir)
	global_stale := skills.out_of_date(vroot, global_dir)
	in_project := skills.installed(project_dir)
	in_global := skills.installed(global_dir)
	mut out := Output{
		lines: ['bundled in ${skills.bundled_root(vroot)}', '']
	}
	for skill in catalog {
		out.lines << skill.name
		out.lines << '	${skill.description}'
		for relative in skill.files {
			out.lines << '	${relative}'
		}
		out.lines << '	status: ${marker(skill.name, in_project, in_global, stale, global_stale)}'
	}
	out.lines << ''
	out.lines << 'project: ${project_dir}${exists_mark(project_dir)}'
	out.lines << 'global:  ${global_dir}${exists_mark(global_dir)}'
	// `catalog` skips what it cannot read, so a bundle whose front matter is broken
	// would otherwise be invisible here. Report it instead: it cannot be installed,
	// and the reason belongs next to the catalog it belongs to.
	for problem in skills.invalid_bundled(vroot) {
		out.errors << 'v skills: cannot install ${problem}'
	}
	if out.errors.len > 0 {
		out.code = 1
	}
	return out
}

// marker says where a skill is installed, so `list` shows one line per skill that
// says what is true rather than making the reader check the paths.
fn marker(name string, in_project []string, in_global []string, stale []string,
	global_stale []string) string {
	mut marks := []string{}
	if name in in_project {
		marks << if name in stale { 'project (out of date)' } else { 'project' }
	}
	if name in in_global {
		marks << if name in global_stale { 'global (out of date)' } else { 'global' }
	}
	if marks.len == 0 {
		return 'not installed'
	}
	return marks.join(', ')
}

// exists_mark annotates a directory that is not there yet.
fn exists_mark(dir string) string {
	return if os.is_dir(dir) { '' } else { ' (missing)' }
}

// add installs one or more bundled skills.
fn add(vroot string, opts Options) Output {
	if opts.names.len == 0 {
		return Output{
			errors: ['v skills: name a skill to add; run `v skills list` to see them']
			code:   1
		}
	}
	dir := target(opts)
	mut out := Output{}
	mut failed := false
	for name in opts.names {
		skill := skills.find(vroot, name) or {
			out.errors << 'v skills: no bundled skill called `${name}`; run `v skills list`'
			failed = true
			continue
		}
		// Reported here rather than left to `install`, so the message names the rule
		// that was broken instead of only saying the install failed.
		skills.validate_bundle(skill.directory) or {
			out.errors << 'v skills: `${name}` cannot be installed: ${err.msg()}'
			failed = true
			continue
		}
		result := skills.install(skill, dir, skills.InstallOptions{
			force:   opts.force
			dry_run: opts.dry_run
		}) or {
			out.errors << 'v skills: could not install `${name}`: ${err.msg()}'
			failed = true
			continue
		}
		out.lines << report_add(result, opts.force)
	}
	if failed {
		out.code = 1
	}
	return out
}

// report_add renders what one install did, saying plainly when nothing was written
// so a skipped skill is not mistaken for an installed one.
fn report_add(result skills.InstallResult, force bool) string {
	if result.dry_run {
		return '${result.skill}: would write ${result.written.len} file(s) to ${result.path}'
	}
	if result.skipped {
		return '${result.skill}: already installed at ${result.path}; pass --force to overwrite'
	}
	verb := if force { 'reinstalled' } else { 'installed' }
	return '${result.skill}: ${verb} ${result.written.len} file(s) at ${result.path}'
}

// remove uninstalls one or more skills.
fn remove(opts Options) Output {
	if opts.names.len == 0 {
		return Output{
			errors: ['v skills: name a skill to remove; run `v skills list` to see them']
			code:   1
		}
	}
	dir := target(opts)
	mut out := Output{}
	mut failed := false
	for name in opts.names {
		if opts.dry_run {
			skills.validate_name(name) or {
				out.errors << 'v skills: could not remove `${name}`: ${err.msg()}'
				failed = true
				continue
			}
			dest := os.join_path_single(dir, name)
			if os.is_link(dest) || (os.is_dir(dest) && os.dir(os.real_path(dest)) != os.real_path(dir)) {
				out.errors << 'v skills: refusing skill path outside its immediate install directory: `${dest}`'
				failed = true
				continue
			}
			out.lines << if os.is_dir(dest) {
				'${name}: would remove ${dest}'
			} else {
				'${name}: not installed in ${dir}'
			}
			continue
		}
		result := skills.remove(dir, name) or {
			out.errors << 'v skills: could not remove `${name}`: ${err.msg()}'
			failed = true
			continue
		}
		if !result.removed {
			out.lines << '${name}: not installed in ${dir}'
			continue
		}
		out.lines << '${name}: removed ${result.path}'
	}
	if failed {
		out.code = 1
	}
	return out
}

// path_of prints where a skill is or would be installed.
fn path_of(vroot string, opts Options) Output {
	if opts.names.len != 1 {
		return Output{
			errors: ['v skills: name exactly one skill']
			code:   1
		}
	}
	name := opts.names[0]
	// The name is checked against the catalog as well, so a typo is reported here
	// instead of producing a path that does not exist.
	if skills.find(vroot, name) == none {
		return Output{
			errors: ['v skills: no bundled skill called `${name}`; run `v skills list`']
			code:   1
		}
	}
	return Output{
		lines: [os.join_path_single(target(opts), name)]
	}
}

// target returns the directory the subcommand installs into.
fn target(opts Options) string {
	return skills.target_dir(opts.scope, opts.base)
}

// find_vroot walks up from an executable to the V source tree it belongs to.
//
// The bundled skills are read from that tree rather than embedded into the
// binary, so a skill can be reviewed and diffed in the repository, and adding one
// needs no rebuild.
fn find_vroot(exe_path string) ?string {
	mut dir := os.dir(os.real_path(exe_path))
	for dir.len > 0 {
		if os.is_file(os.join_path_single(dir, 'v.mod')) && os.is_dir(os.join_path(dir,
			'vlib', 'v', 'skills'))
		{
			return dir
		}
		dir = os.parent_dir(dir)
	}
	return none
}
