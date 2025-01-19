import os
import term
import readline
import net.urllib

fn elog(msg string) {
	eprintln(term.ecolorize(term.gray, msg))
}

fn olog(msg string) {
	println(term.colorize(term.green, msg))
}

fn vversion() string {
	vexe := os.getenv('VEXE')
	return os.execute('${os.quoted_path(vexe)} version').output.trim_space()
}

// get output from `v doctor`
fn get_vdoctor_output(is_verbose bool) string {
	vexe := os.getenv('VEXE')
	verbose_flag := if is_verbose { '-v' } else { '' }
	result := os.execute('${os.quoted_path(vexe)} ${verbose_flag} doctor')
	if result.exit_code != 0 {
		elog('> unable to get `v doctor` output: ${result.output}')
		return ''
	}
	return result.output
}

fn runv(label string, user_cmd string) os.Result {
	mut result := os.Result{}
	elog('> ${label} using: ${term.ecolorize(term.magenta, user_cmd)}')
	result = os.execute(user_cmd)
	print(result.output)
	return result
}

// get output from `./v -g -o vdbg cmd/v && ./vdbg -user_args run file.v`
fn get_v_build_output(is_verbose bool, is_yes bool, file_path string, user_args string, generated_file string) string {
	mut result := os.Result{}
	mut vexe := os.getenv('VEXE')

	// prepare a V compiler with -g to have better backtraces if possible
	wd := os.getwd()
	vroot := @VMODROOT
	os.chdir(vroot) or {}
	verbose_flag := if is_verbose { '-v' } else { '' }
	vdbg_path := $if windows { '${vroot}/vdbg.exe' } $else { '${vroot}/vdbg' }
	vdbg_compilation_cmd := '${os.quoted_path(vexe)} ${verbose_flag} -g -o ${os.quoted_path(vdbg_path)} cmd/v'
	result = runv('Prepare vdbg', vdbg_compilation_cmd)
	os.chdir(wd) or {}

	if result.exit_code == 0 {
		vexe = vdbg_path
	} else {
		elog('> unable to compile V in debug mode: ${result.output}\ncommand: ${vdbg_compilation_cmd}\n')
	}

	result = runv('Compile', '${os.quoted_path(vexe)} ${verbose_flag} ${user_args} ${os.quoted_path(file_path)}')
	defer {
		os.rm(vdbg_path) or {
			if is_verbose {
				elog('> unable to delete `vdbg`: ${err}')
			}
		}
	}
	if result.exit_code == 0 {
		real_generated_file := os.real_path(generated_file)
		defer {
			os.rm(generated_file) or {
				if is_verbose {
					elog('> unable to delete generated file: ${err}')
				}
			}
		}
		run := is_yes
			|| ask('It looks like the compilation went well, do you want to run the file?')
		if run {
			result = runv('Run', real_generated_file)
			if result.exit_code == 0 && !is_yes {
				elog('> The file ran correctly as well.')
				confirm_or_exit('Are you sure you want to continue?')
			}
		}
	}
	return result.output
}

fn ask(msg string) bool {
	prompt := os.input_opt(term.colorize(term.bright_white, '${msg} [Y/n] ')) or { 'y' }
	return prompt == '' || prompt[0].ascii_str().to_lower() != 'n'
}

fn confirm_or_exit(msg string) {
	if !ask(msg) {
		exit(1)
	}
}

fn main() {
	unbuffer_stdout()
	mut compiler_args := []string{}
	mut file_path := ''
	is_verbose := '-v' in os.args
	is_yes := '-y' in os.args

	for arg in os.args[1..] {
		if arg == 'bug' {
			continue
		}
		if arg.ends_with('.v') || arg.ends_with('.vsh') || arg.ends_with('.vv') {
			if file_path != '' {
				elog('> v bug: only one V file can be submitted')
				exit(1)
			}
			file_path = arg
		} else {
			if arg !in ['-y', '-v'] {
				compiler_args << arg
			}
		}
	}

	if file_path == '' {
		elog('> v bug: no v file listed to report')
		exit(1)
	}

	os.unsetenv('VCOLORS')
	// collect error information
	// output from `v doctor`
	vdoctor_output := get_vdoctor_output(is_verbose)
	// file content
	file_content := os.read_file(file_path) or {
		elog('> unable to get file "${file_path}" content: ${err}')
		''
	}

	user_args := compiler_args.join(' ')
	mut generated_file := file_path.all_before_last('.')
	if os.user_os() == 'windows' {
		generated_file += '.exe'
	}
	build_output := get_v_build_output(is_verbose, is_yes, file_path, user_args, generated_file)

	// ask the user if he wants to submit even after an error
	if !is_yes && (vdoctor_output == '' || file_content == '' || build_output == '') {
		elog('> Error while retrieving the information.')
		confirm_or_exit('Do you want to continue?')
	}

	expected_result := readline.read_line('What did you expect to see? ') or {
		// Ctrl-C was pressed
		elog('\nCanceled')
		exit(1)
	}
	// open prefilled issue creation page, or print link as a fallback

	if !is_yes && vdoctor_output.contains('behind V master') {
		olog('> It looks like your installation of V is outdated.')
		olog('> We advise you to run `v up` before submitting an issue.')
		confirm_or_exit('Are you sure you want to continue?')
	}

	// When updating this template, make sure to update `.github/ISSUE_TEMPLATE/bug_report.md` too
	raw_body := '<!-- It is advisable to update all relevant modules using `v outdated` and `v install` -->

<details>
<summary>V version: ${vversion()}, press to see full `v doctor` output</summary>

${vdoctor_output}
</details>

**What did you do?**
`./v -g -o vdbg cmd/v && ./vdbg ${user_args} ${file_path} && ${os.real_path(generated_file)}`
{file_content}

**What did you see?**
```
${build_output}```

**What did you expect to see?**

${expected_result}

'
	mut encoded_body := urllib.query_escape(raw_body.replace_once('{file_content}', '```v\n${file_content}\n```'))
	mut generated_uri := 'https://github.com/vlang/v/issues/new?labels=Bug&body=${encoded_body}'
	if generated_uri.len > 8192 {
		// GitHub doesn't support URLs longer than 8192 characters
		encoded_body = urllib.query_escape(raw_body.replace_once('{file_content}', 'See attached file `${file_path}`'))
		generated_uri = 'https://github.com/vlang/v/issues/new?labels=Bug&body=${encoded_body}'
		elog('> Your file is too big to be submitted.')
		elog('> Go to the following URL, and attach your file:')
		olog(generated_uri)
	} else {
		os.open_uri(generated_uri) or {
			if is_verbose {
				elog(err.str())
			}
			olog(generated_uri)
		}
	}
}
