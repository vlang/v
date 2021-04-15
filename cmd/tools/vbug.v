import net.urllib
import os
import v.util
import v.pref

fn print_help() {
	// TODO -v (verbose), -h | -help (help), -y (force even if there was an error getting infos)
	println('v bug [options] <file>')
}

// get output from `v doctor`
fn get_vdoctor_output(is_verbose bool) string {
	// ensure vdoctor exists
	tool_exe := util.compile_tool(is_verbose, 'vdoctor') or {
		eprintln('unable to get `v doctor` output: $err')
		return ''
	}
	result := os.execute('"$tool_exe"')
	if result.exit_code != 0 {
		eprintln('unable to get `v doctor` output: $result.output')
		return ''
	}
	return result.output
}

// get ouput from `v -g -o vdbg cmd/v && vdbg file.v`
fn get_v_build_output(is_verbose bool, file_path string) string {
	mut vexe := pref.vexe_path()
	v_dir := os.dir(vexe)
	vdbg_path := $if windows { '${v_dir}/vdbg.exe' } $else { '${v_dir}/vdbg' }
	vdbg_result := os.execute('"$vexe" -g -o "$vdbg_path" ${v_dir}/cmd/v')
	if vdbg_result.exit_code == 0 {
		vexe = vdbg_path
	} else {
		eprintln('unable to compile V in debug mode: $vdbg_result.output')
	}
	build_result := os.execute('"$vexe" "$file_path"')
	return build_result.output
}

// TODO move this to vlib ?
// open a url in the default browser
fn open_url(url string) ? {
	cmd := $if darwin {
		'open "$url"'
	} $else $if windows {
		'explorer "$url"'
	} $else $if linux {
		'xdg-open "$url"'
	} $else {
		'' // TODO Can this happen ?
	}
	if os.system(cmd) != 0 {
		return error('unable to open url `$url`')
	}
}

fn main() {
	mut file_path := ''
	mut is_verbose := false
	mut is_yes := false
	for arg in os.args[2..] {
		match arg {
			'-v' {
				if is_verbose {
					eprintln('duplicated option: `-v`')
				}
				is_verbose = true
			}
			'-y' {
				if is_yes {
					eprintln('duplicated option: `-y`')
				}
				is_yes = true
			}
			'-h', '-help' {
				print_help()
				exit(0)
			}
			else {
				if !arg.ends_with('.v') && !arg.ends_with('.vsh') && !arg.ends_with('.vv') {
					eprintln('unknown argument: `$arg`')
					exit(1)
				}
				if file_path != '' {
					eprintln('only one V file can be submitted')
					exit(1)
				}
				file_path = arg
			}
		}
	}
	if file_path == '' {
		eprintln('invalid use of command `bug`')
		print_help()
		exit(1)
	}
	// collect error information
	// output from `v doctor`
	vdoctor_output := get_vdoctor_output(is_verbose)
	// file content
	file_content := os.read_file(file_path) or {
		eprintln('unable to get file "$file_path" content: $err')
		''
	}
	// output from `v -g -o vdbg cmd/v && vdbg file.v`
	build_output := get_v_build_output(is_verbose, file_path)
	// ask the user if he wants to submit even after an error
	if !is_yes && (vdoctor_output == '' || file_content == '' || build_output == '') {
		prompt := os.input_opt('An error occured retrieving the information, do you want to continue ? [Y/n]') or { 'y' }
		if prompt != '' && prompt[0] == `n` {
			exit(1)
		}
	}
	// open prefilled issue creation page, or print link as a fallback

	// TODO Check that V is up-to-date and remove the relevant message at the start of the template
	// if !is_yes && !is_v_up_to_date() {
	// 	prompt := os.input_opt('It looks like your installation of V is outdated, we advise you to run `v up` before submitting an issue. Are you sure you want to continue ? [Y/n]') or { 'y' }
	// 	if prompt != '' && prompt[0] == `n` {
	// 		exit(1)
	// 	}
	// }

	// When updating this template, make sure to update `.github/ISSUE_TEMPLATE/bug_report.md` too
	encoded_body := urllib.query_escape('<!-- Please make sure to run `v up` before reporting any issues as it may have already been fixed.
     It\'s also advisable to update all relevant modules using `v outdated` and `v install` -->

**V doctor:**
```
$vdoctor_output```

**What did you do?**
`v -g -o vdbg cmd/v && vdbg $file_path`
```v
$file_content
```

**What did you expect to see?**


**What did you see instead?**
```
$build_output```')
	// TODO GitHub probably won't accept URL with infinite size, this must be checked before hand
	// TODO We can probably prefill the title with something too (`&title=`)
	generated_url := 'https://github.com/vlang/v/issues/new?labels=Bug&body=${encoded_body}'
	open_url(generated_url) or {
		if is_verbose {
			eprintln(err)
		}
		println(generated_url)
	}
}
