import dl
import net.urllib
import os

// get output from `v doctor`
fn get_vdoctor_output(is_verbose bool) string {
	vexe := os.getenv('VEXE')
	verbose_flag := if is_verbose { '-v' } else { '' }
	result := os.execute('$vexe $verbose_flag doctor')
	if result.exit_code != 0 {
		eprintln('unable to get `v doctor` output: $result.output')
		return ''
	}
	return result.output
}

// get ouput from `v -g -o vdbg cmd/v && vdbg file.v`
fn get_v_build_output(is_verbose bool, is_yes bool, file_path string) string {
	mut vexe := os.getenv('VEXE')
	v_dir := os.dir(vexe)
	verbose_flag := if is_verbose { '-v' } else { '' }
	vdbg_path := $if windows { '$v_dir/vdbg.exe' } $else { '$v_dir/vdbg' }
	vdbg_result := os.execute('"$vexe" $verbose_flag -g -o "$vdbg_path" $v_dir/cmd/v')
	if vdbg_result.exit_code == 0 {
		vexe = vdbg_path
	} else {
		eprintln('unable to compile V in debug mode: $vdbg_result.output')
	}
	mut result := os.execute('"$vexe" $verbose_flag "$file_path"')
	defer {
		os.rm(vdbg_path) or {
			if is_verbose {
				eprintln('unable to delete `vdbg`: $err')
			}
		}
	}
	if result.exit_code == 0 {
		defer {
			mut generated_file := file_path.all_before_last('.')
			$if windows {
				generated_file += '.exe'
			}
			os.rm(generated_file) or {
				if is_verbose {
					eprintln('unable to delete generated file: $err')
				}
			}
		}
		run := is_yes
			|| ask('It looks like the compilation went well, do you want to run the file?')
		if run {
			result = os.execute('"$vexe" $verbose_flag run "$file_path"')
			if result.exit_code == 0 && !is_yes {
				confirm_or_exit('It looks like the file ran correctly as well, are you sure you want to continue?')
			}
		}
	}
	return result.output
}

type ShellExecuteWin = fn (voidptr, &u16, &u16, &u16, &u16, int)

// open a uri using the default associated application
fn open_browser(uri string) ? {
	$if macos {
		result := os.execute('open "$uri"')
		if result.exit_code != 0 {
			return error('unable to open url: $result.output')
		}
	} $else $if freebsd || openbsd {
		result := os.execute('xdg-open "$uri"')
		if result.exit_code != 0 {
			return error('unable to open url: $result.output')
		}
	} $else $if linux {
		providers := ['xdg-open', 'x-www-browser', 'www-browser', 'wslview']

		// There are multiple possible providers to open a browser on linux
		// One of them is xdg-open, another is x-www-browser, then there's www-browser, etc.
		// Look for one that exists and run it
		for provider in providers {
			if os.exists_in_system_path(provider) {
				result := os.execute('$provider "$uri"')
				if result.exit_code != 0 {
					return error('unable to open url: $result.output')
				}
				break
			}
		}
	} $else $if windows {
		handle := dl.open_opt('shell32', dl.rtld_now) ?
		// https://docs.microsoft.com/en-us/windows/win32/api/shellapi/nf-shellapi-shellexecutew
		func := ShellExecuteWin(dl.sym_opt(handle, 'ShellExecuteW') ?)
		func(C.NULL, 'open'.to_wide(), uri.to_wide(), C.NULL, C.NULL, C.SW_SHOWNORMAL)
		dl.close(handle)
	} $else {
		return error('unsupported platform')
	}
}

fn ask(msg string) bool {
	prompt := os.input_opt('$msg [Y/n] ') or { 'y' }
	return prompt == '' || prompt[0].ascii_str().to_lower() != 'n'
}

fn confirm_or_exit(msg string) {
	if !ask(msg) {
		exit(1)
	}
}

fn main() {
	mut file_path := ''
	mut is_verbose := false
	mut is_yes := false
	for arg in os.args[2..] {
		match arg {
			'-v' {
				is_verbose = true
			}
			'-y' {
				is_yes = true
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
		eprintln('v bug: no v file listed to report')
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
	build_output := get_v_build_output(is_verbose, is_yes, file_path)
	// ask the user if he wants to submit even after an error
	if !is_yes && (vdoctor_output == '' || file_content == '' || build_output == '') {
		confirm_or_exit('An error occured retrieving the information, do you want to continue?')
	}

	expected_result := os.input_opt('What did you expect to see? ') or { '' }
	// open prefilled issue creation page, or print link as a fallback

	if !is_yes && vdoctor_output.contains('behind V master') {
		confirm_or_exit('It looks like your installation of V is outdated, we advise you to run `v up` before submitting an issue. Are you sure you want to continue?')
	}

	// When updating this template, make sure to update `.github/ISSUE_TEMPLATE/bug_report.md` too
	raw_body := '<!-- It is advisable to update all relevant modules using `v outdated` and `v install` -->
**V doctor:**
```
$vdoctor_output```

**What did you do?**
`v -g -o vdbg cmd/v && vdbg $file_path`
{file_content}

**What did you expect to see?**

$expected_result

**What did you see instead?**
```
$build_output```'
	mut encoded_body := urllib.query_escape(raw_body.replace_once('{file_content}', '```v\n$file_content\n```'))
	mut generated_uri := 'https://github.com/vlang/v/issues/new?labels=Bug&body=$encoded_body'
	if generated_uri.len > 8192 {
		// GitHub doesn't support URLs longer than 8192 characters
		encoded_body = urllib.query_escape(raw_body.replace_once('{file_content}', 'See attached file `$file_path`'))
		generated_uri = 'https://github.com/vlang/v/issues/new?labels=Bug&body=$encoded_body'
		println('Your file is too big to be submitted. Head over to the following URL and attach your file.')
		println(generated_uri)
	} else {
		open_browser(generated_uri) or {
			if is_verbose {
				eprintln(err)
			}
			println(generated_uri)
		}
	}
}
