import os
import log

const should_clean = os.args.contains('-c')

fn main() {
	mut files := []string{}
	args := os.args#[2..].filter(it != '-c')
	for a in args {
		if os.is_file(a) {
			files << a
			continue
		}
		if os.is_dir(a) {
			files << os.walk_ext(a, '.v')
			continue
		}
	}
	files.sort()
	if files.len == 0 {
		println('0 .v files found.\n')
		println('Usage:')
		println('  v should-compile-all [-c] examples/ some/deep/file.v another/')
		println('... will try to compile all .v files found in the given folders and files, one by one.')
		println('If every single one of them compiles, the command will exit with an error code of 0.')
		println('If *any* of them *fail* to compile, the command will exit with an error code of 1.')
		println('')
		println('  -c will remove all the compiled executables at the end.')
		println('')
		println('Note: this command is intended to be used in CI pipelines for v modules, like this:')
		println(' cd module/ ; v should-compile-all examples/ \n')
		exit(1)
	}
	mut executables := []string{}
	mut failed_commands := []string{}
	mut project_folders := map[string]bool{}
	for idx, example in files {
		folder_of_example := os.dir(example)
		if os.is_file(os.join_path_single(folder_of_example, '.skip_should_compile_all')) {
			log.info('>>> skipping file: ${example}, because a `.skip_should_compile_all` file is present next to it.')
			continue
		}
		if project_folders[folder_of_example] {
			continue
		}
		if os.is_file(os.join_path_single(folder_of_example, 'v.mod')) {
			log.info('>>> delaying compilation of entire project folder ${folder_of_example} ...')
			project_folders[folder_of_example] = true
			continue
		}
		cmd := '${os.quoted_path(@VEXE)} ${os.quoted_path(example)}'
		log.info('> compiling ${idx + 1:4}/${files.len:-4}: ${cmd}')
		if 0 != os.system(cmd) {
			failed_commands << cmd
		} else {
			executables << executable_name(example)
		}
	}
	mut pfi := 0
	for pf, _ in project_folders {
		exe_path := os.join_path(pf, os.file_name(pf) + exe_extension)
		cmd := '${os.quoted_path(@VEXE)} -o ${exe_path} ${pf}'
		log.info('> compiling project ${pfi + 1:4}/${project_folders.len:-4}: ${cmd}')
		if 0 != os.system(cmd) {
			failed_commands << cmd
		} else {
			executables << exe_path
		}
		pfi++
	}
	if should_clean {
		log.info('Removing ${executables.len} successfully build executables...')
		for f in executables {
			os.rm(f) or { log.error('>> could not remove ${f}, err: ${err}') }
		}
	}
	if failed_commands.len > 0 {
		for idx, fcmd in failed_commands {
			log.error('>>> FAILED command ${idx + 1:4}/${failed_commands.len:-4}: ${fcmd}')
		}
		log.info('Summary: ${failed_commands.len:4}/${files.len:-4} file(s) failed to compile.')
		exit(1)
	}
	log.info('Summary: all ${files.len} file(s) compiled successfully.')
}

const exe_extension = if os.user_os() == 'windows' {
	'.exe'
} else {
	''
}

fn executable_name(source string) string {
	basepath := source.replace(os.file_ext(source), '')
	return basepath + exe_extension
}
