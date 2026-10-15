module main

import os
import time

fn test_run_and_inline_forward_termination_and_clean_files() ! {
	for signal_number in [1, 15] {
		for inline in [false, true] {
			root := os.join_path(os.vtmp_dir(), 'run_signal_${os.getpid()}_${time.now().unix_nano()}')
			os.mkdir_all(root)!
			defer { os.rmdir_all(root) or {} }
			ready := os.join_path(root, 'ready')
			program := "import os\nimport time\nfn main() { os.write_file('${ready}', os.getpid().str()) or { panic(err) }; time.sleep(30 * time.second) }\n"
			mut args := ['-nocache', '-gc', 'none']
			if inline {
				args << ['-e', program]
			} else {
				source := os.join_path(root, 'main.v')
				os.write_file(source, program)!
				args << ['run', source]
			}
			mut process := os.new_process(@VEXE)
			process.set_args(args)
			process.set_work_folder(root)
			mut environment := os.environ()
			environment.delete('VEXE')
			environment.delete('VFLAGS')
			environment['V_MACOS_V3_NO_FALLBACK'] = '1'
			process.set_environment(environment)
			process.set_redirect_stdio()
			process.run()
			defer {
				if process.is_alive() { process.signal_kill() }
				process.wait()
				process.close()
			}
			deadline := time.now().add(45 * time.second)
			for !os.exists(ready) && process.is_alive() && time.now() < deadline {
				time.sleep(10 * time.millisecond)
			}
			assert os.exists(ready), 'child did not become ready'
			child_pid := os.read_file(ready)!.int()
			mut child_completed := false
			defer {
				if !child_completed { C.kill(child_pid, 9) }
			}
			assert C.kill(process.pid, signal_number) == 0
			stop_deadline := time.now().add(10 * time.second)
			for process.is_alive() && time.now() < stop_deadline {
				time.sleep(10 * time.millisecond)
			}
			assert !process.is_alive(), 'compiler did not stop after signal ${signal_number}'
			process.wait()
			assert process.code == 128 + signal_number, process.stderr_slurp()
			assert C.kill(child_pid, 0) == -1, 'run child still exists'
			child_completed = true
			for entry in os.ls(root)! {
				assert !entry.starts_with('.v3_inline_'), entry
				assert entry != 'main', 'implicit executable survived termination'
			}
		}
	}
}

fn test_run_signal_helper_has_a_portable_windows_stub() {
	cc := os.find_abs_path_of_executable('clang') or { return }
	source := os.join_path(@VEXEROOT, 'vlib', 'v', 'driver', 'run_signals_nix.c')
	result := os.exec([cc, '-D_WIN32', '-Wall', '-Wextra', '-Werror', '-fsyntax-only', source])
	assert result.exit_code == 0, result.output
}

fn test_run_signal_forwarded_before_exec_terminates_child() ! {
	cc := os.find_abs_path_of_executable('clang') or {
		os.find_abs_path_of_executable('cc') or { return }
	}
	root := os.join_path(os.vtmp_dir(), 'run_signal_preexec_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'preexec.c')
	binary := os.join_path(root, 'preexec')
	os.write_file(source, run_signal_preexec_harness)!
	helper := os.join_path(@VEXEROOT, 'vlib', 'v', 'driver', 'run_signals_nix.c')
	compiled := os.exec([cc, '-Wall', '-Wextra', '-Werror', '-o', binary, source, helper])
	assert compiled.exit_code == 0, compiled.output
	for signal_number in [1, 15] {
		result := os.exec([binary, signal_number.str()])
		assert result.exit_code == 0, result.output
	}
}

// Keep the child between fork and exec so signal forwarding exercises its inherited handler.
// The parent's alarm kills and reaps a stuck child, making failures bounded.
const run_signal_preexec_harness = r'
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int v3_run_signals_begin(void);
void v3_run_signals_set_pid(int child_pid);
int v3_run_signals_finish(void);
static volatile sig_atomic_t timeout_child;
static volatile sig_atomic_t timed_out;
static void timeout_cleanup(int signo) {
    (void)signo;
    timed_out = 1;
    if (timeout_child > 0) kill((int)timeout_child, SIGKILL);
}
int main(int argc, char **argv) {
    int signo = argc > 1 ? atoi(argv[1]) : SIGTERM;
    int ready[2], gate[2];
    if (pipe(ready) || pipe(gate) || v3_run_signals_begin()) return 3;
    pid_t child = fork();
    if (child < 0) return 4;
    if (child == 0) {
        unsigned char byte = 1;
        close(ready[0]);
        close(gate[1]);
        if (write(ready[1], &byte, 1) != 1) _exit(5);
        close(ready[1]);
        while (read(gate[0], &byte, 1) < 0 && errno == EINTR) {}
        execl("/bin/sh", "sh", "-c", "exit 99", (char *)0);
        _exit(6);
    }
    close(ready[1]);
    close(gate[0]);
    timeout_child = child;
    signal(SIGALRM, timeout_cleanup);
    alarm(2);
    unsigned char byte;
    ssize_t read_result;
    do { read_result = read(ready[0], &byte, 1); } while (read_result < 0 && errno == EINTR);
    close(ready[0]);
    v3_run_signals_set_pid((int)child);
    if (read_result != 1 || kill(getpid(), signo)) kill(child, SIGKILL);
    int status;
    pid_t waited;
    do { waited = waitpid(child, &status, 0); } while (waited < 0 && errno == EINTR);
    alarm(0);
    timeout_child = 0;
    close(gate[1]);
    int received = v3_run_signals_finish();
    if (timed_out || waited != child || received != signo || !WIFEXITED(status)
        || WEXITSTATUS(status) != 128 + signo) {
        fprintf(stderr, "signal=%d timeout=%d status=%d received=%d\n", signo, timed_out, status, received);
        return 1;
    }
    return 0;
}
'
