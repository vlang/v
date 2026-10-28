"""Bounded pseudo-terminal coverage for v run foreground job control."""
import errno
import os
import re
import select
import shlex
import signal
import subprocess
import sys
import time
from pathlib import Path

vexe, root = sys.argv[1:3]
pipeline = len(sys.argv) > 3
Path(root).mkdir(parents=True, exist_ok=True)
source = Path(root) / "main.c.v"
reader = Path(root) / "reader.c"
# macOS getline can return EINTR after even an ordinary shell job resumes.
# Retry reads here so this fixture isolates foreground ownership and job control.
reader.write_text(r"""#include <errno.h>
#include <unistd.h>
int read_terminal_line(unsigned char *bytes, int capacity) {
    int size = 0;
    while (size < capacity) {
        unsigned char byte;
        ssize_t count = read(0, &byte, 1);
        if (count < 0 && errno == EINTR) continue;
        if (count < 0) return -1;
        if (!count || byte == '\n') return size;
        bytes[size++] = byte;
    }
    return size;
}
""")
source.write_text("import os\n#flag " + str(reader) + "\n"
                  "fn C.read_terminal_line(&u8, i32) i32\n"
                  "fn main() { println('READY:' + os.getpid().str()); "
                  "for _ in 0 .. 3 { print('input> '); os.flush(); "
                  "mut bytes := []u8{len:4096}; amount := C.read_terminal_line(bytes.data, i32(bytes.len)); "
                  "assert amount >= 0; println('READ:' + bytes[..int(amount)].bytestr()) } }\n")
import pty
shell, terminal = pty.fork()
if shell == 0:
    os.chdir(root)
    os.environ.update(PS1="VPTY> ", ENV="", BASH_ENV="", TERM="xterm")
    os.execv("/bin/sh", ["/bin/sh", "-i"])

buffer = b""
transcript = b""
compiler = 0
child = 0

def read_until(marker, seconds=12):
    global buffer, transcript
    deadline = time.monotonic() + seconds
    while marker not in buffer:
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            raise AssertionError("terminal did not produce " + repr(marker))
        if not select.select([terminal], [], [], remaining)[0]:
            continue
        try:
            chunk = os.read(terminal, 65536)
        except OSError as error:
            if error.errno == errno.EIO:
                chunk = b""
            else:
                raise
        if not chunk:
            raise AssertionError("terminal exited before " + repr(marker))
        buffer += chunk
        transcript += chunk
    index = buffer.index(marker) + len(marker)
    result, buffer = buffer[:index], buffer[index:]
    return result

def send(text):
    os.write(terminal, text)

def wait_stopped(pid):
    deadline = time.monotonic() + 5
    while time.monotonic() < deadline:
        state = subprocess.run(["ps", "-o", "state=", "-p", str(pid)],
                               capture_output=True, text=True, check=False).stdout
        if "T" in state:
            return
        time.sleep(0.02)
    raise AssertionError("background terminal reader did not stop")

def wait_job_stopped():
    # A job that reads the terminal in the background stops again. The shell's
    # `fg` continues only a job that it has seen stop; before that it just hands
    # the terminal over, and the stopped processes never resume.
    wait_stopped(compiler)
    deadline = time.monotonic() + 12
    while True:
        send(b"jobs\n")
        if b"Stopped" in read_until(b"VPTY> "):
            return
        if time.monotonic() >= deadline:
            raise AssertionError("shell did not see the background job stop")
        time.sleep(0.02)

def cleanup():
    # Include children that created separate job groups or sessions.
    rows = subprocess.check_output(["ps", "-axo", "pid,ppid"], text=True).splitlines()[1:]
    parents = {int(parts[0]): int(parts[1]) for row in rows if len(parts := row.split()) == 2}
    owned = {shell}
    while True:
        children = {pid for pid, parent in parents.items() if parent in owned} - owned
        if not children:
            break
        owned |= children
    for pid in owned | {compiler, child}:
        if pid > 0:
            try:
                os.kill(pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
    os.close(terminal)
    deadline = time.monotonic() + 2
    while time.monotonic() < deadline:
        try:
            if os.waitpid(shell, os.WNOHANG)[0]:
                break
        except ChildProcessError:
            break
        time.sleep(0.02)

def early_terminal_write():
    global compiler, child
    fixture = Path(root) / "early.c"
    helper = Path(__file__).resolve().parents[5] / "vlib/v/driver/run_signals_nix.c"
    fixture.write_text(r"""#define _DEFAULT_SOURCE 1
#define _POSIX_C_SOURCE 200809L
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>
#ifdef V3_TEST_HEADERLESS
#define V3_RUN_SIGNALS_HEADERLESS 1
static int opaque_action(int n, const void *a, void *p) { return sigaction(n, a, p); }
#define v3_run_sigaction opaque_action
#ifdef __APPLE__
int fixture_siginterrupt(int, int) __asm__("_siginterrupt");
#else
int fixture_siginterrupt(int, int) __asm__("siginterrupt");
#endif
#define v3_run_siginterrupt fixture_siginterrupt
#define v3_run_sigprocmask sigprocmask
#define v3_run_sigemptyset sigemptyset
#define v3_run_sigaddset sigaddset
#define v3_run_setpgid setpgid
#define v3_run_getpgid getpgid
#define v3_run_getpgrp getpgrp
#define v3_run_tcgetpgrp tcgetpgrp
#define v3_run_tcsetpgrp tcsetpgrp
#define v3_run_open open
#define v3_run_close close
#define v3_run_fcntl fcntl
#define v3_run_waitpid waitpid
#define v3_run_fork fork
#define v3_run_pipe pipe
#define v3_run_read read
#define v3_run_write write
#endif
#include "@HELPER@"
int main(int argc, char **argv) {
    if (argc != 2 || v3_run_signals_begin()) return 3;
    pid_t app = fork();
    if (app < 0) return 4;
    if (!app) {
        struct sigaction action = {0};
        action.sa_handler = SIG_DFL;
        if (setpgid(0, 0) || sigaction(SIGTTOU, &action, 0)) _exit(5);
        const char *bytes = "EARLY_APP_WRITE\n";
        int offset = 0;
        while (offset < 16) {
            long count = write(1, bytes + offset, (size_t)(16 - offset));
            if (count < 0 && errno == EINTR) continue;
            if (count <= 0) _exit(6);
            offset += (int)count;
        }
        _exit(0);
    }
    FILE *ready = fopen(argv[1], "w");
    if (!ready) return 7;
    fprintf(ready, "%d", app); fclose(ready);
    siginfo_t info;
    /* Observe the real early stop without consuming the waiter's status. */
    if (waitid(P_PID, app, &info, WSTOPPED | WNOWAIT)
        || info.si_code != CLD_STOPPED || info.si_status != SIGTTOU) return 8;
    if (v3_run_signals_set_pid(app)) return 9;
    int code, signal_number;
    if (v3_run_signals_wait(&code, &signal_number)) return 10;
    int received = v3_run_signals_finish();
    if (code || signal_number || received) return 11;
    puts("EARLY_WRITE_OK");
    return 0;
}
""".replace("@HELPER@", str(helper)))
    send(b"stty tostop\n")
    read_until(b"VPTY> ")
    for headerless in [False, True]:
        binary = Path(root) / ("early-headerless" if headerless else "early-native")
        args = ["cc", "-Wall", "-Wextra", "-Werror", str(fixture), "-o", str(binary)]
        if headerless:
            args.append("-DV3_TEST_HEADERLESS")
        subprocess.run(args, check=True, timeout=20)
        ready = Path(root) / (binary.name + ".ready")
        send((shlex.join([str(binary), str(ready)]) + " & job=$!; echo JOB:$job; fg; echo EARLY_STATUS:$?\n").encode())
        read_until(b"\r\nJOB:")
        compiler = int(read_until(b"\r\n").strip())
        read_until(b"EARLY_WRITE_OK")
        read_until(b"VPTY> ")
        child = int(ready.read_text())
    send(b"stty -tostop\n")
    read_until(b"VPTY> ")
    print("PTY deterministic early TOSTOP write recovered: native and headerless", flush=True)

def extra_foreground_cases():
    global compiler, child
    extra = Path(root) / "interrupt.c"
    extra.write_text(r"""#include <signal.h>
#include <unistd.h>
static volatile sig_atomic_t calls;
static void caught(int signo) { (void)signo; calls++; }
int install_interrupt(int signo) {
    struct sigaction action = {0};
    action.sa_handler = caught; action.sa_flags = SA_RESTART;
    sigemptyset(&action.sa_mask);
    return sigaction(signo, &action, 0);
}
int ignore_interrupt(int signo) { return signal(signo, SIG_IGN) == SIG_ERR ? -1 : 0; }
int interrupt_calls(void) { return calls; }
int close_stdout(void) { return close(1); }
int leave_app_group(void) { return setpgid(0, getpgid(getppid())); }
""")
    output_source = Path(root) / "output.c.v"
    output_source.write_text("import os\nimport time\n#flag " + str(reader) + "\n#flag " + str(extra) + "\n"
        "fn C.read_terminal_line(&u8, i32) i32\nfn C.install_interrupt(i32) i32\n"
        "fn C.ignore_interrupt(i32) i32\nfn C.interrupt_calls() i32\nfn C.close_stdout() i32\nfn C.leave_app_group() i32\n"
        "fn main() { mode := os.args[1]; if mode in ['catch', 'parent', 'stop'] { assert C.install_interrupt(i32(os.args[4].int())) == 0 }; "
        "if mode == 'ignorestop' { assert C.ignore_interrupt(i32(os.args[4].int())) == 0 }; if mode == 'close' { assert C.close_stdout() == 0 }; if mode == 'move' { assert C.leave_app_group() == 0 }; os.write_file(os.args[2], os.getpid().str())!; if mode == 'move' { return }; "
        "mut bytes := []u8{len:4096}; _ = C.read_terminal_line(bytes.data, i32(bytes.len)); "
        "if mode == 'parent' { for C.interrupt_calls() == 0 { time.sleep(time.millisecond) } }; "
        "os.write_file(os.args[3], C.interrupt_calls().str())! }\n")
    for mode, signo in [("default", signal.SIGINT), ("catch", signal.SIGINT),
                         ("catch", signal.SIGQUIT), ("stop", signal.SIGTSTP), ("ignorestop", signal.SIGTSTP), ("parent", signal.SIGINT), ("close", signal.SIGINT), ("move", signal.SIGINT)]:
        signo = int(signo)
        ready = Path(root) / (mode + str(signo) + ".ready")
        counts = Path(root) / (mode + str(signo) + ".counts")
        command = shlex.join([vexe, "-nocache", "-gc", "none", "run", str(output_source),
                              mode, str(ready), str(counts), str(signo)])
        peer_command = "cat >/dev/null" if mode in ["close", "move"] else "sleep 60"
        send((command + " | " + peer_command + " & peer=$!; echo PEER:$peer; fg\n").encode())
        read_until(b"\r\nPEER:")
        peer = int(read_until(b"\r\n").strip())
        compiler = os.getpgid(peer)
        deadline = time.monotonic() + 45
        while not ready.exists() and time.monotonic() < deadline:
            time.sleep(0.02)
        assert ready.exists(), "output pipeline did not become ready"
        child = int(ready.read_text())
        if mode != "move":
            assert os.tcgetpgrp(terminal) == child
        if mode in ["stop", "ignorestop"]:
            send(b"\x1a")
            read_until(b"VPTY> ")
            wait_stopped(compiler)
            wait_stopped(peer)
            send(b"fg\n")
            deadline = time.monotonic() + 5
            while os.tcgetpgrp(terminal) != child and time.monotonic() < deadline:
                time.sleep(0.02)
            assert os.tcgetpgrp(terminal) == child
            send(b"done\n")
            deadline = time.monotonic() + 5
            while not counts.exists() and time.monotonic() < deadline:
                time.sleep(0.02)
            assert counts.exists()
            assert counts.read_text() == ("1" if mode == "stop" else "0")
            os.kill(peer, signal.SIGTERM)
        elif mode == "parent":
            os.kill(compiler, signal.SIGINT)
            send(b"done\n")
            deadline = time.monotonic() + 5
            while not counts.exists() and time.monotonic() < deadline:
                time.sleep(0.02)
            assert counts.exists() and counts.read_text() == "1"
            while time.monotonic() < deadline:
                state = subprocess.run(["ps", "-o", "state=", "-p", str(compiler)],
                                       capture_output=True, text=True, check=False).stdout.strip()
                if not state or "Z" in state:
                    break
                time.sleep(0.02)
            else:
                raise AssertionError("compiler did not finish after parent-origin interrupt")
            os.kill(peer, 0)
            os.kill(peer, signal.SIGTERM)
        elif mode == "close":
            send(b"done\n")
        elif mode != "move":
            send(b"\x03" if signo == signal.SIGINT else b"\x1c")
        deadline = time.monotonic() + 5
        while time.monotonic() < deadline:
            state = subprocess.run(["ps", "-o", "state=", "-p", str(peer)],
                                   capture_output=True, text=True, check=False).stdout.strip()
            if not state or "Z" in state:
                break
            time.sleep(0.02)
        else:
            raise AssertionError("output pipeline peer survived terminal interrupt/EOF")
        if mode == "catch":
            os.kill(child, 0)
            send(b"done\n")
        read_until(b"VPTY> ")
        if mode == "catch":
            assert counts.read_text() == "1", "terminal interrupt was forwarded twice"
        assert not (Path(root) / "output").exists(), "output pipeline executable survived"
    print("PTY output pipelines: default/caught INT, caught QUIT, parent-only INT, stdout EOF and app group move passed", flush=True)

try:
    read_until(b"VPTY> ")
    send(b'if [ -n "${BASH_VERSION-}" ]; then set +H; fi\n')
    read_until(b"VPTY> ")
    command = shlex.join([vexe, "-nocache", "-gc", "none", "run", str(source)])
    if pipeline:
        # BSD cat, like getline, can exit on EINTR after a shell stop/resume.
        peer_source = Path(root) / "peer.c"
        peer = Path(root) / "peer"
        peer_source.write_text(r"""#include <errno.h>
#include <unistd.h>
int main(void) {
    unsigned char bytes[4096];
    for (;;) {
        ssize_t count = read(0, bytes, sizeof(bytes));
        if (count < 0 && errno == EINTR) continue;
        if (count <= 0) return count < 0;
        ssize_t offset = 0;
        while (offset < count) {
            ssize_t written = write(1, bytes + offset, (size_t)(count - offset));
            if (written < 0 && errno == EINTR) continue;
            if (written <= 0) return 1;
            offset += written;
        }
    }
}
""")
        subprocess.run(["cc", str(peer_source), "-o", str(peer)], check=True, timeout=20)
        command = shlex.quote(str(peer)) + " | " + command
    send((command + " & job=$!; echo JOB:$job; fg\n").encode())
    startup = read_until(b"input> ", 45)
    compiler = int(re.search(rb"JOB:(\d+)", startup).group(1))
    child = int(re.search(rb"READY:(\d+)", startup).group(1))
    assert os.tcgetpgrp(terminal) == (os.getpgid(compiler) if pipeline else child)
    if pipeline:
        assert os.getpgid(compiler) != compiler, "compiler should be a pipeline group member"
    send(b"\x1a")
    read_until(b"VPTY> ")
    wait_stopped(compiler)
    assert os.tcgetpgrp(terminal) == shell
    send(b"fg\nfirst\n")
    read_until(b"READ:first")
    read_until(b"input> ")
    send(b"\x1a")
    read_until(b"VPTY> ")
    send(b"bg\n")
    read_until(b"VPTY> ")
    # The pipeline's peer reads the terminal, so that job stops again too.
    wait_job_stopped()
    assert os.tcgetpgrp(terminal) == shell
    send(b"fg\nsecond\n")
    read_until(b"READ:second")
    read_until(b"input> ")
    # An external group stop must stop the isolated child and return the terminal.
    os.killpg(os.getpgid(compiler), signal.SIGTSTP)
    read_until(b"VPTY> ")
    wait_stopped(compiler)
    assert os.tcgetpgrp(terminal) == shell
    os.killpg(os.getpgid(compiler), signal.SIGCONT)
    # Terminal input stops again in the background: the app's own in the direct
    # case, the peer's in the pipeline case.
    wait_job_stopped()
    send(b"fg\n")
    deadline = time.monotonic() + 5
    while os.tcgetpgrp(terminal) != (os.getpgid(compiler) if pipeline else child) and time.monotonic() < deadline:
        time.sleep(0.02)
    assert os.tcgetpgrp(terminal) == (os.getpgid(compiler) if pipeline else child)
    send(b"\x03")
    read_until(b"VPTY> ")
    send(b"echo STATUS:$?\n")
    read_until(b"STATUS:130")
    assert not (Path(root) / "main").exists(), "implicit executable survived Ctrl-C"
    if pipeline:
        command = "sleep 60 | " + shlex.join([vexe, "-nocache", "-gc", "none", "run", str(source)])
        send((command + " & job=$!; echo JOB:$job; fg\n").encode())
        startup = read_until(b"input> ", 45)
        compiler = int(re.search(rb"JOB:(\d+)", startup).group(1))
        child = int(re.search(rb"READY:(\d+)", startup).group(1))
        assert os.getpgid(compiler) != compiler
        send(b"\x1a")
        read_until(b"VPTY> ")
        wait_stopped(compiler)
        send(b"fg\n")
        deadline = time.monotonic() + 5
        while os.tcgetpgrp(terminal) != os.getpgid(compiler) and time.monotonic() < deadline:
            time.sleep(0.02)
        assert os.tcgetpgrp(terminal) == os.getpgid(compiler)
        send(b"\x03")
        read_until(b"VPTY> ")
        send(b"echo STATUS:$?\n")
        read_until(b"STATUS:130")
        assert not (Path(root) / "main").exists()
    if not pipeline:
        early_terminal_write()
        extra_foreground_cases()
    print("PTY stdin, Ctrl-Z/fg/bg, group TSTP/CONT, Ctrl-C, cleanup passed; pipeline=" + str(pipeline), flush=True)
except BaseException:
    sys.stderr.buffer.write(transcript)
    raise
finally:
    cleanup()

if not pipeline:
    subprocess.run([sys.executable, __file__, vexe, str(Path(root) / "pipeline"), "pipeline"],
                   check=True, timeout=90)
