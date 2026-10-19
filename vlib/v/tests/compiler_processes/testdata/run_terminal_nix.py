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
    if not pipeline:
        wait_stopped(compiler)
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
    # Direct terminal input stops again in the background; pipe input keeps waiting.
    if not pipeline:
        wait_stopped(compiler)
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
    print("PTY stdin, Ctrl-Z/fg/bg, group TSTP/CONT, Ctrl-C, cleanup passed; pipeline=" + str(pipeline), flush=True)
except BaseException:
    sys.stderr.buffer.write(transcript)
    raise
finally:
    cleanup()

if not pipeline:
    subprocess.run([sys.executable, __file__, vexe, str(Path(root) / "pipeline"), "pipeline"],
                   check=True, timeout=90)
