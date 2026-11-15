#!/usr/bin/env python3
"""Stop serial V test suites on their first failure or kernel-confirmed OOM."""
import os
from pathlib import Path
import re
import selectors
import signal
import subprocess
import sys


ANSI = re.compile(r'\x1b\[[0-?]*[ -/]*[@-~]')
START = re.compile(r'^\s*starting (.+_test(?:\.[a-z]+)?\.v) \.\.\.$')
RUNNING = re.compile(r'^\s*>>>>>\s+(?:compiling|running) .+`([^`]+_test(?:\.[a-z]+)?\.v)`')


def oom_kills(group):
    try:
        events = dict(line.split() for line in (group / 'memory.events').read_text().splitlines())
        return int(events['oom_kill'])
    except (OSError, ValueError, KeyError):
        return None


class OOMReporter:
    def __init__(self, group, output):
        self.group = group
        self.output = output
        self.previous = oom_kills(group)
        self.active = None
        self.failures = {}
        self.pending = b''

    def check(self):
        current = oom_kills(self.group)
        if current is not None and self.previous is not None and current > self.previous:
            name = self.active or '(sandbox command; test name unavailable)'
            killed = current - self.previous
            self.failures[name] = self.failures.get(name, 0) + killed
            self.output.write(f'\n[gitly_ci] OOM: {name} ({killed} process(es) killed by the kernel).\n'.encode())
            self.output.flush()
        self.previous = current

    def feed(self, data):
        self.output.write(data)
        self.output.flush()
        self.pending += data
        while b'\n' in self.pending:
            line, self.pending = self.pending.split(b'\n', 1)
            line = ANSI.sub('', line.decode(errors='replace')).rstrip('\r')
            match = START.match(line) or RUNNING.match(line)
            if match:
                # Sample before changing the active file, including when a failed
                # result and the next start arrive in one pipe read.
                if self.active is None:
                    self.active = match[1]
                self.check()
                self.active = match[1]
        # Compiler diagnostics can contain very long lines; keep memory bounded.
        self.pending = self.pending[-16384:]

    def finish(self):
        self.check()
        if self.failures:
            self.output.write(b'\n[gitly_ci] OOM failure (test suite stopped):\n')
            for name, count in self.failures.items():
                self.output.write(f'  OOM  {name} ({count} killed)\n'.encode())
            self.output.flush()


def supervise(arguments, group=Path('/sys/fs/cgroup'), output=None):
    output = output or sys.stdout.buffer
    environment = dict(os.environ, VJOBS='1', VTEST_FAIL_FAST='1',
                       VTEST_SHOW_START='1', VTEST_MAX_COMPILATION_RETRIES='1')
    reporter = OOMReporter(group, output)
    process = subprocess.Popen(arguments, env=environment, stdout=subprocess.PIPE,
                               stderr=subprocess.STDOUT, start_new_session=True)
    previous_handlers = {}
    oom_stopped = False

    def forward(signum, _frame):
        try:
            os.killpg(process.pid, signum)
        except ProcessLookupError:
            pass
        except PermissionError:
            # macOS can report EPERM for a group whose leader just became a
            # zombie. A still-running command must never hide a signal error.
            if process.poll() is None:
                raise

    try:
        for signum in (signal.SIGTERM, signal.SIGINT):
            previous_handlers[signum] = signal.signal(signum, forward)
        with selectors.DefaultSelector() as selector:
            selector.register(process.stdout, selectors.EVENT_READ)
            while selector.get_map():
                for key, _events in selector.select(timeout=0.1):
                    data = os.read(key.fd, 65536)
                    if data:
                        reporter.feed(data)
                    else:
                        selector.unregister(key.fileobj)
                reporter.check()
                if reporter.failures and not oom_stopped:
                    # A nested compiler can die while its parent keeps running.
                    # Stop the entire suite, including descendants holding stdout.
                    forward(signal.SIGKILL, None)
                    oom_stopped = True
        code = process.wait()
        reporter.finish()
        if code < 0:
            code = 128 - code
        # A test's own retry/ignored-failure policy must not hide a kernel OOM.
        return 137 if reporter.failures else code
    finally:
        for signum, handler in previous_handlers.items():
            signal.signal(signum, handler)
        if process.poll() is None:
            os.killpg(process.pid, signal.SIGKILL)
            process.wait()
        process.stdout.close()


if __name__ == '__main__':
    raise SystemExit(supervise(sys.argv[1:]))
