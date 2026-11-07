#!/usr/bin/env python3
"""Partition VTL's existing bin/test commands without changing its test discovery.

This is a PATH adapter only for the upstream script's V test, example-build, and
smoke-run invocations. The real compiler and its children get the original PATH,
so V commands issued from inside a test are never independently sharded.
"""

import hashlib
import os
from pathlib import Path
import shlex
import sys


def command_target(args, root):
    """Accept only the command shapes emitted by VTL's scoped bin/test mode."""
    if len(args) < 2:
        raise ValueError("expected a VTL test, example build, or smoke run")
    target = Path(args[-1]).resolve(strict=True)
    relative = target.relative_to(root).as_posix()
    if args[-2] in ("test", "run"):
        command = args[-2]
        flags = args[:-2]
        if command == "test":
            if not target.is_dir() and not target.name.endswith("_test.v"):
                raise ValueError("test target must be a package or *_test.v file")
        elif not target.is_file() or target.suffix != ".v":
            raise ValueError("smoke target must be a V source file")
    elif len(args) >= 3 and args[-3] == "-o":
        command = "build"
        flags = args[:-3]
        if not target.is_file() or target.suffix != ".v" or args[-2] != args[-1] + ".bin":
            raise ValueError("example output must be its source path plus .bin")
    else:
        raise ValueError("unsupported VTL V invocation")

    # Fail before selecting a shard if upstream adds an unsupported command or
    # flag. A silent success here could otherwise hide a new test or setup step.
    takes_value = {"-d", "-gc", "-cflags"}
    switches = {"-stats", "-prod", "-no-prod-options", "-autofree", "-no-parallel"}
    index = 0
    while index < len(flags):
        flag = flags[index]
        if flag in takes_value and index + 1 < len(flags):
            index += 2
        elif flag in switches:
            index += 1
        else:
            raise ValueError(f"unsupported VTL V argument: {flag}")
    return command, relative


def main(args):
    count_text = os.environ["VTL_CI_SHARD_COUNT"]
    index_text = os.environ["VTL_CI_SHARD_INDEX"]
    count, index = int(count_text), int(index_text)
    if count_text != str(count) or index_text != str(index) or not 0 <= index < count:
        raise ValueError("require a positive shard count and a zero-based index below it")
    root = Path(os.environ["VTL_CI_ROOT"]).resolve(strict=True)
    compiler = Path(os.environ["VTL_CI_VEXE"]).resolve(strict=True)
    original_path = os.environ["VTL_CI_PATH"]
    if not root.is_dir() or not compiler.is_file() or not original_path:
        raise ValueError("require a module directory, compiler file, and original PATH")
    command, relative = command_target(args, root)
    digest = hashlib.sha256(relative.encode("utf-8")).digest()
    selected = int.from_bytes(digest[:8], "big") % count == index
    action = "run" if selected else "skip"
    print(f"VTL shard {index}/{count}: {action} {command} {relative}: {shlex.join(args)}", flush=True)
    if not selected:
        return 0

    environment = dict(os.environ)
    environment["PATH"] = original_path
    for name in list(environment):
        if name.startswith("VTL_CI_"):
            del environment[name]
    # exec preserves the actual compiler's exit status and signal behavior.
    os.execve(str(compiler), [str(compiler), *args], environment)


if __name__ == "__main__":
    try:
        sys.exit(main(sys.argv[1:]))
    except (KeyError, OSError, ValueError) as error:
        print(f"VTL CI sharding: {error}", file=sys.stderr)
        sys.exit(2)
