"""Native Windows regression for publishing a verified optimized V compiler."""
import ctypes
from ctypes import wintypes
import hashlib
import os
from pathlib import Path
import queue
import shutil
import stat
import subprocess
import tempfile
import threading
import time

ROOT = Path(__file__).resolve().parents[2]
BASELINE = "3f2fd2c984cf742306fe83d2f27f579d1545e249"
SCRIPT = ".github/workflows/prepare_v.ps1"
MARKER = "Compiler publication sharing violation"
KERNEL32 = ctypes.WinDLL("kernel32", use_last_error=True)
CREATE = KERNEL32.CreateFileW
CREATE.argtypes = [
    wintypes.LPCWSTR, wintypes.DWORD, wintypes.DWORD, wintypes.LPVOID,
    wintypes.DWORD, wintypes.DWORD, wintypes.HANDLE,
]
CREATE.restype = wintypes.HANDLE
CLOSE = KERNEL32.CloseHandle
CLOSE.argtypes = [wintypes.HANDLE]
CLOSE.restype = wintypes.BOOL
INVALID_HANDLE = ctypes.c_void_p(-1).value


def git(cwd, *args):
    return subprocess.check_output(["git", "-C", str(cwd), *args]).strip()


def lock_for_read(path, share=1):
    # Deny deletion, optionally permitting writes as well as reads.
    handle = CREATE(str(path), 0x80000000, share, None, 3, 0x80, None)
    if handle == INVALID_HANDLE:
        raise ctypes.WinError(ctypes.get_last_error())
    return handle


def replay(script, mode):
    with tempfile.TemporaryDirectory(prefix="prepare-v-native-") as tmp:
        work = Path(tmp)
        git(work, "init", "-q")
        git(work, "-c", "user.name=CI", "-c", "user.email=ci@example.invalid",
            "commit", "--allow-empty", "-qm", "fixture")
        sha = git(work, "rev-parse", "HEAD").decode()
        asset = work / (sha + "-windows-x64")
        shutil.copyfile(ROOT / "v.exe", asset)
        digest = hashlib.sha256(asset.read_bytes()).hexdigest()
        expected = digest
        if mode == "bad-checksum":
            digest = "0" * 64
        Path(str(asset) + ".sha256").write_text(digest + "  " + asset.name + "\n")
        # A different real executable models an existing bootstrap image.
        destination = work / "v.exe"
        shutil.copyfile(Path(os.environ["SystemRoot"]) / "System32/where.exe",
                        destination)
        original = destination.read_bytes()
        handle = None
        if mode in ("baseline-lock", "upstream-lock", "release-lock", "release-delete-only", "persistent-lock"):
            handle = lock_for_read(destination, 3 if mode == "release-delete-only" else 1)
        if mode == "read-only":
            destination.chmod(stat.S_IREAD)
        output = []
        events = queue.Queue()
        process = None
        try:
            process = subprocess.Popen(
                ["pwsh", "-NoProfile", "-NonInteractive", "-File", str(script),
                 "-Prebuilt", str(asset)], cwd=work, stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT, text=True, errors="replace",
            )

            def read():
                for line in process.stdout:
                    events.put(line)
                events.put(None)

            reader = threading.Thread(target=read, daemon=True)
            reader.start()
            deadline = time.monotonic() + 60
            saw_retry = False
            while True:
                remaining = deadline - time.monotonic()
                if remaining <= 0:
                    raise AssertionError("native publication test exceeded 60 seconds")
                try:
                    line = events.get(timeout=remaining)
                except queue.Empty as exc:
                    raise AssertionError("native publication test exceeded 60 seconds") from exc
                if line is None:
                    break
                output.append(line)
                print(line, end="", flush=True)
                if MARKER in line:
                    saw_retry = True
                    if mode in ("release-lock", "release-delete-only") and handle is not None:
                        assert destination.read_bytes() == original
                        assert CLOSE(handle)
                        handle = None
            status = process.wait(timeout=10)
            print(f"mode={mode} status={status} retry={saw_retry}", flush=True)
            if mode in ("release-lock", "release-delete-only", "unlocked"):
                assert status == 0, "".join(output)
                assert hashlib.sha256(destination.read_bytes()).hexdigest() == expected
                assert not (work / "vprod.exe").exists()
                assert saw_retry == (mode in ("release-lock", "release-delete-only"))
                subprocess.run([str(destination), "version"], cwd=work, check=True)
            else:
                assert status != 0, "unexpected publication success"
                assert destination.read_bytes() == original
                assert saw_retry == (mode == "persistent-lock")
                if mode == "baseline-lock":
                    assert "Move-Item" in "".join(output), "".join(output)
                if mode == "upstream-lock":
                    assert "Access to the path is denied" in "".join(output), "".join(output)
                if mode == "bad-checksum":
                    assert "Compiler checksum does not match" in "".join(output)
        finally:
            if process is not None and process.poll() is None:
                process.kill()
                process.wait(timeout=10)
            if handle is not None:
                assert CLOSE(handle)
            if mode == "read-only":
                destination.chmod(stat.S_IREAD | stat.S_IWRITE)


if __name__ == "__main__":
    with tempfile.TemporaryDirectory(prefix="prepare-v-baseline-") as tmp:
        baseline = Path(tmp) / "prepare_v.ps1"
        baseline.write_bytes(git(ROOT, "show", BASELINE + ":" + SCRIPT) + b"\n")
        replay(baseline, "baseline-lock")
    with tempfile.TemporaryDirectory(prefix="prepare-v-upstream-") as tmp:
        upstream = Path(tmp) / "prepare_v.ps1"
        upstream.write_bytes(git(ROOT, "show", "d1d9c1aab01a87be8ed4025355e1ddedd6edabfa:" + SCRIPT) + b"\n")
        replay(upstream, "upstream-lock")
    for mode in ("unlocked", "release-lock", "release-delete-only", "persistent-lock", "read-only", "bad-checksum"):
        replay(ROOT / SCRIPT, mode)
