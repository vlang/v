# Runs the shared tccbin conformance tests (and, if -Platform is given,
# that platform's additional regression tests) against a built tcc.
#
# Usage:
#   run.ps1 -Tcc <path to tcc.exe> [-Platform windows] [-- <extra tcc args>]
#
# Extra args (everything after --) are passed to every test compile -
# e.g. GC defines, -bt25, -municode, -ldbghelp -luser32, path to
# libgc.a. These vary per platform, so the caller supplies them; this
# script doesn't hardcode anything platform-specific itself.
#
# Each test is a pair: <name>.c and <name>.expected. The .expected
# file has 1 or 2 lines:
#   line 1: expected exit code, or the literal word "nonzero"
#   line 2 (optional): a substring that must appear in the program's
#                       combined stdout+stderr. Omit for no check.

param(
    [Parameter(Mandatory = $true)][string]$Tcc,
    [string]$Platform = "",
    [Parameter(ValueFromRemainingArguments = $true)][string[]]$ExtraArgs
)

$ErrorActionPreference = "Stop"
$Here = $PSScriptRoot

function Get-TestDirs {
    $dirs = @(Join-Path $Here "shared")
    if ($Platform -ne "") {
        $p = Join-Path $Here "platform\$Platform"
        if (Test-Path $p) { $dirs += $p }
    }
    return $dirs
}

$passed = 0
$failed = 0
$work = Join-Path ([System.IO.Path]::GetTempPath()) ("tccbin-tests-" + [guid]::NewGuid())
New-Item -ItemType Directory -Path $work | Out-Null

foreach ($dir in (Get-TestDirs)) {
    foreach ($src in (Get-ChildItem (Join-Path $dir "*.c"))) {
        $name = $src.BaseName
        $expectedFile = Join-Path $dir "$name.expected"
        if (-not (Test-Path $expectedFile)) {
            Write-Host "FAIL $name (no .expected file)"
            $failed++
            continue
        }
        $lines = @(Get-Content $expectedFile)
        $expectExit = $lines[0].Trim()
        $expectSubstr = if ($lines.Count -gt 1) { $lines[1] } else { "" }

        $exe = Join-Path $work "$name.exe"
        & $Tcc $src.FullName @ExtraArgs -o $exe 2>&1 | Out-Null
        if ($LASTEXITCODE -ne 0) {
            Write-Host "FAIL $name (compile error)"
            $failed++
            continue
        }

        # Read via the .NET Process API directly rather than `&`/$LASTEXITCODE
        # (raced/read stale exit codes on processes that terminate via an
        # unhandled SEH exception) or Start-Process -RedirectStandardOutput
        # to a file (the file wasn't reliably flushed by the time -Wait
        # returned, for short-lived processes - both reproduced locally).
        # ReadToEnd() blocks until the stream closes (i.e. the process has
        # actually finished producing output), so there's no race.
        $psi = [System.Diagnostics.ProcessStartInfo]::new($exe)
        $psi.RedirectStandardOutput = $true
        $psi.RedirectStandardError = $true
        $psi.UseShellExecute = $false
        $proc = [System.Diagnostics.Process]::Start($psi)
        $stdoutText = $proc.StandardOutput.ReadToEnd()
        $stderrText = $proc.StandardError.ReadToEnd()
        $proc.WaitForExit()
        $code = $proc.ExitCode
        $out = $stdoutText + $stderrText

        $ok = $true
        if ($expectExit -eq "nonzero") {
            if ($code -eq 0) { $ok = $false }
        }
        elseif ($code -ne [int]$expectExit) {
            $ok = $false
        }
        if ($expectSubstr -ne "" -and $out -notmatch [regex]::Escape($expectSubstr)) {
            $ok = $false
        }

        if ($ok) {
            Write-Host "PASS $name"
            $passed++
        }
        else {
            Write-Host "FAIL $name (exit=$code, output=$out)"
            $failed++
        }
    }
}

Remove-Item -Recurse -Force $work -ErrorAction SilentlyContinue
Write-Host "---"
Write-Host "$passed passed, $failed failed"
if ($failed -gt 0) { exit 1 }
