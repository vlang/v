[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [ValidateSet('msvc', 'gcc', 'tcc')]
    [string]$Compiler,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedCompositeSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedRunnerSha256
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$cases = @(
    [pscustomobject]@{
        Kind = 'native'
        File = 'vlib/x/multiwindow/service_native_win32_contract_red_test.v'
        Name = 'test_win32_w3_late_exact_name_reserves_unavailable_slot_and_stales_old_ids_red'
    }
    [pscustomobject]@{
        Kind = 'native'
        File = 'vlib/x/multiwindow/service_native_win32_contract_red_test.v'
        Name = 'test_win32_native_monitor_dpi_display_change_and_generation_red'
    }
    [pscustomobject]@{
        Kind = 'native'
        File = 'vlib/x/multiwindow/service_native_win32_contract_red_test.v'
        Name = 'test_win32_zero_window_monitor_snapshot_equality_uses_all_public_fields'
    }
    [pscustomobject]@{
        Kind = 'public'
        File = 'vlib/gg/multiwindow_win32_public_services_contract_windows_test.v'
        Name = 'test_win32_public_monitor_projection_and_event_order_red'
    }
    [pscustomobject]@{
        Kind = 'native'
        File = 'vlib/x/multiwindow/event_sequence_exhaustion_test.v'
        Name = 'test_win32_w3_cold_start_sequence_exhaustion_releases_native_ownership'
    }
)

$surface = @(
    [pscustomobject]@{
        Path = 'vlib/x/multiwindow/service_native_win32_contract_red_test.v'
		Hash = 'e71a0856bcbe5b278b9a580c5151faacae66c0f0c3f9b0861c710b2238ed43bc'
    }
    [pscustomobject]@{
        Path = 'vlib/gg/multiwindow_win32_public_services_contract_windows_test.v'
        Hash = 'd08eafb919ae97b185fc480c22f6d990973396152e9a0b3a01035c3e9a30275c'
    }
    [pscustomobject]@{
        Path = 'vlib/x/multiwindow/event_sequence_exhaustion_test.v'
        Hash = 'caa2c020d0d5ea50a57e8949af7f7353e82c18949edbfe226c0855f5c1bd4533'
    }
)
$knownCompositeSha256 = '6f20eb6d5cb7c3a774fbe75eac6c41f9598acfc87cabad2d73e6ed669f15e9c7'

function Get-W3TextSha256 {
    param(
        [Parameter(Mandatory = $true)]
        [AllowEmptyString()]
        [string]$Text
    )

    $bytes = [System.Text.UTF8Encoding]::new($false).GetBytes($Text)
    $sha = [System.Security.Cryptography.SHA256]::Create()
    try {
        return ([System.BitConverter]::ToString($sha.ComputeHash($bytes))).Replace('-', '').ToLowerInvariant()
    } finally {
        $sha.Dispose()
    }
}

function Assert-W3FileHash {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Path,
        [Parameter(Mandatory = $true)]
        [string]$Expected
    )

    if (-not (Test-Path -LiteralPath $Path -PathType Leaf)) {
        throw "W3 GREEN hashed input is missing: $Path"
    }
    $actual = (Get-FileHash -LiteralPath $Path -Algorithm SHA256).Hash.ToLowerInvariant()
    if ($actual -cne $Expected.ToLowerInvariant()) {
        throw "W3 GREEN hash mismatch for ${Path}: expected=$Expected actual=$actual"
    }
    Write-Host "PACKAGE2_W3_GREEN_HASH_OK path=$Path sha256=$actual"
}

$surfaceRecords = @('schema=package2-win32-w3-green-surface-v1')
$surfaceRecords += @($surface | ForEach-Object { "file=$($_.Path)|sha256=$($_.Hash)" })
$surfaceRecords += @($cases | ForEach-Object { "case=$($_.Kind)|$($_.File)|$($_.Name)" })
$actualCompositeSha256 = Get-W3TextSha256 -Text (($surfaceRecords -join "`n") + "`n")
if ($actualCompositeSha256 -cne $knownCompositeSha256 `
    -or $ExpectedCompositeSha256.ToLowerInvariant() -cne $knownCompositeSha256) {
    throw "W3 GREEN composite mismatch: known=$knownCompositeSha256 actual=$actualCompositeSha256 passed=$ExpectedCompositeSha256"
}
foreach ($entry in $surface) {
    Assert-W3FileHash -Path $entry.Path -Expected $entry.Hash
}
Assert-W3FileHash -Path $PSCommandPath -Expected $ExpectedRunnerSha256
Write-Host "PACKAGE2_W3_GREEN_SURFACE_OK sha256=$actualCompositeSha256 cases=$($cases.Count)"

$vexe = (Resolve-Path '.\v.exe').Path
foreach ($case in $cases) {
    $testFile = (Resolve-Path $case.File).Path
    $discoveredTests = @(
        Select-String -LiteralPath $testFile -Pattern '^fn (test_[A-Za-z0-9_]+)\(\)' |
            ForEach-Object { $_.Matches[0].Groups[1].Value }
    )
    if ($discoveredTests -cnotcontains $case.Name) {
        throw "W3 GREEN test was not discovered: $($case.Name) in $($case.File)"
    }

    Write-Host "::group::Win32 W3 GREEN $Compiler $($case.Name)"
    $process = [System.Diagnostics.Process]::new()
    try {
        $startInfo = [System.Diagnostics.ProcessStartInfo]::new()
        $startInfo.FileName = $vexe
        $startInfo.WorkingDirectory = (Get-Location).Path
        $startInfo.UseShellExecute = $false
        $startInfo.RedirectStandardOutput = $true
        $startInfo.RedirectStandardError = $true
        $startInfo.CreateNoWindow = $true
        foreach ($argument in @(
            '-cc', $Compiler,
            '-no-retry-compilation',
            '-no-parallel',
            '-subsystem', 'console',
            '-d', 'gg_multiwindow',
            'test', '-run-only', $case.Name, $testFile
        )) {
            [void]$startInfo.ArgumentList.Add($argument)
        }
        $process.StartInfo = $startInfo
        if (-not $process.Start()) {
            throw "failed to start W3 GREEN test $($case.Name)"
        }

        $stdoutTask = $process.StandardOutput.ReadToEndAsync()
        $stderrTask = $process.StandardError.ReadToEndAsync()
        $timedOut = -not $process.WaitForExit(180000)
        if ($timedOut) {
            $process.Kill($true)
            [void]$process.WaitForExit(5000)
        } else {
            $process.WaitForExit()
        }
        $stdout = $stdoutTask.GetAwaiter().GetResult()
        $stderr = $stderrTask.GetAwaiter().GetResult()
        if ($stdout) {
            Write-Host $stdout.TrimEnd()
        }
        if ($stderr) {
            Write-Host $stderr.TrimEnd()
        }
        if ($timedOut) {
            throw "W3 GREEN test timed out: $($case.Name)"
        }

        $combined = "$stdout`n$stderr"
        $summaryLines = @(
            $combined -split "`r?`n" |
                ForEach-Object { $_.Trim() } |
                Where-Object { $_ -cmatch '^Summary for all V _test\.v files:' }
        )
        $exactSummary = $summaryLines.Count -eq 1 `
            -and $summaryLines[0] -cmatch '^Summary for all V _test\.v files: 1 passed, 1 total\.(?: .*)?$'
        $selectionMismatch = $combined -match '(?im)^\s*retrying\s' `
            -or $combined -match '(?im)\bskipped?\b' `
            -or $combined -match '(?im)\b0 passed\b'
        if ($process.ExitCode -ne 0 -or -not $exactSummary -or $selectionMismatch) {
            $summary = if ($summaryLines.Count -eq 0) {
                '<missing>'
            } else {
                $summaryLines -join ' | '
            }
            throw "W3 GREEN gate failed: compiler=$Compiler kind=$($case.Kind) case=$($case.Name) exit=$($process.ExitCode) summary=$summary"
        }
        Write-Host "PACKAGE2_W3_GREEN_PASS compiler=$Compiler kind=$($case.Kind) case=$($case.Name) passed=1 total=1"
    } finally {
        $process.Dispose()
        Write-Host '::endgroup::'
    }
}
