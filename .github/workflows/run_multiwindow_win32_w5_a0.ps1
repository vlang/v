[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [ValidateSet('msvc', 'gcc', 'tcc')]
    [string]$Compiler,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedHeaderSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedMainSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedTupleSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedRunnerSha256
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$header = 'vlib/x/multiwindow/testdata/win32_raw_input_w5_preflight.h'
$main = 'vlib/x/multiwindow/testdata/win32_raw_input_w5_preflight.c'
$knownHeaderSha256 = '9decdc2e825c91da2b9d2ce0c98cb8ccdad5c4800a690863d166135eb188e916'
$knownMainSha256 = '5f30869ae97f680032e622273ea69986d612cc82cad2adf53e4c3b1415154301'
$knownTupleSha256 = '99ef24bf101dc83543f26f86cb23a80c82607e0f1b35c32e9dbb431368ecc7c4'

$runtimeMarkers = @(
    'PACKAGE2_W5_A0_IDENTITY=win32_raw_input_sendinput_preflight'
    'PACKAGE2_W5_A0_FAMILY=raw_input_environment'
    'PACKAGE2_W5_A0_REACHED=sendinput_rawinput_correlation'
    'PACKAGE2_W5_A0_SOURCE_OK=injected_mouse_extra_tag'
    'PACKAGE2_W5_A0_RAW_OK=real_hrawinput_relative'
    'PACKAGE2_W5_A0_CLEANUP_OK=restored'
    'PACKAGE2_W5_A0_SUMMARY=accepted:1 rejected:0 total:1'
    'PACKAGE2_W5_A0_TERMINAL=native_pass:raw_input_environment'
)

$fatalPattern = '(?i)(fatal error|unhandled exception|access violation|STATUS_ACCESS_VIOLATION|0xC0000005|segmentation fault|stack overflow|illegal instruction|abort trap|process crashed|application crashed)'
$compilerDiagnosticPattern = '(?im)(fatal error|error C[0-9]{4}|warning C[0-9]{4}|warning:|error:|undefined reference|unresolved external symbol|LNK[0-9]{4})'
$crashExitCodes = @(
    -2147483645,
    -1073741819,
    -1073741795,
    -1073741571,
    -1073741510,
    -1073740940,
    -1073740791
)

function Get-W5A0TextSha256 {
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

function Assert-W5A0FileHash {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Path,
        [Parameter(Mandatory = $true)]
        [string]$Expected
    )

    if (-not (Test-Path -LiteralPath $Path -PathType Leaf)) {
        throw "W5 A0 hashed input is missing: $Path"
    }
    $actual = (Get-FileHash -LiteralPath $Path -Algorithm SHA256).Hash.ToLowerInvariant()
    $normalizedExpected = $Expected.ToLowerInvariant()
    if ($actual -cne $normalizedExpected) {
        throw "W5 A0 hash mismatch for ${Path}: expected=$normalizedExpected actual=$actual"
    }
    Write-Host "PACKAGE2_W5_A0_HASH_OK path=$Path sha256=$actual"
}

function Test-W5A0SourceTokenAbsent {
    param(
        [Parameter(Mandatory = $true)]
        [AllowEmptyString()]
        [string]$Text
    )

    $matched = [System.Text.RegularExpressions.Regex]::IsMatch(
        $Text,
        '(?<![A-Za-z0-9_])(?:ShowCursor|SetCursor|SetSystemCursor|DestroyCursor|SetClassLong(?:Ptr)?(?:A|W)?)(?![A-Za-z0-9_])',
        [System.Text.RegularExpressions.RegexOptions]::CultureInvariant
    )
    return -not $matched
}

function Test-W5A0SourceTokenGate {
    $cleanAccepted = 0
    foreach ($fixture in @(
        'int cursor_state = 0;',
        'SetCursorPos(1, 2);',
        'window_class.hCursor = snapshot.hCursor;'
    )) {
        if (Test-W5A0SourceTokenAbsent -Text $fixture) {
            $cleanAccepted++
        }
    }
    $forbiddenTokens = @(
        'ShowCursor',
        'SetCursor',
        'SetSystemCursor',
        'DestroyCursor',
        'SetClassLong',
        'SetClassLongA',
        'SetClassLongW',
        'SetClassLongPtr',
        'SetClassLongPtrA',
        'SetClassLongPtrW'
    )
    $forbiddenRejected = 0
    foreach ($token in $forbiddenTokens) {
        foreach ($fixture in @(
            ('{0}(0);' -f $token),
            ('"{0}"' -f $token),
            ('/* {0} */' -f $token)
        )) {
            if (-not (Test-W5A0SourceTokenAbsent -Text $fixture)) {
                $forbiddenRejected++
            }
        }
    }
    if ($cleanAccepted -ne 3 -or $forbiddenRejected -ne 30) {
        throw 'W5 A0 source-token self-test failed'
    }
    Write-Host 'PACKAGE2_W5_A0_SOURCE_TOKEN_SELF_TEST accepted=3 rejected=30 total=33'
}

function Assert-W5A0FailedReap {
    param(
        [Parameter(Mandatory = $true)]
        [AllowEmptyString()]
        [string]$Failure
    )

    if ($Failure) {
        throw "W5 A0 process supervision failed: $Failure"
    }
}

function Receive-W5A0ProcessOutput {
    param(
        [Parameter(Mandatory = $true)]
        [System.Threading.Tasks.Task]$StdoutTask,
        [Parameter(Mandatory = $true)]
        [System.Threading.Tasks.Task]$StderrTask,
        [int]$TimeoutMilliseconds = 5000
    )

    $errors = [System.Collections.Generic.List[string]]::new()
    $combined = [System.Threading.Tasks.Task]::WhenAll(
        [System.Threading.Tasks.Task[]]@($StdoutTask, $StderrTask)
    )
    try {
        if (-not $combined.Wait($TimeoutMilliseconds)) {
            $errors.Add("redirected output did not close within $TimeoutMilliseconds ms")
        }
    } catch {
        Write-Verbose "W5 A0 output aggregate fault: $($_.Exception.Message)"
    }

    $stdout = ''
    if ($StdoutTask.IsCompletedSuccessfully) {
        $stdout = [string]$StdoutTask.GetAwaiter().GetResult()
    } elseif ($StdoutTask.IsFaulted) {
        $errors.Add("stdout drain failed: $($StdoutTask.Exception.GetBaseException().Message)")
    } elseif ($StdoutTask.IsCanceled) {
        $errors.Add('stdout drain was cancelled')
    } else {
        $errors.Add('stdout drain remained incomplete')
    }

    $stderr = ''
    if ($StderrTask.IsCompletedSuccessfully) {
        $stderr = [string]$StderrTask.GetAwaiter().GetResult()
    } elseif ($StderrTask.IsFaulted) {
        $errors.Add("stderr drain failed: $($StderrTask.Exception.GetBaseException().Message)")
    } elseif ($StderrTask.IsCanceled) {
        $errors.Add('stderr drain was cancelled')
    } else {
        $errors.Add('stderr drain remained incomplete')
    }

    return [pscustomobject]@{
        Completed = $StdoutTask.IsCompletedSuccessfully -and $StderrTask.IsCompletedSuccessfully
        Error = $errors -join '; '
        Stdout = $stdout
        Stderr = $stderr
    }
}

function Invoke-W5A0BoundedProcess {
    param(
        [Parameter(Mandatory = $true)]
        [string]$FileName,
        [Parameter(Mandatory = $true)]
        [AllowEmptyCollection()]
        [string[]]$Arguments,
        [Parameter(Mandatory = $true)]
        [string]$WorkingDirectory,
        [int]$TimeoutSeconds,
        [int]$ReapTimeoutMilliseconds = 5000,
        [int]$DrainTimeoutMilliseconds = 5000
    )

    $startInfo = [System.Diagnostics.ProcessStartInfo]::new()
    $startInfo.FileName = $FileName
    $startInfo.WorkingDirectory = $WorkingDirectory
    $startInfo.UseShellExecute = $false
    $startInfo.RedirectStandardOutput = $true
    $startInfo.RedirectStandardError = $true
    $startInfo.CreateNoWindow = $true
    foreach ($argument in $Arguments) {
        [void]$startInfo.ArgumentList.Add($argument)
    }

    $process = [System.Diagnostics.Process]::new()
    $process.StartInfo = $startInfo
    $started = $false
    $stdoutTask = $null
    $stderrTask = $null
    $timedOut = $false
    $exitCode = $null
    $failedReap = ''
    $infrastructureErrors = [System.Collections.Generic.List[string]]::new()
    try {
        if (-not $process.Start()) {
            throw 'child process did not start'
        }
        $started = $true
        $stdoutTask = $process.StandardOutput.ReadToEndAsync()
        $stderrTask = $process.StandardError.ReadToEndAsync()
        if (-not $process.WaitForExit($TimeoutSeconds * 1000)) {
            $timedOut = $true
            try {
                $process.Kill($true)
            } catch {
                $infrastructureErrors.Add("watchdog tree kill failed: $($_.Exception.Message)")
            }
            try {
                if (-not $process.WaitForExit($ReapTimeoutMilliseconds)) {
                    $failedReap = "child did not reap within $ReapTimeoutMilliseconds ms after tree kill"
                    $infrastructureErrors.Add($failedReap)
                }
            } catch {
                $failedReap = "watchdog reap failed: $($_.Exception.Message)"
                $infrastructureErrors.Add($failedReap)
            }
        } else {
            $process.WaitForExit()
        }
        if ($process.HasExited) {
            $exitCode = $process.ExitCode
        }
    } catch {
        $infrastructureErrors.Add($_.Exception.Message)
        if ($started) {
            try {
                if (-not $process.HasExited) {
                    $process.Kill($true)
                }
            } catch {
                $infrastructureErrors.Add("exception cleanup tree kill failed: $($_.Exception.Message)")
            }
            try {
                if (-not $process.WaitForExit($ReapTimeoutMilliseconds)) {
                    $failedReap = "child did not reap within $ReapTimeoutMilliseconds ms after exception cleanup"
                    $infrastructureErrors.Add($failedReap)
                }
            } catch {
                $failedReap = "exception cleanup reap failed: $($_.Exception.Message)"
                $infrastructureErrors.Add($failedReap)
            }
            if ($process.HasExited) {
                $exitCode = $process.ExitCode
            }
        }
    }

    $stdout = ''
    $stderr = ''
    if ($null -ne $stdoutTask -and $null -ne $stderrTask) {
        $drain = Receive-W5A0ProcessOutput -StdoutTask $stdoutTask `
            -StderrTask $stderrTask -TimeoutMilliseconds $DrainTimeoutMilliseconds
        $stdout = $drain.Stdout
        $stderr = $drain.Stderr
        if (-not $drain.Completed) {
            $infrastructureErrors.Add($drain.Error)
        }
    } else {
        $infrastructureErrors.Add('redirected output tasks were not established')
    }
    $process.Dispose()
    Assert-W5A0FailedReap -Failure $failedReap

    return [pscustomobject]@{
        ExitCode = $exitCode
        TimedOut = $timedOut
        InfrastructureError = $infrastructureErrors -join '; '
        Stdout = $stdout
        Stderr = $stderr
    }
}

function Write-W5A0ProcessOutput {
    param(
        [Parameter(Mandatory = $true)]
        [pscustomobject]$Result
    )

    if ($Result.Stdout) {
        Write-Host $Result.Stdout.TrimEnd()
    }
    if ($Result.Stderr) {
        [Console]::Error.WriteLine($Result.Stderr.TrimEnd())
    }
}

function Get-W5A0StrictStderrLines {
    param(
        [Parameter(Mandatory = $true)]
        [AllowEmptyString()]
        [string]$Text
    )

    $normalized = $Text.Replace("`r`n", "`n")
    if ($normalized.Contains("`r")) {
        return [pscustomobject]@{ Valid = $false; Lines = [string[]]@() }
    }
    $segments = [string[]]$normalized.Split(
        [char[]]@([char]10),
        [System.StringSplitOptions]::None
    )
    if ($segments.Count -eq 0 -or $segments[$segments.Count - 1] -cne '') {
        return [pscustomobject]@{ Valid = $false; Lines = $segments }
    }
    if ($segments.Count -eq 1) {
        return [pscustomobject]@{ Valid = $true; Lines = [string[]]@() }
    }
    $lines = [string[]]$segments[0..($segments.Count - 2)]
    return [pscustomobject]@{ Valid = $true; Lines = $lines }
}

function Get-W5A0Classification {
    param(
        [Parameter(Mandatory = $true)]
        [pscustomobject]$Result
    )

    $text = ([string]$Result.Stdout) + "`n" + ([string]$Result.Stderr)
    if ($Result.TimedOut) {
        return [pscustomobject]@{ Kind = 'TimeoutFailure'; Detail = 'watchdog timeout' }
    }
    if ($Result.InfrastructureError) {
        return [pscustomobject]@{
            Kind = 'InfrastructureFailure'
            Detail = $Result.InfrastructureError
        }
    }
    if ($null -eq $Result.ExitCode) {
        return [pscustomobject]@{ Kind = 'UnknownExit'; Detail = 'no exit code' }
    }
    if ($Result.ExitCode -in $crashExitCodes -or $text -match $fatalPattern) {
        return [pscustomobject]@{
            Kind = 'FatalFailure'
            Detail = "exit=$($Result.ExitCode)"
        }
    }
    if ([string]$Result.Stderr -cmatch '(?m)^PACKAGE2_W5_INFRA=') {
        return [pscustomobject]@{
            Kind = 'InfrastructureFailure'
            Detail = 'child emitted PACKAGE2_W5_INFRA'
        }
    }
    if ([string]$Result.Stdout -cne '') {
        return [pscustomobject]@{
            Kind = 'StdoutFailure'
            Detail = 'child stdout must be exactly empty'
        }
    }
    $parsed = Get-W5A0StrictStderrLines -Text ([string]$Result.Stderr)
    if (-not $parsed.Valid -or $parsed.Lines.Count -ne $runtimeMarkers.Count) {
        return [pscustomobject]@{
            Kind = 'TranscriptFailure'
            Detail = 'runtime stderr framing or line count mismatch'
        }
    }
    for ($index = 0; $index -lt $runtimeMarkers.Count; $index++) {
        if ($parsed.Lines[$index] -cne $runtimeMarkers[$index]) {
            return [pscustomobject]@{
                Kind = 'TranscriptFailure'
                Detail = "runtime stderr mismatch at line $($index + 1)"
            }
        }
    }
    if ($Result.ExitCode -ne 0) {
        return [pscustomobject]@{
            Kind = 'ExitFailure'
            Detail = "expected=0 actual=$($Result.ExitCode)"
        }
    }
    return [pscustomobject]@{ Kind = 'NativePreflightPass'; Detail = 'exit=0' }
}

function New-W5A0SyntheticResult {
    param(
        [string[]]$Lines,
        [int]$ExitCode = 0,
        [AllowEmptyString()]
        [string]$Stdout = '',
        [bool]$TimedOut = $false,
        [AllowEmptyString()]
        [string]$InfrastructureError = ''
    )

    $stderr = if ($null -eq $Lines) { '' } else { ($Lines -join "`r`n") + "`r`n" }
    return [pscustomobject]@{
        ExitCode = $ExitCode
        TimedOut = $TimedOut
        InfrastructureError = $InfrastructureError
        Stdout = $Stdout
        Stderr = $stderr
    }
}

function Test-W5A0Classifier {
    $valid = [string[]]$runtimeMarkers
    $withoutIdentity = [string[]]$valid[1..($valid.Count - 1)]
    $withoutReached = [string[]](@($valid[0], $valid[1]) + $valid[3..($valid.Count - 1)])
    $withoutSource = [string[]](@($valid[0], $valid[1], $valid[2]) + $valid[4..($valid.Count - 1)])
    $withoutRaw = [string[]](@($valid[0], $valid[1], $valid[2], $valid[3]) + $valid[5..($valid.Count - 1)])
    $withoutCleanup = [string[]](@($valid[0], $valid[1], $valid[2], $valid[3], $valid[4]) + $valid[6..($valid.Count - 1)])
    $duplicateIdentity = [string[]](@($valid[0]) + $valid)
    $reordered = [string[]](@($valid[1], $valid[0]) + $valid[2..($valid.Count - 1)])
    $whitespace = [string[]](@((' ' + $valid[0])) + $valid[1..($valid.Count - 1)])
    $synthetic = @(
        @{ Name = 'valid'; Expected = 'NativePreflightPass'; Result = New-W5A0SyntheticResult -Lines $valid }
        @{ Name = 'missing identity'; Expected = 'TranscriptFailure'; Result = New-W5A0SyntheticResult -Lines $withoutIdentity }
        @{ Name = 'duplicate identity'; Expected = 'TranscriptFailure'; Result = New-W5A0SyntheticResult -Lines $duplicateIdentity }
        @{ Name = 'reordered'; Expected = 'TranscriptFailure'; Result = New-W5A0SyntheticResult -Lines $reordered }
        @{ Name = 'extra line'; Expected = 'TranscriptFailure'; Result = New-W5A0SyntheticResult -Lines @($valid + 'unexpected') }
        @{ Name = 'whitespace'; Expected = 'TranscriptFailure'; Result = New-W5A0SyntheticResult -Lines $whitespace }
        @{ Name = 'stdout spoof'; Expected = 'StdoutFailure'; Result = New-W5A0SyntheticResult -Lines $valid -Stdout ($valid[0] + "`n") }
        @{ Name = 'infra exit zero'; Expected = 'InfrastructureFailure'; Result = New-W5A0SyntheticResult -Lines @('PACKAGE2_W5_INFRA=synthetic', 'PACKAGE2_W5_A0_TERMINAL=infra:raw_input_environment') }
        @{ Name = 'pass exit one'; Expected = 'ExitFailure'; Result = New-W5A0SyntheticResult -Lines $valid -ExitCode 1 }
        @{ Name = 'wrong summary'; Expected = 'TranscriptFailure'; Result = New-W5A0SyntheticResult -Lines @($valid[0..5] + 'PACKAGE2_W5_A0_SUMMARY=accepted:0 rejected:1 total:1' + $valid[7]) }
        @{ Name = 'wrong terminal'; Expected = 'TranscriptFailure'; Result = New-W5A0SyntheticResult -Lines @($valid[0..6] + 'PACKAGE2_W5_A0_TERMINAL=infra:raw_input_environment') }
        @{ Name = 'timeout'; Expected = 'TimeoutFailure'; Result = New-W5A0SyntheticResult -Lines $valid -TimedOut $true }
        @{ Name = 'crash'; Expected = 'FatalFailure'; Result = New-W5A0SyntheticResult -Lines $valid -ExitCode (-1073741819) }
        @{ Name = 'supervisor'; Expected = 'InfrastructureFailure'; Result = New-W5A0SyntheticResult -Lines $valid -InfrastructureError 'synthetic supervisor failure' }
        @{ Name = 'drain'; Expected = 'InfrastructureFailure'; Result = New-W5A0SyntheticResult -Lines $valid -InfrastructureError 'synthetic redirected output drain failure' }
        @{ Name = 'missing reached'; Expected = 'TranscriptFailure'; Result = New-W5A0SyntheticResult -Lines $withoutReached }
        @{ Name = 'duplicate reached'; Expected = 'TranscriptFailure'; Result = New-W5A0SyntheticResult -Lines @($valid[0..2] + $valid[2] + $valid[3..($valid.Count - 1)]) }
        @{ Name = 'missing source'; Expected = 'TranscriptFailure'; Result = New-W5A0SyntheticResult -Lines $withoutSource }
        @{ Name = 'missing raw'; Expected = 'TranscriptFailure'; Result = New-W5A0SyntheticResult -Lines $withoutRaw }
        @{ Name = 'missing cleanup'; Expected = 'TranscriptFailure'; Result = New-W5A0SyntheticResult -Lines $withoutCleanup }
        @{ Name = 'product red'; Expected = 'TranscriptFailure'; Result = New-W5A0SyntheticResult -Lines @($valid[0..6] + 'PACKAGE2_RED_TERMINAL=behavioral_red:raw_input_environment') }
        @{ Name = 'skip'; Expected = 'TranscriptFailure'; Result = New-W5A0SyntheticResult -Lines @($valid + 'SKIP') }
    )

    if ($synthetic.Count -ne 22) {
        throw "W5 A0 classifier self-test fixture count changed: $($synthetic.Count)"
    }
    $accepted = 0
    foreach ($item in $synthetic) {
        $classification = Get-W5A0Classification -Result $item.Result
        if ($classification.Kind -cne $item.Expected) {
            throw "W5 A0 classifier self-test '$($item.Name)' expected $($item.Expected), got $($classification.Kind)"
        }
        if ($classification.Kind -ceq 'NativePreflightPass') {
            $accepted++
        }
    }
    if ($accepted -ne 1) {
        throw "W5 A0 classifier self-test accepted $accepted cases instead of one"
    }
    Write-Host "PACKAGE2_W5_A0_CLASSIFIER_SELF_TEST accepted=$accepted rejected=$($synthetic.Count - $accepted) total=$($synthetic.Count)"
}

function Test-W5A0FailedReapGate {
    $failure = 'child did not reap within 0 ms after synthetic tree kill'
    $hardAbort = $false
    $nextCaseStarted = $false
    try {
        Assert-W5A0FailedReap -Failure $failure
        $nextCaseStarted = $true
    } catch {
        $expected = "W5 A0 process supervision failed: $failure"
        if ($_.Exception.Message -cne $expected) {
            throw
        }
        $hardAbort = $true
    }
    if (-not $hardAbort -or $nextCaseStarted) {
        throw 'W5 A0 failed-reap self-test did not hard-abort before the next case'
    }
    Write-Host 'PACKAGE2_W5_A0_FAILED_REAP_SELF_TEST injected=true hard_abort=true next_case_started=false'
}

function Assert-W5A0CommandGreen {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Label,
        [Parameter(Mandatory = $true)]
        [pscustomobject]$Result,
        [switch]$RejectCompilerDiagnostics
    )

    $text = ([string]$Result.Stdout) + "`n" + ([string]$Result.Stderr)
    if ($Result.TimedOut) {
        throw "$Label timed out"
    }
    if ($Result.InfrastructureError) {
        throw "$Label infrastructure failure: $($Result.InfrastructureError)"
    }
    if ($null -eq $Result.ExitCode -or $Result.ExitCode -ne 0 `
        -or $Result.ExitCode -in $crashExitCodes -or $text -match $fatalPattern) {
        throw "$Label failed with exit $($Result.ExitCode)"
    }
    if ($RejectCompilerDiagnostics -and $text -match $compilerDiagnosticPattern) {
        throw "$Label emitted a compiler/linker warning or error diagnostic"
    }
}

function Get-W5A0CompilerIdentity {
    param(
        [Parameter(Mandatory = $true)]
        [string]$SelectedCompiler,
        [Parameter(Mandatory = $true)]
        [string]$WorkingDirectory
    )

    $compilerPath = ''
    $versionArguments = @()
    if ($SelectedCompiler -ceq 'tcc') {
        if (-not $env:PINNED_TCC) {
            throw 'PINNED_TCC is not set for W5 A0 TCC validation'
        }
        $compilerPath = (Resolve-Path -LiteralPath $env:PINNED_TCC).Path
        $resolvedTcc = (Get-Command tcc.exe -CommandType Application -ErrorAction Stop).Source
        if ([IO.Path]::GetFullPath($resolvedTcc) -cne [IO.Path]::GetFullPath($compilerPath)) {
            throw "tcc.exe resolved to '$resolvedTcc', expected PINNED_TCC '$compilerPath'"
        }
        $versionArguments = @('-v')
    } elseif ($SelectedCompiler -ceq 'gcc') {
        $selectedGccCommands = @(
            @(Get-Command x86_64-w64-mingw32-gcc.exe `
                -CommandType Application -All -ErrorAction SilentlyContinue) |
                Select-Object -First 1
        )
        if ($selectedGccCommands.Count -ne 1) {
            throw 'W5 A0 target-prefixed GCC was not found on PATH'
        }
        $gccSource = [string]$selectedGccCommands[0].Source
        if ([string]::IsNullOrWhiteSpace($gccSource)) {
            throw 'W5 A0 target-prefixed GCC resolved to an empty source'
        }
        $resolvedGccSource = (Resolve-Path -LiteralPath $gccSource `
            -ErrorAction Stop).Path
        $compilerPath = [IO.Path]::GetFullPath($resolvedGccSource)
        if (-not (Test-Path -LiteralPath $compilerPath -PathType Leaf)) {
            throw "W5 A0 selected GCC is not a file: '$compilerPath'"
        }
        $versionArguments = @('--version')
        $machine = Invoke-W5A0BoundedProcess -FileName $compilerPath `
            -Arguments @('-dumpmachine') -WorkingDirectory $WorkingDirectory `
            -TimeoutSeconds 30
        Write-W5A0ProcessOutput -Result $machine
        Assert-W5A0CommandGreen -Label 'W5 A0 GCC target identity' -Result $machine
        $target = ([string]$machine.Stdout).Trim()
        if ($target -cnotmatch '^x86_64(?:-w64)?-mingw32$') {
            throw "W5 A0 GCC target is not x64 MinGW: '$target'"
        }
    } else {
        $compilerPath = (Get-Command cl.exe -CommandType Application -ErrorAction Stop).Source
        $versionArguments = @('/?')
        if ($env:VSCMD_ARG_TGT_ARCH -cne 'x64') {
            throw "W5 A0 MSVC target architecture is not x64: '$env:VSCMD_ARG_TGT_ARCH'"
        }
    }

    $versionResult = Invoke-W5A0BoundedProcess -FileName $compilerPath `
        -Arguments $versionArguments -WorkingDirectory $WorkingDirectory `
        -TimeoutSeconds 30
    Write-W5A0ProcessOutput -Result $versionResult
    Assert-W5A0CommandGreen -Label "W5 A0 $SelectedCompiler version" `
        -Result $versionResult
    $versionLines = @(
        (([string]$versionResult.Stdout) + "`n" + ([string]$versionResult.Stderr)) `
            -split "\r?\n" | ForEach-Object { $_.Trim() } | Where-Object { $_ -ne '' }
    )
    if ($versionLines.Count -eq 0) {
        throw "W5 A0 $SelectedCompiler version output was empty"
    }
    if ($SelectedCompiler -ceq 'msvc') {
        $msvcVersionLines = @(
            $versionLines | Where-Object {
                $_ -cmatch '^Microsoft \(R\) C/C\+\+ Optimizing Compiler Version [0-9]+(?:\.[0-9]+){2,3} for x64$'
            }
        )
        if ($msvcVersionLines.Count -ne 1) {
            throw "W5 A0 expected exactly one MSVC x64 banner, found $($msvcVersionLines.Count)"
        }
        $version = [string]$msvcVersionLines[0]
    } else {
        $version = [string]$versionLines[0]
    }
    $sha256 = (Get-FileHash -LiteralPath $compilerPath -Algorithm SHA256).Hash.ToLowerInvariant()
    $encodedPath = [Uri]::EscapeDataString([IO.Path]::GetFullPath($compilerPath))
    $encodedVersion = [Uri]::EscapeDataString($version)
    Write-Host "PACKAGE2_W5_A0_COMPILER_IDENTITY compiler=$SelectedCompiler resolved=$encodedPath sha256=$sha256 version=$encodedVersion"
    return [pscustomobject]@{
        Path = $compilerPath
        Version = $version
        Sha256 = $sha256
    }
}

if ($ExpectedHeaderSha256.ToLowerInvariant() -cne $knownHeaderSha256) {
    throw "W5 A0 header parameter mismatch: expected=$knownHeaderSha256 passed=$ExpectedHeaderSha256"
}
if ($ExpectedMainSha256.ToLowerInvariant() -cne $knownMainSha256) {
    throw "W5 A0 main parameter mismatch: expected=$knownMainSha256 passed=$ExpectedMainSha256"
}
if ($ExpectedTupleSha256.ToLowerInvariant() -cne $knownTupleSha256) {
    throw "W5 A0 tuple parameter mismatch: expected=$knownTupleSha256 passed=$ExpectedTupleSha256"
}

Assert-W5A0FileHash -Path $header -Expected $ExpectedHeaderSha256
Assert-W5A0FileHash -Path $main -Expected $ExpectedMainSha256
Test-W5A0SourceTokenGate
foreach ($sourceInput in @($header, $main)) {
    $sourceText = [System.IO.File]::ReadAllText(
        (Resolve-Path -LiteralPath $sourceInput -ErrorAction Stop).Path
    )
    if (-not (Test-W5A0SourceTokenAbsent -Text $sourceText)) {
        throw "W5 A0 forbidden cursor-state mutation token in source: $sourceInput"
    }
    Write-Host "PACKAGE2_W5_A0_SOURCE_TOKEN_OK path=$sourceInput"
}
Assert-W5A0FileHash -Path $PSCommandPath -Expected $ExpectedRunnerSha256

$tupleRecords = @(
    'schema=package2-win32-w5-a0-v1'
    "file=$header|sha256=$knownHeaderSha256"
    "file=$main|sha256=$knownMainSha256"
    'case=win32_raw_input_sendinput_preflight|family=raw_input_environment'
)
$tupleRecords += @($runtimeMarkers | ForEach-Object { "marker=$_" })
$tupleSha256 = Get-W5A0TextSha256 -Text (($tupleRecords -join "`n") + "`n")
if ($tupleSha256 -cne $knownTupleSha256) {
    throw "W5 A0 ordered tuple mismatch: expected=$knownTupleSha256 actual=$tupleSha256"
}
Write-Host "PACKAGE2_W5_A0_TUPLE_OK sha256=$tupleSha256 markers=$($runtimeMarkers.Count)"

Test-W5A0Classifier
Test-W5A0FailedReapGate

$tempRoot = if ($env:RUNNER_TEMP) {
    $env:RUNNER_TEMP
} else {
    [System.IO.Path]::GetTempPath()
}
$tempDir = Join-Path $tempRoot "multiwindow_w5_a0_$([guid]::NewGuid().ToString('N'))"
[void](New-Item -ItemType Directory -Path $tempDir)
$sourcePath = (Resolve-Path -LiteralPath $main).Path
$sourceDirectory = Split-Path -Parent $sourcePath
$probeExe = Join-Path $tempDir 'win32_raw_input_w5_preflight.exe'

try {
    $compilerIdentity = Get-W5A0CompilerIdentity -SelectedCompiler $Compiler `
        -WorkingDirectory $tempDir
    if (Test-Path -LiteralPath $probeExe) {
        throw "W5 A0 probe output was not fresh: $probeExe"
    }

    $compileArguments = @()
    if ($Compiler -ceq 'tcc') {
        $compileArguments = @(
            "-I$sourceDirectory",
            $sourcePath,
            '-o', $probeExe,
            '-luser32'
        )
    } elseif ($Compiler -ceq 'gcc') {
        $compileArguments = @(
            '-m64',
            '-std=c11',
            '-O0',
            '-Wall',
            '-Wextra',
            "-I$sourceDirectory",
            $sourcePath,
            '-o', $probeExe,
            '-luser32'
        )
    } else {
        $objectPath = Join-Path $tempDir 'win32_raw_input_w5_preflight.obj'
        $pdbPath = Join-Path $tempDir 'win32_raw_input_w5_preflight.pdb'
        $compileArguments = @(
            '/nologo',
            '/TC',
            "/I$sourceDirectory",
            "/Fe:$probeExe",
            "/Fo:$objectPath",
            $sourcePath,
            '/link',
            '/SUBSYSTEM:CONSOLE',
            '/INCREMENTAL:NO',
            "/PDB:$pdbPath",
            'user32.lib'
        )
    }

    Write-Host "::group::Win32 W5 A0 compile $Compiler"
    try {
        $compile = Invoke-W5A0BoundedProcess -FileName $compilerIdentity.Path `
            -Arguments $compileArguments -WorkingDirectory $tempDir `
            -TimeoutSeconds 120
        Write-W5A0ProcessOutput -Result $compile
        Assert-W5A0CommandGreen -Label "W5 A0 compile/$Compiler" `
            -Result $compile -RejectCompilerDiagnostics
        if (-not (Test-Path -LiteralPath $probeExe -PathType Leaf) `
            -or (Get-Item -LiteralPath $probeExe).Length -le 0) {
            throw "W5 A0 compiler did not produce a fresh nonempty executable: $probeExe"
        }
        Write-Host "PACKAGE2_W5_A0_COMPILE_PASS compiler=$Compiler"
    } finally {
        Write-Host '::endgroup::'
    }

    Write-Host "::group::Win32 W5 A0 native preflight $Compiler"
    try {
        $run = Invoke-W5A0BoundedProcess -FileName $probeExe -Arguments @() `
            -WorkingDirectory $tempDir -TimeoutSeconds 30
        Write-W5A0ProcessOutput -Result $run
        $classification = Get-W5A0Classification -Result $run
        if ($classification.Kind -cne 'NativePreflightPass') {
            throw "W5 A0 native gate rejected ${Compiler}: $($classification.Kind): $($classification.Detail)"
        }
        Write-Host "PACKAGE2_W5_A0_CASE_ACCEPT compiler=$Compiler classification=NativePreflightPass"
        Write-Host "PACKAGE2_W5_A0_RUNNER_SUMMARY compiler=$Compiler accepted=1 rejected=0 total=1"
    } finally {
        Write-Host '::endgroup::'
    }
} finally {
    Remove-Item -LiteralPath $tempDir -Recurse -Force -ErrorAction SilentlyContinue
}
