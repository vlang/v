[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [ValidateSet('msvc', 'gcc', 'tcc')]
    [string]$Compiler,

    [Parameter(Mandatory = $true)]
    [ValidateSet('Red', 'Green')]
    [string]$Expectation,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedOracleSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedNativeTestSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedPublicTestSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedNoOptProbeSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedServiceBackendSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedEventDeliverySha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedWin32BackendSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedWin32ServiceBackendSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedWin32ServiceNativeSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedRedSurfaceSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedGreenSurfaceSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedRunnerSha256
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$oracle = 'vlib/x/multiwindow/testdata/win32_nonreadback_test_oracle.h'
$nativeTest = 'vlib/x/multiwindow/service_native_win32_contract_red_test.v'
$publicTest = 'vlib/gg/multiwindow_win32_public_services_contract_windows_test.v'
$noOptProbe = 'vlib/gg/testdata/multiwindow_win32_clipboard_no_optin_probe.v'
$surfacePaths = @(
    'vlib/x/multiwindow/service_backend.v'
    'vlib/x/multiwindow/event_delivery.v'
    'vlib/x/multiwindow/win32_backend.c.v'
    'vlib/x/multiwindow/win32_service_backend.c.v'
    'vlib/x/multiwindow/win32_service_native.h'
)

$cases = @(
    [pscustomobject]@{
        Kind = 'native'
        File = $nativeTest
        Name = 'test_win32_native_cf_unicodetext_roundtrip_exact_limit_and_terminal_queue_red'
        Family = 'clipboard_unicode_limit'
    }
    [pscustomobject]@{
        Kind = 'native'
        File = $nativeTest
        Name = 'test_win32_native_clipboard_malformed_read_bounds_red'
        Family = 'clipboard_malformed_bounds'
    }
    [pscustomobject]@{
        Kind = 'native'
        File = $nativeTest
        Name = 'test_win32_native_clipboard_exact_utf8_limit_and_over_red'
        Family = 'clipboard_utf8_limit'
    }
    [pscustomobject]@{
        Kind = 'native'
        File = $nativeTest
        Name = 'test_win32_native_clipboard_contention_retry_success_red'
        Family = 'clipboard_contention_retry'
    }
    [pscustomobject]@{
        Kind = 'native'
        File = $nativeTest
        Name = 'test_win32_native_clipboard_fifo_head_only_red'
        Family = 'clipboard_fifo'
    }
    [pscustomobject]@{
        Kind = 'native'
        File = $nativeTest
        Name = 'test_win32_native_clipboard_real_wm_close_global_order_red'
        Family = 'clipboard_global_order'
    }
    [pscustomobject]@{
        Kind = 'native'
        File = $nativeTest
        Name = 'test_win32_native_clipboard_occupancy_timeout_failure_and_cancel_red'
        Family = 'clipboard_occupancy_cancel'
    }
    [pscustomobject]@{
        Kind = 'public'
        File = $publicTest
        Name = 'test_win32_public_clipboard_cf_unicodetext_bmp_astral_roundtrip_red'
        Family = 'public_clipboard_unicode'
    }
)

$infrastructurePattern = '(?i)(builder error|C compilation error|compilation failed|undefined symbol|undefined reference|unresolved external|linker error|unknown method|cannot find (file|path|module|compiler|library|symbol)|cannot open (file|path)|no such file|failed to start|retrying [1-9][0-9]*/|max_retry:\s*[1-9])'
$fatalPattern = '(?i)(^|\s)(V panic:|panic:|fatal error:|unhandled exception|access violation|STATUS_ACCESS_VIOLATION|0xC0000005|segmentation fault|signal 11|stack overflow|illegal instruction|abort trap|process crashed|application crashed)'
$timeoutPattern = '(?i)(child timed out|process timed out|timed out after|execution timeout)'
$crashExitCodes = @(
    -2147483645,
    -1073741819,
    -1073741795,
    -1073741571,
    -1073741510,
    -1073740940,
    -1073740791
)

function Assert-W4FileHash {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Path,
        [Parameter(Mandatory = $true)]
        [string]$Expected
    )

    if (-not (Test-Path -LiteralPath $Path -PathType Leaf)) {
        throw "W4 hashed input is missing: $Path"
    }
    $actual = (Get-FileHash -LiteralPath $Path -Algorithm SHA256).Hash.ToLowerInvariant()
    $normalizedExpected = $Expected.ToLowerInvariant()
    if ($actual -cne $normalizedExpected) {
        throw "W4 hash mismatch for ${Path}: expected=$normalizedExpected actual=$actual"
    }
    Write-Host "PACKAGE2_W4_HASH_OK path=$Path sha256=$actual"
}

function Get-W4TextSha256 {
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

function Assert-W4FailedReap {
    param(
        [Parameter(Mandatory = $true)]
        [AllowEmptyString()]
        [string]$Failure
    )

    if ($Failure) {
        throw "W4 process supervision failed: $Failure"
    }
}

function Receive-W4ProcessOutput {
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
        Write-Verbose "W4 output aggregate fault: $($_.Exception.Message)"
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

function Invoke-W4BoundedProcess {
    param(
        [Parameter(Mandatory = $true)]
        [string]$FileName,
        [Parameter(Mandatory = $true)]
        [AllowEmptyCollection()]
        [string[]]$Arguments,
        [int]$TimeoutSeconds = 240,
        [int]$ReapTimeoutMilliseconds = 5000,
        [int]$DrainTimeoutMilliseconds = 5000
    )

    $startInfo = [System.Diagnostics.ProcessStartInfo]::new()
    $startInfo.FileName = $FileName
    $startInfo.WorkingDirectory = (Get-Location).Path
    $startInfo.UseShellExecute = $false
    $startInfo.RedirectStandardOutput = $true
    $startInfo.RedirectStandardError = $true
    $startInfo.CreateNoWindow = $true
    $startInfo.Environment['VFLAGS'] = ''
    $startInfo.Environment['VTEST_RETRY_MAX'] = '0'
    $startInfo.Environment['VTEST_FAIL_FAST'] = '1'
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
                    $failedReap =
                        "child did not reap within $ReapTimeoutMilliseconds ms after tree kill"
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
                    $failedReap =
                        "child did not reap within $ReapTimeoutMilliseconds ms after exception cleanup"
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
        $drain = Receive-W4ProcessOutput -StdoutTask $stdoutTask -StderrTask $stderrTask `
            -TimeoutMilliseconds $DrainTimeoutMilliseconds
        $stdout = $drain.Stdout
        $stderr = $drain.Stderr
        if (-not $drain.Completed) {
            $infrastructureErrors.Add($drain.Error)
        }
    } else {
        $infrastructureErrors.Add('redirected output tasks were not established')
    }
    $process.Dispose()
    Assert-W4FailedReap -Failure $failedReap

    $lines = @()
    if ($stdout) {
        $lines += @($stdout -split "\r?\n")
    }
    if ($stderr) {
        $lines += @($stderr -split "\r?\n")
    }
    return [pscustomobject]@{
        ExitCode = $exitCode
        TimedOut = $timedOut
        InfrastructureError = $infrastructureErrors -join '; '
        Stdout = $stdout
        Stderr = $stderr
        Output = $lines
    }
}

function Write-W4ProcessOutput {
    param(
        [Parameter(Mandatory = $true)]
        [pscustomobject]$Result
    )

    if ($Result.Stdout) {
        Write-Host $Result.Stdout.TrimEnd()
    }
    if ($Result.Stderr) {
        Write-Host $Result.Stderr.TrimEnd()
    }
}

function Get-W4Classification {
    param(
        [Parameter(Mandatory = $true)]
        [pscustomobject]$Case,
        [Parameter(Mandatory = $true)]
        [pscustomobject]$Result,
        [Parameter(Mandatory = $true)]
        [ValidateSet('Red', 'Green')]
        [string]$ExpectedState,
        [int]$ExpectedFailureExitCode = 1
    )

    $lines = @($Result.Output | ForEach-Object { ([string]$_).Trim() })
    $text = $lines -join "`n"
    $expectedTest = "PACKAGE2_RED_TEST=$($Case.Name)"
    $expectedFamily = "PACKAGE2_RED_FAMILY=$($Case.Family)"
    $expectedReached = "PACKAGE2_W4_REACHED=$($Case.Family)"
    $expectedTerminal = "PACKAGE2_RED_TERMINAL=behavioral_red:$($Case.Family)"
    $testLines = @($lines | Where-Object { $_ -cmatch '^PACKAGE2_RED_TEST=' })
    $familyLines = @($lines | Where-Object { $_ -cmatch '^PACKAGE2_RED_FAMILY=' })
    $reachedLines = @($lines | Where-Object { $_ -cmatch '^PACKAGE2_W4_REACHED=' })
    $infraLines = @($lines | Where-Object { $_ -cmatch '^PACKAGE2_W4_INFRA=' })
    $terminalLines = @($lines | Where-Object { $_ -cmatch '^PACKAGE2_RED_TERMINAL=' })
    $summaryLines = @(
        $lines | Where-Object { $_ -cmatch '^Summary for all V _test\.v files:.*$' }
    )
    $selectionMismatch = $text -match '(?im)^\s*retrying\s' `
        -or $text -match '(?im)\bskipped?\b'

    if ($Result.TimedOut -or $text -match $timeoutPattern) {
        return [pscustomobject]@{ Kind = 'TimeoutFailure'; Detail = 'watchdog or child timeout' }
    }
    if ($Result.InfrastructureError) {
        return [pscustomobject]@{
            Kind = 'InfrastructureFailure'
            Detail = $Result.InfrastructureError
        }
    }
    if ($null -eq $Result.ExitCode) {
        return [pscustomobject]@{ Kind = 'UnknownExit'; Detail = 'child returned no exit code' }
    }
    if ($Result.ExitCode -in $crashExitCodes -or $text -match $fatalPattern) {
        return [pscustomobject]@{
            Kind = 'FatalFailure'
            Detail = "exit=$($Result.ExitCode)"
        }
    }
    if ($text -match $infrastructurePattern) {
        return [pscustomobject]@{
            Kind = 'InfrastructureFailure'
            Detail = "compiler or runner diagnostic; exit=$($Result.ExitCode)"
        }
    }
    if ($infraLines.Count -ne 0) {
        return [pscustomobject]@{
            Kind = 'InfrastructureFailure'
            Detail = "test emitted $($infraLines.Count) PACKAGE2_W4_INFRA marker(s)"
        }
    }
    if ($testLines.Count -ne 1 -or $testLines[0] -cne $expectedTest `
        -or $familyLines.Count -ne 1 -or $familyLines[0] -cne $expectedFamily) {
        return [pscustomobject]@{
            Kind = 'IdentityFailure'
            Detail = "expected exact test=$expectedTest and family=$expectedFamily"
        }
    }
    if ($reachedLines.Count -ne 1 -or $reachedLines[0] -cne $expectedReached) {
        return [pscustomobject]@{
            Kind = 'ReachedFailure'
            Detail = "expected exact reached=$expectedReached"
        }
    }

    if ($ExpectedState -ceq 'Red') {
        if ($terminalLines.Count -ne 1 -or $terminalLines[0] -cne $expectedTerminal) {
            return [pscustomobject]@{
                Kind = 'TerminalFailure'
                Detail = "expected exact terminal=$expectedTerminal"
            }
        }
        $exactSummary = $summaryLines.Count -eq 1 `
            -and $summaryLines[0] -cmatch '^Summary for all V _test\.v files: 1 failed, 1 total\.(?: .*)?$'
        if (-not $exactSummary -or $selectionMismatch) {
            return [pscustomobject]@{
                Kind = 'SummaryFailure'
                Detail = "expected exactly one '1 failed, 1 total' summary without skip/retry"
            }
        }
        if ($Result.ExitCode -ne $ExpectedFailureExitCode) {
            return [pscustomobject]@{
                Kind = 'ExitFailure'
                Detail = "expected=$ExpectedFailureExitCode actual=$($Result.ExitCode)"
            }
        }
        return [pscustomobject]@{
            Kind = 'BehavioralRed'
            Detail = "exit=$ExpectedFailureExitCode"
        }
    }

    if ($terminalLines.Count -ne 0) {
        return [pscustomobject]@{
            Kind = 'TerminalFailure'
            Detail = 'GREEN output retained a PACKAGE2_RED_TERMINAL marker'
        }
    }
    $exactSummary = $summaryLines.Count -eq 1 `
        -and $summaryLines[0] -cmatch '^Summary for all V _test\.v files: 1 passed, 1 total\.(?: .*)?$'
    if (-not $exactSummary -or $selectionMismatch) {
        return [pscustomobject]@{
            Kind = 'SummaryFailure'
            Detail = "expected exactly one '1 passed, 1 total' summary without skip/retry"
        }
    }
    if ($Result.ExitCode -ne 0) {
        return [pscustomobject]@{
            Kind = 'ExitFailure'
            Detail = "expected=0 actual=$($Result.ExitCode)"
        }
    }
    return [pscustomobject]@{ Kind = 'BehavioralGreen'; Detail = 'exit=0' }
}

function Test-W4Classifier {
    $probe = [pscustomobject]@{
        Name = 'test_win32_w4_classifier_probe'
        Family = 'classifier_probe'
    }
    $test = "PACKAGE2_RED_TEST=$($probe.Name)"
    $family = "PACKAGE2_RED_FAMILY=$($probe.Family)"
    $reached = "PACKAGE2_W4_REACHED=$($probe.Family)"
    $terminal = "PACKAGE2_RED_TERMINAL=behavioral_red:$($probe.Family)"
    $redSummary = 'Summary for all V _test.v files: 1 failed, 1 total. Elapsed time: 1 ms.'
    $greenSummary = 'Summary for all V _test.v files: 1 passed, 1 total. Elapsed time: 1 ms.'
    $validRed = @($test, $family, $reached, $terminal, $redSummary)
    $validGreen = @($test, $family, $reached, $greenSummary)
    $synthetic = @(
        @{ Name = 'valid RED'; State = 'Red'; Expected = 'BehavioralRed'; Exit = 1; Output = $validRed; TimedOut = $false; Infra = '' }
        @{ Name = 'valid GREEN'; State = 'Green'; Expected = 'BehavioralGreen'; Exit = 0; Output = $validGreen; TimedOut = $false; Infra = '' }
        @{ Name = 'GREEN missing test'; State = 'Green'; Expected = 'IdentityFailure'; Exit = 0; Output = @($family, $reached, $greenSummary); TimedOut = $false; Infra = '' }
        @{ Name = 'GREEN missing family'; State = 'Green'; Expected = 'IdentityFailure'; Exit = 0; Output = @($test, $reached, $greenSummary); TimedOut = $false; Infra = '' }
        @{ Name = 'GREEN missing reached'; State = 'Green'; Expected = 'ReachedFailure'; Exit = 0; Output = @($test, $family, $greenSummary); TimedOut = $false; Infra = '' }
        @{ Name = 'missing test'; State = 'Red'; Expected = 'IdentityFailure'; Exit = 1; Output = @($family, $reached, $terminal, $redSummary); TimedOut = $false; Infra = '' }
        @{ Name = 'duplicate family'; State = 'Red'; Expected = 'IdentityFailure'; Exit = 1; Output = @($test, $family, $family, $reached, $terminal, $redSummary); TimedOut = $false; Infra = '' }
        @{ Name = 'missing reached'; State = 'Red'; Expected = 'ReachedFailure'; Exit = 1; Output = @($test, $family, $terminal, $redSummary); TimedOut = $false; Infra = '' }
        @{ Name = 'duplicate reached'; State = 'Red'; Expected = 'ReachedFailure'; Exit = 1; Output = @($test, $family, $reached, $reached, $terminal, $redSummary); TimedOut = $false; Infra = '' }
        @{ Name = 'infra marker'; State = 'Red'; Expected = 'InfrastructureFailure'; Exit = 1; Output = @($validRed + 'PACKAGE2_W4_INFRA=synthetic'); TimedOut = $false; Infra = '' }
        @{ Name = 'supervisor infra'; State = 'Red'; Expected = 'InfrastructureFailure'; Exit = 1; Output = $validRed; TimedOut = $false; Infra = 'synthetic failed reap' }
        @{ Name = 'compiler infra'; State = 'Red'; Expected = 'InfrastructureFailure'; Exit = 1; Output = @($validRed + 'C compilation error'); TimedOut = $false; Infra = '' }
        @{ Name = 'missing RED terminal'; State = 'Red'; Expected = 'TerminalFailure'; Exit = 1; Output = @($test, $family, $reached, $redSummary); TimedOut = $false; Infra = '' }
        @{ Name = 'RED terminal in GREEN'; State = 'Green'; Expected = 'TerminalFailure'; Exit = 0; Output = @($validGreen + $terminal); TimedOut = $false; Infra = '' }
        @{ Name = 'wrong RED summary'; State = 'Red'; Expected = 'SummaryFailure'; Exit = 1; Output = @($test, $family, $reached, $terminal, $greenSummary); TimedOut = $false; Infra = '' }
        @{ Name = 'wrong GREEN summary'; State = 'Green'; Expected = 'SummaryFailure'; Exit = 0; Output = @($test, $family, $reached, $redSummary); TimedOut = $false; Infra = '' }
        @{ Name = 'RED wrong exit'; State = 'Red'; Expected = 'ExitFailure'; Exit = 0; Output = $validRed; TimedOut = $false; Infra = '' }
        @{ Name = 'GREEN wrong exit'; State = 'Green'; Expected = 'ExitFailure'; Exit = 1; Output = $validGreen; TimedOut = $false; Infra = '' }
        @{ Name = 'watchdog timeout'; State = 'Red'; Expected = 'TimeoutFailure'; Exit = $null; Output = $validRed; TimedOut = $true; Infra = '' }
        @{ Name = 'fatal panic'; State = 'Red'; Expected = 'FatalFailure'; Exit = 1; Output = @($validRed + 'V panic: synthetic'); TimedOut = $false; Infra = '' }
        @{ Name = 'unknown exit'; State = 'Red'; Expected = 'ExitFailure'; Exit = 7; Output = $validRed; TimedOut = $false; Infra = '' }
    )

    $accepted = 0
    foreach ($item in $synthetic) {
        $result = [pscustomobject]@{
            ExitCode = $item.Exit
            TimedOut = $item.TimedOut
            InfrastructureError = $item.Infra
            Output = $item.Output
        }
        $actual = Get-W4Classification -Case $probe -Result $result `
            -ExpectedState $item.State -ExpectedFailureExitCode 1
        if ($actual.Kind -cne $item.Expected) {
            throw "W4 classifier self-test '$($item.Name)' expected $($item.Expected), got $($actual.Kind)"
        }
        if ($actual.Kind -in @('BehavioralRed', 'BehavioralGreen')) {
            $accepted++
        }
    }
    if ($accepted -ne 2) {
        throw "W4 classifier self-test accepted $accepted synthetic cases instead of two"
    }
    Write-Host "PACKAGE2_W4_CLASSIFIER_SELF_TEST accepted=$accepted rejected=$($synthetic.Count - $accepted) total=$($synthetic.Count)"
}

function Test-W4FailedReapGate {
    $failure = 'child did not reap within 0 ms after synthetic tree kill'
    $hardAbort = $false
    $nextCaseStarted = $false
    try {
        Assert-W4FailedReap -Failure $failure
        $nextCaseStarted = $true
    } catch {
        $expected = "W4 process supervision failed: $failure"
        if ($_.Exception.Message -cne $expected) {
            throw
        }
        $hardAbort = $true
    }
    if (-not $hardAbort -or $nextCaseStarted) {
        throw 'W4 failed-reap self-test did not hard-abort before the next case'
    }
    Write-Host 'PACKAGE2_W4_FAILED_REAP_SELF_TEST injected=true hard_abort=true next_case_started=false'
}

function Assert-W4GreenCommand {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Label,
        [Parameter(Mandatory = $true)]
        [pscustomobject]$Result,
        [switch]$RequireSingleTestSummary
    )

    $lines = @($Result.Output | ForEach-Object { ([string]$_).Trim() })
    $text = $lines -join "`n"
    if ($Result.TimedOut -or $text -match $timeoutPattern) {
        throw "$Label timed out"
    }
    if ($Result.InfrastructureError -or $text -match $infrastructurePattern) {
        throw "$Label had an infrastructure failure: $($Result.InfrastructureError)"
    }
    if ($null -eq $Result.ExitCode -or $Result.ExitCode -ne 0 `
        -or $Result.ExitCode -in $crashExitCodes -or $text -match $fatalPattern) {
        throw "$Label failed with exit $($Result.ExitCode)"
    }
    if ($RequireSingleTestSummary) {
        $summaryLines = @(
            $lines | Where-Object { $_ -cmatch '^Summary for all V _test\.v files:.*$' }
        )
        $exactSummary = $summaryLines.Count -eq 1 `
            -and $summaryLines[0] -cmatch '^Summary for all V _test\.v files: 1 passed, 1 total\.(?: .*)?$'
        $selectionMismatch = $text -match '(?im)^\s*retrying\s' `
            -or $text -match '(?im)\bskipped?\b'
        if (-not $exactSummary -or $selectionMismatch) {
            throw "$Label did not run exactly one passing test without skip/retry"
        }
    }
}

function Get-W4ExpectedFailureExitCode {
    param(
        [Parameter(Mandatory = $true)]
        [string]$VExe,
        [Parameter(Mandatory = $true)]
        [string]$SelectedCompiler
    )

    $tempRoot = if ($env:RUNNER_TEMP) {
        $env:RUNNER_TEMP
    } else {
        [System.IO.Path]::GetTempPath()
    }
    $tempDir = Join-Path $tempRoot "multiwindow_w4_exit_probe_$([guid]::NewGuid().ToString('N'))"
    [void](New-Item -ItemType Directory -Path $tempDir)
    $sourcePath = Join-Path $tempDir 'w4_controlled_failure_test.v'
    $source = @'
// vtest retry: 0
module main

fn test_w4_controlled_failure_exit() {
	eprintln('PACKAGE2_W4_EXIT_PROBE=controlled_assertion')
	assert false, 'PACKAGE2_W4_EXIT_PROBE_ASSERTION=controlled_failure'
}
'@
    try {
        [System.IO.File]::WriteAllText(
            $sourcePath,
            $source,
            [System.Text.UTF8Encoding]::new($false)
        )
        $result = Invoke-W4BoundedProcess -FileName $VExe -Arguments @(
            '-cc', $SelectedCompiler,
            '-no-retry-compilation',
            '-no-parallel',
            '-subsystem', 'console',
            '-run-only', 'test_w4_controlled_failure_exit',
            'test', $sourcePath
        )
        Write-W4ProcessOutput -Result $result
        $lines = @($result.Output | ForEach-Object { ([string]$_).Trim() })
        $text = $lines -join "`n"
        $markerLines = @(
            $lines | Where-Object { $_ -cmatch '^PACKAGE2_W4_EXIT_PROBE=' }
        )
        $summaryLines = @(
            $lines | Where-Object { $_ -cmatch '^Summary for all V _test\.v files:.*$' }
        )
        $exactMarker = $markerLines.Count -eq 1 `
            -and $markerLines[0] -ceq 'PACKAGE2_W4_EXIT_PROBE=controlled_assertion'
        $exactSummary = $summaryLines.Count -eq 1 `
            -and $summaryLines[0] -cmatch '^Summary for all V _test\.v files: 1 failed, 1 total\.(?: .*)?$'
        if ($result.TimedOut -or $result.InfrastructureError -or $text -match $timeoutPattern `
            -or $text -match $infrastructurePattern -or $text -match $fatalPattern `
            -or $result.ExitCode -in $crashExitCodes -or -not $exactMarker -or -not $exactSummary `
            -or $result.ExitCode -ne 1) {
            throw "W4 controlled failure-exit probe was not an exact V assertion failure for $SelectedCompiler"
        }
        Write-Host "PACKAGE2_W4_EXPECTED_FAILURE_EXIT compiler=$SelectedCompiler exit=1"
        return 1
    } finally {
        Remove-Item -LiteralPath $tempDir -Recurse -Force -ErrorAction SilentlyContinue
    }
}

$names = @($cases | ForEach-Object { $_.Name } | Sort-Object -Unique)
$families = @($cases | ForEach-Object { $_.Family } | Sort-Object -Unique)
if ($cases.Count -ne 8 -or $names.Count -ne 8 -or $families.Count -ne 8) {
    throw 'W4 surface must contain exactly eight unique tests and eight unique families'
}

foreach ($group in @($cases | Group-Object File)) {
    $discovered = @(
        Select-String -LiteralPath $group.Name -Pattern '^fn (test_[A-Za-z0-9_]+)\(\)' |
            ForEach-Object { $_.Matches[0].Groups[1].Value }
    )
    foreach ($case in $group.Group) {
        if ($discovered -cnotcontains $case.Name) {
            throw "W4 test was not discovered: $($case.Name) in $($case.File)"
        }
    }
}

$testPaths = @($oracle, $nativeTest, $publicTest, $noOptProbe)
$knownTestFileHashes = @(
    '804e8cbc5f5f7c390e90736d54a60d65d19b649a94afa4915bd9cb4e95c4e04d'
	'e71a0856bcbe5b278b9a580c5151faacae66c0f0c3f9b0861c710b2238ed43bc'
    'd08eafb919ae97b185fc480c22f6d990973396152e9a0b3a01035c3e9a30275c'
    '288f148ca15b6694481f117c03be5f80c4045baf76d3b0e90db61b5e0596741c'
)
$passedTestFileHashes = @(
    $ExpectedOracleSha256.ToLowerInvariant()
    $ExpectedNativeTestSha256.ToLowerInvariant()
    $ExpectedPublicTestSha256.ToLowerInvariant()
    $ExpectedNoOptProbeSha256.ToLowerInvariant()
)
for ($index = 0; $index -lt $testPaths.Count; $index++) {
    if ($passedTestFileHashes[$index] -cne $knownTestFileHashes[$index]) {
        throw "W4 frozen test parameter mismatch for $($testPaths[$index]): expected=$($knownTestFileHashes[$index]) passed=$($passedTestFileHashes[$index])"
    }
}

Assert-W4FileHash -Path $oracle -Expected $ExpectedOracleSha256
Assert-W4FileHash -Path $nativeTest -Expected $ExpectedNativeTestSha256
Assert-W4FileHash -Path $publicTest -Expected $ExpectedPublicTestSha256
Assert-W4FileHash -Path $noOptProbe -Expected $ExpectedNoOptProbeSha256
Assert-W4FileHash -Path $PSCommandPath -Expected $ExpectedRunnerSha256

$testTupleRecords = @(
    'schema=package2-win32-w4-test-tuple-v1'
    "file=$oracle|sha256=$($ExpectedOracleSha256.ToLowerInvariant())"
    "file=$nativeTest|sha256=$($ExpectedNativeTestSha256.ToLowerInvariant())"
    "file=$publicTest|sha256=$($ExpectedPublicTestSha256.ToLowerInvariant())"
    "file=$noOptProbe|sha256=$($ExpectedNoOptProbeSha256.ToLowerInvariant())"
)
$testTupleRecords += @(
    $cases | ForEach-Object {
        "case=$($_.Kind)|$($_.Name)|$($_.Family)"
    }
)
$testTupleSha256 = Get-W4TextSha256 -Text (($testTupleRecords -join "`n") + "`n")
$knownTestTupleSha256 = 'a5b036e5eebc6b5ba8f6de54cb614896d4fea62b7e331d7faaba8e2d806a0e7c'
if ($testTupleSha256 -cne $knownTestTupleSha256) {
    throw "W4 frozen ordered test tuple mismatch: expected=$knownTestTupleSha256 actual=$testTupleSha256"
}
Write-Host "PACKAGE2_W4_TEST_TUPLE_OK sha256=$testTupleSha256 cases=$($cases.Count)"

function Get-W4ProductionSurfaceSha256 {
    param(
        [Parameter(Mandatory = $true)]
        [ValidateSet('Red', 'Green')]
        [string]$State,
        [Parameter(Mandatory = $true)]
        [string[]]$Hashes
    )

    if ($Hashes.Count -ne $surfacePaths.Count) {
        throw "W4 $State production surface hash tuple has $($Hashes.Count) entries"
    }
    $records = [System.Collections.Generic.List[string]]::new()
    [void]$records.Add('schema=package2-win32-w4-production-surface-v1')
    [void]$records.Add(
        "expectation=$State"
    )
    for ($index = 0; $index -lt $surfacePaths.Count; $index++) {
        [void]$records.Add(
            "file=$($surfacePaths[$index])|sha256=$($Hashes[$index].ToLowerInvariant())"
        )
    }
    return Get-W4TextSha256 -Text (($records -join "`n") + "`n")
}

$knownSurfaceFiles = @{
    Red = @(
        '507686b3e29d22d9b913da79bb20d435394daf693e166be1c61569bd08b853d6'
		'b4b5551def3d0037769db3339ac8e0e55f2ff150e4954a40183909e2f8f0244b'
        '2ae1143cd1e069727ac82f4d0ca97866686874562fcdd8d660d8706dfd7e7856'
        '5261f7fd1b9aefe3bb76c298f8eeeabd281f8fa26fe58b15868ceb6415dd8ba1'
        'c113d9361d0f55f5fd127d2a2ae7e05fa6930022350cb717161de07e5eeb0054'
    )
    Green = @(
		'8ad39d63360446ef7049c726544200780eff61ff39917830f7024243dfd363be'
		'd38c868f574c02fc46e047da25e770e5b9f672500d796694b5ef35bf042cdae0'
		'8c915ec5bd9b116d98b8711bbd46f3afd5d6761800d7f91fed0deef6ac26cbaf'
		'faaf67be3e0381ea8a0e7a753b477b7965dde6e655f83f220eca036da19c5df6'
		'2ffc93e245a43c4a557fd7cc66ebcdda8874b9783ec3050ba754657a97efd033'
    )
}
$knownSurfaceComposites = @{
    Red = Get-W4ProductionSurfaceSha256 -State Red -Hashes $knownSurfaceFiles.Red
    Green = Get-W4ProductionSurfaceSha256 -State Green -Hashes $knownSurfaceFiles.Green
}
$passedSurfaceComposites = @{
    Red = $ExpectedRedSurfaceSha256.ToLowerInvariant()
    Green = $ExpectedGreenSurfaceSha256.ToLowerInvariant()
}
foreach ($state in @('Red', 'Green')) {
    if ($passedSurfaceComposites[$state] -cne $knownSurfaceComposites[$state]) {
        throw "W4 $state production surface constant mismatch: expected=$($knownSurfaceComposites[$state]) passed=$($passedSurfaceComposites[$state])"
    }
}

$passedCurrentSurface = @(
    $ExpectedServiceBackendSha256.ToLowerInvariant()
	$ExpectedEventDeliverySha256.ToLowerInvariant()
    $ExpectedWin32BackendSha256.ToLowerInvariant()
    $ExpectedWin32ServiceBackendSha256.ToLowerInvariant()
    $ExpectedWin32ServiceNativeSha256.ToLowerInvariant()
)
$actualCurrentSurface = [System.Collections.Generic.List[string]]::new()
for ($index = 0; $index -lt $surfacePaths.Count; $index++) {
    $path = $surfacePaths[$index]
    $knownExpected = $knownSurfaceFiles[$Expectation][$index]
    $passedExpected = $passedCurrentSurface[$index]
    if ($passedExpected -cne $knownExpected) {
        throw "W4 $Expectation current surface parameter mismatch for ${path}: expected=$knownExpected passed=$passedExpected"
    }
    if (-not (Test-Path -LiteralPath $path -PathType Leaf)) {
        throw "W4 production surface file is missing: $path"
    }
    $actual = (Get-FileHash -LiteralPath $path -Algorithm SHA256).Hash.ToLowerInvariant()
    if ($actual -cne $passedExpected) {
        throw "W4 $Expectation production surface mismatch for ${path}: expected=$passedExpected actual=$actual"
    }
    [void]$actualCurrentSurface.Add($actual)
    Write-Host "PACKAGE2_W4_SURFACE_FILE_OK expectation=$Expectation path=$path sha256=$actual"
}
$actualCurrentSurfaceArray = @($actualCurrentSurface)
$actualSurfaceComposite = Get-W4ProductionSurfaceSha256 -State $Expectation `
    -Hashes $actualCurrentSurfaceArray
if ($actualSurfaceComposite -cne $passedSurfaceComposites[$Expectation]) {
    throw "W4 $Expectation production surface composite mismatch: expected=$($passedSurfaceComposites[$Expectation]) actual=$actualSurfaceComposite"
}
Write-Host "PACKAGE2_W4_SURFACE_OK expectation=$Expectation sha256=$actualSurfaceComposite files=$($surfacePaths.Count)"

Test-W4Classifier
Test-W4FailedReapGate

$vexe = (Resolve-Path '.\v.exe').Path
$savedVflags = $env:VFLAGS
$env:VFLAGS = ''
try {
    Write-Host "::group::Win32 W4 no-opt public gate $Compiler"
    try {
        $noOptPublic = Invoke-W4BoundedProcess -FileName $vexe -Arguments @(
            '-cc', $Compiler,
            '-no-retry-compilation',
            '-no-parallel',
            '-subsystem', 'console',
            '-run-only', 'test_win32_public_services_stay_disabled_without_opt_in',
            'test', $publicTest
        )
        Write-W4ProcessOutput -Result $noOptPublic
        Assert-W4GreenCommand -Label "W4 no-opt public gate/$Compiler" `
            -Result $noOptPublic -RequireSingleTestSummary
        Write-Host "PACKAGE2_W4_NO_OPT_PUBLIC_PASS compiler=$Compiler"
    } finally {
        Write-Host '::endgroup::'
    }

    Write-Host "::group::Win32 W4 no-opt standalone probe $Compiler"
    $tempRoot = if ($env:RUNNER_TEMP) {
        $env:RUNNER_TEMP
    } else {
        [System.IO.Path]::GetTempPath()
    }
    $probeTemp = Join-Path $tempRoot "multiwindow_w4_no_opt_$([guid]::NewGuid().ToString('N'))"
    [void](New-Item -ItemType Directory -Path $probeTemp)
    try {
        $probeExe = Join-Path $probeTemp 'clipboard_no_opt.exe'
        $probeCompile = Invoke-W4BoundedProcess -FileName $vexe -Arguments @(
            '-cc', $Compiler,
            '-no-retry-compilation',
            '-gc', 'none',
            '-subsystem', 'console',
            '-o', $probeExe,
            $noOptProbe
        )
        Write-W4ProcessOutput -Result $probeCompile
        Assert-W4GreenCommand -Label "W4 no-opt probe compile/$Compiler" `
            -Result $probeCompile
        if (-not (Test-Path -LiteralPath $probeExe -PathType Leaf)) {
            throw "W4 no-opt probe binary is missing: $probeExe"
        }

        $probeRun = Invoke-W4BoundedProcess -FileName $probeExe -Arguments @() `
            -TimeoutSeconds 30
        Write-W4ProcessOutput -Result $probeRun
        Assert-W4GreenCommand -Label "W4 no-opt probe run/$Compiler" -Result $probeRun
        $nonempty = @(
            $probeRun.Output |
                ForEach-Object { ([string]$_).Trim() } |
                Where-Object { $_ -ne '' }
        )
        $expectedCompiler = if ($Compiler -ceq 'tcc') { 'tinyc' } else { $Compiler }
        $expectedProbeLine = "CCOMPILER=$expectedCompiler"
        if ($nonempty.Count -ne 1 -or $nonempty[0] -cne $expectedProbeLine) {
            throw "W4 no-opt probe output mismatch: expected '$expectedProbeLine'"
        }
        Write-Host "PACKAGE2_W4_NO_OPT_PROBE_PASS compiler=$Compiler output=$expectedProbeLine"
    } finally {
        Remove-Item -LiteralPath $probeTemp -Recurse -Force -ErrorAction SilentlyContinue
        Write-Host '::endgroup::'
    }

    Write-Host "::group::Win32 W4 controlled failure-exit probe $Compiler"
    try {
        $expectedFailureExitCode = Get-W4ExpectedFailureExitCode -VExe $vexe `
            -SelectedCompiler $Compiler
    } finally {
        Write-Host '::endgroup::'
    }

    $accepted = 0
    foreach ($case in $cases) {
        Write-Host "::group::Win32 W4 $Expectation $Compiler $($case.Name)"
        try {
            Write-Host "PACKAGE2_W4_CASE_START compiler=$Compiler expectation=$Expectation kind=$($case.Kind) case=$($case.Name) family=$($case.Family)"
            $result = Invoke-W4BoundedProcess -FileName $vexe -Arguments @(
                '-stats',
                '-cc', $Compiler,
                '-no-retry-compilation',
                '-no-parallel',
                '-subsystem', 'console',
                '-d', 'gg_multiwindow',
                '-run-only', $case.Name,
                'test', $case.File
            )
            Write-W4ProcessOutput -Result $result
            $classification = Get-W4Classification -Case $case -Result $result `
                -ExpectedState $Expectation `
                -ExpectedFailureExitCode $expectedFailureExitCode
            $expectedKind = if ($Expectation -ceq 'Red') {
                'BehavioralRed'
            } else {
                'BehavioralGreen'
            }
            if ($classification.Kind -cne $expectedKind) {
                Write-Host "PACKAGE2_W4_CASE_REJECT compiler=$Compiler expectation=$Expectation case=$($case.Name) kind=$($classification.Kind) detail=$($classification.Detail)"
                throw "W4 $Expectation gate rejected $($case.Name): $($classification.Kind): $($classification.Detail)"
            }
            $accepted++
            Write-Host "PACKAGE2_W4_CASE_ACCEPT compiler=$Compiler expectation=$Expectation case=$($case.Name) family=$($case.Family) classification=$expectedKind"
        } finally {
            Write-Host '::endgroup::'
        }
    }

    if ($accepted -ne $cases.Count) {
        throw "W4 $Expectation matrix accepted $accepted of $($cases.Count) cases for $Compiler"
    }
    Write-Host "PACKAGE2_W4_SUMMARY compiler=$Compiler expectation=$Expectation accepted=$accepted rejected=0 total=$($cases.Count)"
} finally {
    $env:VFLAGS = $savedVflags
}
