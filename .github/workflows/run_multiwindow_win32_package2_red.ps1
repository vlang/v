param(
    [Parameter(Mandatory = $true)]
    [ValidateSet('msvc', 'gcc', 'tcc')]
    [string]$Compiler
)

$ErrorActionPreference = 'Continue'
$env:VTEST_RETRY_MAX = '0'
$env:VTEST_FAIL_FAST = '1'

function Receive-Package2ProcessOutput {
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
        # Faults are reported per stream below so completed peer output is retained.
        Write-Verbose "redirected output aggregate fault: $($_.Exception.Message)"
    }

    $stdout = ''
    if ($StdoutTask.IsCompletedSuccessfully) {
        $stdout = [string]$StdoutTask.GetAwaiter().GetResult()
    } elseif ($StdoutTask.IsFaulted) {
        $errors.Add("stdout drain failed: $($StdoutTask.Exception.GetBaseException().Message)")
    } elseif ($StdoutTask.IsCanceled) {
        $errors.Add('stdout drain was cancelled')
    }

    $stderr = ''
    if ($StderrTask.IsCompletedSuccessfully) {
        $stderr = [string]$StderrTask.GetAwaiter().GetResult()
    } elseif ($StderrTask.IsFaulted) {
        $errors.Add("stderr drain failed: $($StderrTask.Exception.GetBaseException().Message)")
    } elseif ($StderrTask.IsCanceled) {
        $errors.Add('stderr drain was cancelled')
    }

    return [pscustomobject]@{
        Completed = $StdoutTask.IsCompletedSuccessfully -and $StderrTask.IsCompletedSuccessfully
        Error = $errors -join '; '
        Stdout = $stdout
        Stderr = $stderr
    }
}

function Invoke-Package2SupervisionFailureGate {
    param(
        [Parameter(Mandatory = $true)]
        [AllowEmptyString()]
        [string]$Failure
    )

    if ($Failure) {
        throw "Package 2 process supervision failed: $Failure"
    }
}

function Invoke-Package2Process {
    param(
        [Parameter(Mandatory = $true)]
        [string]$FileName,
        [Parameter(Mandatory = $true)]
        [AllowEmptyCollection()]
        [string[]]$Arguments,
        [int]$TimeoutSeconds = 120,
        [int]$ReapTimeoutMilliseconds = 5000,
        [int]$OutputDrainTimeoutMilliseconds = 5000
    )

    $startInfo = [System.Diagnostics.ProcessStartInfo]::new()
    $startInfo.FileName = $FileName
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
    $supervisionFailure = ''
    $result = $null
    try {
        if (-not $process.Start()) {
            throw 'v.exe did not start'
        }
        $started = $true
        $stdoutTask = $process.StandardOutput.ReadToEndAsync()
        $stderrTask = $process.StandardError.ReadToEndAsync()
        $infrastructureErrors = [System.Collections.Generic.List[string]]::new()
        $timedOut = -not $process.WaitForExit($TimeoutSeconds * 1000)
        if ($timedOut) {
            try {
                $process.Kill($true)
            } catch {
                Write-Host "WATCHDOG_KILL_FAILURE $($_.Exception.Message)"
                $infrastructureErrors.Add("watchdog tree kill failed: $($_.Exception.Message)")
            }
            if (-not $process.WaitForExit($ReapTimeoutMilliseconds)) {
                $supervisionFailure =
                    "child did not exit within $ReapTimeoutMilliseconds ms after watchdog kill"
                $infrastructureErrors.Add($supervisionFailure)
            }
        }
        $drain = Receive-Package2ProcessOutput -StdoutTask $stdoutTask -StderrTask $stderrTask `
            -TimeoutMilliseconds $OutputDrainTimeoutMilliseconds
        if (-not $drain.Completed) {
            $infrastructureErrors.Add($drain.Error)
        }
        $stdout = $drain.Stdout
        $stderr = $drain.Stderr
        $lines = @()
        if ($stdout) {
            $lines += @($stdout -split "\r?\n")
        }
        if ($stderr) {
            $lines += @($stderr -split "\r?\n")
        }
        $result = [pscustomobject]@{
            ExitCode = if ($timedOut) { $null } else { $process.ExitCode }
            TimedOut = $timedOut
            InfrastructureError = $infrastructureErrors -join '; '
            Stdout = $stdout
            Stderr = $stderr
            Output = $lines
        }
    } catch {
        $infrastructureErrors = [System.Collections.Generic.List[string]]::new()
        $infrastructureErrors.Add($_.Exception.Message)
        if ($started) {
            try {
                if (-not $process.HasExited) {
                    $process.Kill($true)
                }
            } catch {
                Write-Host "WATCHDOG_EXCEPTION_KILL_FAILURE $($_.Exception.Message)"
                $infrastructureErrors.Add(
                    "exception cleanup tree kill failed: $($_.Exception.Message)"
                )
            }
            try {
                if (-not $process.WaitForExit($ReapTimeoutMilliseconds)) {
                    $supervisionFailure =
                        "child did not exit within $ReapTimeoutMilliseconds ms after exception cleanup"
                    $infrastructureErrors.Add($supervisionFailure)
                }
            } catch {
                $supervisionFailure = "exception cleanup reap failed: $($_.Exception.Message)"
                $infrastructureErrors.Add($supervisionFailure)
            }
        }

        $stdout = ''
        $stderr = ''
        if ($null -ne $stdoutTask -and $null -ne $stderrTask) {
            $drain = Receive-Package2ProcessOutput -StdoutTask $stdoutTask `
                -StderrTask $stderrTask -TimeoutMilliseconds $OutputDrainTimeoutMilliseconds
            $stdout = $drain.Stdout
            $stderr = $drain.Stderr
            if (-not $drain.Completed) {
                $infrastructureErrors.Add($drain.Error)
            }
        }
        $lines = @()
        if ($stdout) {
            $lines += @($stdout -split "\r?\n")
        }
        if ($stderr) {
            $lines += @($stderr -split "\r?\n")
        }
        $result = [pscustomobject]@{
            ExitCode = $null
            TimedOut = $timedOut
            InfrastructureError = $infrastructureErrors -join '; '
            Stdout = $stdout
            Stderr = $stderr
            Output = $lines
        }
    } finally {
        $process.Dispose()
    }
    Invoke-Package2SupervisionFailureGate -Failure $supervisionFailure
    return $result
}

function Test-Package2BoundedProcessDrain {
    $stdoutSource = [System.Threading.Tasks.TaskCompletionSource[string]]::new()
    $stderrSource = [System.Threading.Tasks.TaskCompletionSource[string]]::new()
    $watch = [System.Diagnostics.Stopwatch]::StartNew()
    $result = Receive-Package2ProcessOutput -StdoutTask $stdoutSource.Task `
        -StderrTask $stderrSource.Task -TimeoutMilliseconds 50
    $watch.Stop()

    if ($result.Completed -or -not $result.Error) {
        throw 'Package 2 bounded output-drain self-test accepted an inherited open pipe'
    }
    if ($watch.ElapsedMilliseconds -gt 1000) {
        throw "Package 2 bounded output-drain self-test took $($watch.ElapsedMilliseconds) ms"
    }

    $completedStdout = [System.Threading.Tasks.Task]::FromResult(
        [string]'package2-partial-stdout'
    )
    $partial = Receive-Package2ProcessOutput -StdoutTask $completedStdout `
        -StderrTask $stderrSource.Task -TimeoutMilliseconds 50
    if ($partial.Completed -or $partial.Stdout -cne 'package2-partial-stdout' `
        -or $partial.Stderr -cne '' -or -not $partial.Error) {
        throw 'Package 2 output-drain self-test lost completed stdout'
    }
    $faultedStderr = [System.Threading.Tasks.Task]::FromException[string](
        [System.InvalidOperationException]::new('package2-stderr-fault')
    )
    $faulted = Receive-Package2ProcessOutput -StdoutTask $completedStdout `
        -StderrTask $faultedStderr -TimeoutMilliseconds 50
    if ($faulted.Completed -or $faulted.Stdout -cne 'package2-partial-stdout' `
        -or $faulted.Stderr -cne '' -or $faulted.Error -cnotmatch 'package2-stderr-fault') {
        throw 'Package 2 output-drain self-test lost stdout beside faulted stderr'
    }

    $tempRoot = if ($env:RUNNER_TEMP) { $env:RUNNER_TEMP } else { [System.IO.Path]::GetTempPath() }
    $token = [guid]::NewGuid().ToString('N')
    $probePath = Join-Path $tempRoot "package2_process_tree_$token.ps1"
    $pidPath = Join-Path $tempRoot "package2_process_tree_$token.pids"
    $probeIds = @()
    $probeSource = @'
param([string]$PidPath)

$childInfo = [System.Diagnostics.ProcessStartInfo]::new()
$childInfo.FileName = (Get-Process -Id $PID).Path
$childInfo.UseShellExecute = $false
$childInfo.CreateNoWindow = $true
[void]$childInfo.ArgumentList.Add('-NoLogo')
[void]$childInfo.ArgumentList.Add('-NoProfile')
[void]$childInfo.ArgumentList.Add('-Command')
[void]$childInfo.ArgumentList.Add('Start-Sleep -Seconds 30')
$child = [System.Diagnostics.Process]::Start($childInfo)
[System.IO.File]::WriteAllText($PidPath, "$PID`n$($child.Id)")
Start-Sleep -Seconds 30
'@
    try {
        [System.IO.File]::WriteAllText(
            $probePath,
            $probeSource,
            [System.Text.UTF8Encoding]::new($false)
        )
        $pwshPath = (Get-Process -Id $PID).Path
        $tree = Invoke-Package2Process -FileName $pwshPath -Arguments @(
            '-NoLogo', '-NoProfile', '-File', $probePath, $pidPath
        ) -TimeoutSeconds 2
        if (-not (Test-Path $pidPath -PathType Leaf)) {
            throw 'Package 2 process-tree self-test did not publish child PIDs'
        }
        $probeIds = @(
            Get-Content $pidPath | ForEach-Object { [int]$_ }
        )
        $survivors = @(
            $probeIds | Where-Object {
                $null -ne (Get-Process -Id $_ -ErrorAction SilentlyContinue)
            }
        )
        if (-not $tree.TimedOut -or $tree.InfrastructureError -or $probeIds.Count -ne 2 `
            -or $survivors.Count -ne 0) {
            throw "Package 2 process-tree self-test left survivors: $($survivors -join ',')"
        }
    } finally {
        foreach ($probeId in $probeIds) {
            Stop-Process -Id $probeId -Force -ErrorAction SilentlyContinue
        }
        [System.IO.File]::Delete($probePath)
        [System.IO.File]::Delete($pidPath)
    }

    $syntheticFailure = 'child did not exit within 0 ms after watchdog kill'
    $hardAbort = $false
    $nextCaseStarted = $false
    try {
        Invoke-Package2SupervisionFailureGate -Failure $syntheticFailure
        $nextCaseStarted = $true
    } catch {
        $expected = "Package 2 process supervision failed: $syntheticFailure"
        if ($_.Exception.Message -cne $expected) {
            throw
        }
        $hardAbort = $true
    }
    if (-not $hardAbort -or $nextCaseStarted) {
        throw 'Package 2 failed-reap self-test did not abort before the next case'
    }
    Write-Host 'PACKAGE2_FAILED_REAP_SELF_TEST injected=true hard_abort=true next_case_started=false'
    Write-Host "PACKAGE2_PROCESS_DRAIN_SELF_TEST bounded=true partial_streams=true tree_reaped=true elapsed_ms=$($watch.ElapsedMilliseconds)"
}

function Write-Package2ProcessOutput {
    param(
        [Parameter(Mandatory = $true)]
        [pscustomobject]$Result
    )

    $Result.Output | ForEach-Object { Write-Host $_ }
}

$infrastructurePattern = '(?i)(builder error|C compilation error|compilation failed|undefined symbol|undefined reference|unresolved external|linker error|unknown method|cannot find (file|path|module|compiler|library|symbol)|cannot open (file|path)|no such file|failed to start|retrying [1-9][0-9]*/|max_retry:\s*[1-9])'
$fatalPattern = '(?i)(^|\s)(V panic:|panic:|fatal error:|unhandled exception|access violation|STATUS_ACCESS_VIOLATION|0xC0000005|segmentation fault|signal 11|stack overflow|illegal instruction|abort trap|process crashed|application crashed)'
$timeoutPattern = '(?i)(child timed out|process timed out|timed out after|execution timeout)'
$crashExitCodes = @(
    -2147483645, # STATUS_BREAKPOINT
    -1073741819, # STATUS_ACCESS_VIOLATION
    -1073741795, # STATUS_ILLEGAL_INSTRUCTION
    -1073741571, # STATUS_STACK_OVERFLOW
    -1073741510, # STATUS_CONTROL_C_EXIT
    -1073740940, # STATUS_HEAP_CORRUPTION
    -1073740791  # STATUS_STACK_BUFFER_OVERRUN
)

function Get-Package2RedClassification {
    param(
        [Parameter(Mandatory = $true)]
        [hashtable]$Case,
        [Parameter(Mandatory = $true)]
        [pscustomobject]$Result,
        [Parameter(Mandatory = $true)]
        [int]$ExpectedFailureExitCode
    )

    if ($ExpectedFailureExitCode -ne 1) {
        throw "Package 2 classifier requires the controlled V failure exit 1, got $ExpectedFailureExitCode"
    }
    $lines = @($Result.Output | ForEach-Object { ([string]$_).Trim() })
    $text = $lines -join "`n"
    $expectedName = "PACKAGE2_RED_TEST=$($Case.Name)"
    $expectedMarker = "PACKAGE2_RED_FAMILY=$($Case.Marker)"
    $expectedTerminal = "PACKAGE2_RED_TERMINAL=$($Case.Terminal)"
    $nameLines = @($lines | Where-Object { $_ -cmatch '^PACKAGE2_RED_TEST=' })
    $markerLines = @($lines | Where-Object { $_ -cmatch '^PACKAGE2_RED_FAMILY=' })
    $terminalLines = @($lines | Where-Object { $_ -cmatch '^PACKAGE2_RED_TERMINAL=' })
    $summaryLines = @($lines | Where-Object { $_ -cmatch '^Summary for all V _test\.v files:.*$' })
    $exactIdentity = $nameLines.Count -eq 1 -and $nameLines[0] -ceq $expectedName `
        -and $markerLines.Count -eq 1 -and $markerLines[0] -ceq $expectedMarker
    $exactTerminal = $terminalLines.Count -eq 1 `
        -and $terminalLines[0] -ceq $expectedTerminal
    $exactFailureSummary = $summaryLines.Count -eq 1 `
        -and $summaryLines[0] -cmatch '^Summary for all V _test\.v files: 1 failed, 1 total\.(?: .*)?$'
    $selectionMismatch = $text -match '(?im)^\s*retrying\s' `
        -or $text -match '(?im)\bskip(?:ped)?\b'

    if ($Result.TimedOut) {
        return [pscustomobject]@{ Kind = 'TimeoutFailure'; Detail = 'watchdog expired' }
    }
    if ($Result.InfrastructureError) {
        return [pscustomobject]@{ Kind = 'InfrastructureFailure'; Detail = $Result.InfrastructureError }
    }
    if ($null -eq $Result.ExitCode) {
        return [pscustomobject]@{ Kind = 'UnknownExit'; Detail = 'child returned no exit code' }
    }
    if ($text -match $timeoutPattern) {
        return [pscustomobject]@{ Kind = 'TimeoutFailure'; Detail = "exit=$($Result.ExitCode)" }
    }
    if ($text -match $infrastructurePattern) {
        return [pscustomobject]@{ Kind = 'InfrastructureFailure'; Detail = "exit=$($Result.ExitCode)" }
    }
    if ($Result.ExitCode -eq 0) {
        return [pscustomobject]@{ Kind = 'UnexpectedGreen'; Detail = 'exit=0' }
    }
    if ($Result.ExitCode -ne $ExpectedFailureExitCode) {
        return [pscustomobject]@{
            Kind = 'UnknownExit'
            Detail = "expected_exit=$ExpectedFailureExitCode actual_exit=$($Result.ExitCode)"
        }
    }
    if ($text -match $fatalPattern) {
        return [pscustomobject]@{ Kind = 'FatalFailure'; Detail = "exit=$($Result.ExitCode)" }
    }
    if (-not $exactIdentity) {
        return [pscustomobject]@{
            Kind = 'IdentityFailure'
            Detail = "expected_name=$expectedName expected_marker=$expectedMarker"
        }
    }
    if (-not $exactTerminal) {
        return [pscustomobject]@{
            Kind = 'TerminalFailure'
            Detail = "expected_terminal=$expectedTerminal"
        }
    }
    if (-not $exactFailureSummary -or $selectionMismatch) {
        return [pscustomobject]@{
            Kind = 'SummaryFailure'
            Detail = "expected exactly one '1 failed, 1 total' summary without skip/retry"
        }
    }
    return [pscustomobject]@{
        Kind = 'BehavioralRed'
        Detail = "exit=$ExpectedFailureExitCode"
    }
}

function Test-Package2RedClassifier {
    param(
        [Parameter(Mandatory = $true)]
        [int]$ExpectedFailureExitCode
    )

    if ($ExpectedFailureExitCode -ne 1) {
        throw "Package 2 classifier self-test requires expected exit 1, got $ExpectedFailureExitCode"
    }
    $probeCase = @{
        Name = 'test_package2_classifier_probe_red'
        Marker = 'classifier_probe'
        Terminal = 'classifier_probe_expected_failure'
    }
    $name = "PACKAGE2_RED_TEST=$($probeCase.Name)"
    $marker = "PACKAGE2_RED_FAMILY=$($probeCase.Marker)"
    $terminal = "PACKAGE2_RED_TERMINAL=$($probeCase.Terminal)"
    $failureSummary = 'Summary for all V _test.v files: 1 failed, 1 total. Elapsed time: 1 ms.'
    $validOutput = @($name, $marker, $terminal, 'assertion failed as expected', $failureSummary)
    $syntheticCases = @(
        @{
            Name = 'valid behavioral RED'
            Expected = 'BehavioralRed'
            Result = [pscustomobject]@{ ExitCode = 1; TimedOut = $false; InfrastructureError = ''; Output = $validOutput }
        }
        @{
            Name = 'missing test identity'
            Expected = 'IdentityFailure'
            Result = [pscustomobject]@{ ExitCode = 1; TimedOut = $false; InfrastructureError = ''; Output = @($marker, $terminal) }
        }
        @{
            Name = 'duplicate test identity'
            Expected = 'IdentityFailure'
            Result = [pscustomobject]@{ ExitCode = 1; TimedOut = $false; InfrastructureError = ''; Output = @($name, $name, $marker, $terminal) }
        }
        @{
            Name = 'wrong test identity'
            Expected = 'IdentityFailure'
            Result = [pscustomobject]@{ ExitCode = 1; TimedOut = $false; InfrastructureError = ''; Output = @('PACKAGE2_RED_TEST=test_wrong', $marker, $terminal) }
        }
        @{
            Name = 'missing family identity'
            Expected = 'IdentityFailure'
            Result = [pscustomobject]@{ ExitCode = 1; TimedOut = $false; InfrastructureError = ''; Output = @($name, $terminal) }
        }
        @{
            Name = 'duplicate family marker'
            Expected = 'IdentityFailure'
            Result = [pscustomobject]@{ ExitCode = 1; TimedOut = $false; InfrastructureError = ''; Output = @($name, $marker, $marker, $terminal) }
        }
        @{
            Name = 'wrong family identity'
            Expected = 'IdentityFailure'
            Result = [pscustomobject]@{ ExitCode = 1; TimedOut = $false; InfrastructureError = ''; Output = @($name, 'PACKAGE2_RED_FAMILY=wrong_family', $terminal) }
        }
        @{
            Name = 'missing terminal'
            Expected = 'TerminalFailure'
            Result = [pscustomobject]@{ ExitCode = 1; TimedOut = $false; InfrastructureError = ''; Output = @($name, $marker) }
        }
        @{
            Name = 'duplicate terminal'
            Expected = 'TerminalFailure'
            Result = [pscustomobject]@{ ExitCode = 1; TimedOut = $false; InfrastructureError = ''; Output = @($name, $marker, $terminal, $terminal) }
        }
        @{
            Name = 'wrong terminal'
            Expected = 'TerminalFailure'
            Result = [pscustomobject]@{ ExitCode = 1; TimedOut = $false; InfrastructureError = ''; Output = @($name, $marker, 'PACKAGE2_RED_TERMINAL=wrong_terminal') }
        }
        @{
            Name = 'missing failure summary'
            Expected = 'SummaryFailure'
            Result = [pscustomobject]@{ ExitCode = 1; TimedOut = $false; InfrastructureError = ''; Output = @($name, $marker, $terminal) }
        }
        @{
            Name = 'duplicate failure summary'
            Expected = 'SummaryFailure'
            Result = [pscustomobject]@{ ExitCode = 1; TimedOut = $false; InfrastructureError = ''; Output = @($validOutput + $failureSummary) }
        }
        @{
            Name = 'wrong failure summary'
            Expected = 'SummaryFailure'
            Result = [pscustomobject]@{ ExitCode = 1; TimedOut = $false; InfrastructureError = ''; Output = @($name, $marker, $terminal, 'Summary for all V _test.v files: 2 failed, 2 total.') }
        }
        @{
            Name = 'retrying selection'
            Expected = 'InfrastructureFailure'
            Result = [pscustomobject]@{ ExitCode = 1; TimedOut = $false; InfrastructureError = ''; Output = @($validOutput + 'retrying 1/1') }
        }
        @{
            Name = 'arbitrary nonzero after terminal'
            Expected = 'UnknownExit'
            Result = [pscustomobject]@{ ExitCode = 7; TimedOut = $false; InfrastructureError = ''; Output = $validOutput }
        }
        @{
            Name = 'silent NTSTATUS stack overflow'
            Expected = 'UnknownExit'
            Result = [pscustomobject]@{ ExitCode = -1073741571; TimedOut = $false; InfrastructureError = ''; Output = $validOutput }
        }
        @{
            Name = 'panic text after terminal'
            Expected = 'FatalFailure'
            Result = [pscustomobject]@{ ExitCode = 1; TimedOut = $false; InfrastructureError = ''; Output = @($validOutput + 'V panic: synthetic') }
        }
        @{
            Name = 'watchdog timeout after terminal'
            Expected = 'TimeoutFailure'
            Result = [pscustomobject]@{ ExitCode = $null; TimedOut = $true; InfrastructureError = ''; Output = $validOutput }
        }
        @{
            Name = 'process start error'
            Expected = 'InfrastructureFailure'
            Result = [pscustomobject]@{ ExitCode = $null; TimedOut = $false; InfrastructureError = 'v.exe did not start'; Output = @() }
        }
        @{
            Name = 'infrastructure failure after terminal'
            Expected = 'InfrastructureFailure'
            Result = [pscustomobject]@{ ExitCode = 1; TimedOut = $false; InfrastructureError = ''; Output = @($validOutput + 'C compilation error') }
        }
        @{
            Name = 'unexpected green after terminal'
            Expected = 'UnexpectedGreen'
            Result = [pscustomobject]@{ ExitCode = 0; TimedOut = $false; InfrastructureError = ''; Output = $validOutput }
        }
    )

    if ($syntheticCases.Count -gt 24) {
        throw 'Package 2 classifier self-test exceeded its fixed case bound'
    }
    $watch = [System.Diagnostics.Stopwatch]::StartNew()
    $accepted = 0
    foreach ($probe in $syntheticCases) {
        if ($watch.ElapsedMilliseconds -gt 5000) {
            throw 'Package 2 classifier self-test exceeded 5 seconds'
        }
        $actual = Get-Package2RedClassification -Case $probeCase -Result $probe.Result `
            -ExpectedFailureExitCode $ExpectedFailureExitCode
        if ($actual.Kind -cne $probe.Expected) {
            throw "Package 2 classifier self-test '$($probe.Name)' expected $($probe.Expected), got $($actual.Kind)"
        }
        if ($actual.Kind -ceq 'BehavioralRed') {
            $accepted++
        }
    }
    $watch.Stop()
    if ($accepted -ne 1) {
        throw "Package 2 classifier self-test accepted $accepted synthetic cases instead of exactly one"
    }
    Write-Host "PACKAGE2_CLASSIFIER_SELF_TEST accepted=$accepted rejected=$($syntheticCases.Count - $accepted) total=$($syntheticCases.Count) elapsed_ms=$($watch.ElapsedMilliseconds)"
}

function Get-Package2ExpectedFailureExitCode {
    param(
        [Parameter(Mandatory = $true)]
        [string]$VExe,
        [Parameter(Mandatory = $true)]
        [string]$Compiler
    )

    $tempRoot = if ($env:RUNNER_TEMP) { $env:RUNNER_TEMP } else { [System.IO.Path]::GetTempPath() }
    $token = [guid]::NewGuid().ToString('N')
    $sourcePath = Join-Path $tempRoot "package2_exit_probe_$($token)_test.v"
    $testName = 'test_package2_controlled_failure_exit'
    $testMarker = 'PACKAGE2_EXIT_PROBE_TEST=test_package2_controlled_failure_exit'
    $terminalMarker = 'PACKAGE2_EXIT_PROBE_TERMINAL=controlled_assertion'
    $source = @'
// vtest retry: 0
module main

fn test_package2_controlled_failure_exit() {
	eprintln('PACKAGE2_EXIT_PROBE_TEST=test_package2_controlled_failure_exit')
	eprintln('PACKAGE2_EXIT_PROBE_TERMINAL=controlled_assertion')
	assert false, 'PACKAGE2_EXIT_PROBE_ASSERTION=controlled_failure'
}
'@

    try {
        [System.IO.File]::WriteAllText($sourcePath, $source, [System.Text.UTF8Encoding]::new($false))
        $result = Invoke-Package2Process -FileName $VExe -Arguments @(
            '-cc', $Compiler,
            '-no-retry-compilation',
            '-no-parallel',
            '-d', 'gg_multiwindow',
            '-run-only', $testName,
            'test', $sourcePath
        )
        Write-Package2ProcessOutput -Result $result
        $lines = @($result.Output | ForEach-Object { ([string]$_).Trim() })
        $text = $lines -join "`n"
        $testLines = @($lines | Where-Object { $_ -cmatch '^PACKAGE2_EXIT_PROBE_TEST=' })
        $terminalLines = @($lines | Where-Object { $_ -cmatch '^PACKAGE2_EXIT_PROBE_TERMINAL=' })
        $exactMarkers = $testLines.Count -eq 1 -and $testLines[0] -ceq $testMarker `
            -and $terminalLines.Count -eq 1 -and $terminalLines[0] -ceq $terminalMarker

        if ($result.TimedOut -or $text -match $timeoutPattern) {
            throw "Package 2 controlled failure-exit probe timed out for $Compiler"
        }
        if ($result.InfrastructureError -or $text -match $infrastructurePattern) {
            throw "Package 2 controlled failure-exit probe had an infrastructure failure for $Compiler"
        }
        if ($null -eq $result.ExitCode) {
            throw "Package 2 controlled failure-exit probe returned no exit code for $Compiler"
        }
        if ($result.ExitCode -in $crashExitCodes -or $text -match $fatalPattern) {
            throw "Package 2 controlled failure-exit probe crashed or panicked for $Compiler"
        }
        if ($result.ExitCode -eq 0) {
            throw "Package 2 controlled failure-exit probe was unexpectedly green for $Compiler"
        }
        if (-not $exactMarkers) {
            throw "Package 2 controlled failure-exit probe lacked its exact markers for $Compiler"
        }
        if ($result.ExitCode -ne 1) {
            throw "Package 2 controlled failure-exit probe returned UNKNOWN_EXIT for ${Compiler}: expected=1 actual=$($result.ExitCode)"
        }
        Write-Host "PACKAGE2_EXPECTED_FAILURE_EXIT compiler=$Compiler exit=$($result.ExitCode) proof=controlled_v_assertion"
        return [int]$result.ExitCode
    } finally {
        Remove-Item -Force -ErrorAction SilentlyContinue $sourcePath
    }
}

function Invoke-Package2NoFlagDiagnostic {
    param(
        [Parameter(Mandatory = $true)]
        [string]$VExe,
        [Parameter(Mandatory = $true)]
        [string]$Compiler
    )

    $tempRoot = if ($env:RUNNER_TEMP) { $env:RUNNER_TEMP } else { [System.IO.Path]::GetTempPath() }
    $token = [guid]::NewGuid().ToString('N')
    $sourcePath = Join-Path $tempRoot "package2_no_flag_$token.v"
    $binaryPath = Join-Path $tempRoot "package2_no_flag_$token.exe"
    $vlibDir = (Resolve-Path '.\vlib').Path
    $source = @'
module main

import gg

fn main() {
	mut app := gg.App{}
	app.monitor_ids() or {
		println(err.msg())
		return
	}
	println('unexpected success')
}
'@

    try {
        [System.IO.File]::WriteAllText($sourcePath, $source, [System.Text.UTF8Encoding]::new($false))
        $compile = Invoke-Package2Process -FileName $VExe -Arguments @(
            '-cc', $Compiler,
            '-no-retry-compilation',
            '-gc', 'none',
            '-subsystem', 'console',
            '-path', "$vlibDir|@vlib|@vmodules",
            '-o', $binaryPath,
            $sourcePath
        )
        Write-Host "NO_FLAG_DIAGNOSTIC_COMPILE compiler=$Compiler exit=$($compile.ExitCode) timed_out=$($compile.TimedOut) infrastructure=$($compile.InfrastructureError)"
        Write-Package2ProcessOutput -Result $compile
        if ($compile.TimedOut -or $compile.InfrastructureError -or $compile.ExitCode -ne 0) {
            return
        }
        if (-not (Test-Path $binaryPath -PathType Leaf)) {
            Write-Host "NO_FLAG_DIAGNOSTIC_BINARY_MISSING compiler=$Compiler path=$binaryPath"
            return
        }

        $run = Invoke-Package2Process -FileName $binaryPath -Arguments @() -TimeoutSeconds 30
        Write-Host "NO_FLAG_DIAGNOSTIC_RUN compiler=$Compiler exit=$($run.ExitCode) timed_out=$($run.TimedOut) infrastructure=$($run.InfrastructureError)"
        Write-Package2ProcessOutput -Result $run
        $nonemptyLines = @($run.Output | ForEach-Object { ([string]$_).Trim() } | Where-Object { $_ -ne '' })
        $expected = 'gg.multiwindow: compile with `-d gg_multiwindow` to enable gg.App'
        $exactOutput = $nonemptyLines.Count -eq 1 -and $nonemptyLines[0] -ceq $expected
        Write-Host "NO_FLAG_DIAGNOSTIC_RESULT compiler=$Compiler exact_output=$exactOutput"
    } finally {
        Remove-Item -Force -ErrorAction SilentlyContinue $sourcePath, $binaryPath
    }
}

$core = 'vlib/x/multiwindow/service_native_win32_contract_red_test.v'
$noFlag = 'vlib/x/multiwindow/service_native_win32_no_flag_test.v'
$gg = 'vlib/gg/multiwindow_win32_services_red_d_gg_multiwindow_test.v'
$w2GreenTerminal = 'PACKAGE2_W2_GREEN_TERMINAL=behavioral_green:modal_child_first'
$greenCases = @(
    @{ Wave = 'W1'; File = $core; Name = 'test_win32_w1_native_authority_show_focus_and_fullscreen_contract' }
    @{ Wave = 'W1'; File = $core; Name = 'test_win32_w1_native_borrow_is_bounded_and_epoch_checked' }
    @{ Wave = 'W1'; File = $core; Name = 'test_win32_native_controls_state_and_independent_window_oracles_red' }
    @{ Wave = 'W1'; File = $gg; Name = 'test_win32_gg_public_borrow_is_live_callback_bounded_stale_and_defers_teardown_red' }
    @{ Wave = 'W1'; File = $core; Name = 'test_win32_native_raw_input_clipcursor_release_and_two_window_isolation' }
    @{ Wave = 'W2'; File = $core; Name = 'test_win32_native_modal_reenable_and_child_first_hwnd_destruction_red' }
)
$cases = @(
    @{ File = $core; Name = 'test_win32_native_conditional_titlebar_dwm_and_style_oracles_red'; Marker = 'titlebar_dwm_style'; Terminal = 'behavioral_red:titlebar_dwm_style' }
    @{ File = $gg; Name = 'test_win32_gg_public_facade_capabilities_are_distinct_and_complete_red'; Marker = 'gg_public_facade'; Terminal = 'behavioral_red:gg_public_facade' }
)

$names = @($cases | ForEach-Object { $_.Name } | Sort-Object -Unique)
$markers = @($cases | ForEach-Object { $_.Marker } | Sort-Object -Unique)
$terminals = @($cases | ForEach-Object { $_.Terminal } | Sort-Object -Unique)
if ($names.Count -ne $cases.Count -or $markers.Count -ne $cases.Count `
    -or $terminals.Count -ne $cases.Count) {
    throw 'Package 2 RED case names, family markers, and terminals must be unique'
}
$greenNames = @($greenCases | ForEach-Object { $_.Name } | Sort-Object -Unique)
$greenInRed = @($greenNames | Where-Object { $_ -in $names })
$w2GreenCount = @($greenCases | Where-Object { $_.Wave -ceq 'W2' }).Count
if ($greenNames.Count -ne 6 -or $greenInRed.Count -ne 0 `
    -or $w2GreenCount -ne 1 -or $cases.Count -ne 2) {
    throw 'Package 2 closure requires six GREEN and two disjoint RED cases'
}

$vexe = (Resolve-Path '.\v.exe').Path
if ($env:VFLAGS -match '(?i)(^|\s)-d\s+gg_multiwindow(\s|$)') {
    throw 'Package 2 no-flag gate inherited -d gg_multiwindow through VFLAGS'
}

Test-Package2BoundedProcessDrain

Write-Host "::group::Package 2 no-flag gate $Compiler"
Write-Host "PACKAGE2_NO_FLAG_START compiler=$Compiler define=disabled"
$noFlagArguments = @(
    '-cc', $Compiler,
    '-no-retry-compilation',
    '-no-parallel',
    '-subsystem', 'console',
    '-run-only', 'test_win32_nonreadback_no_flag_facade_stays_disabled',
    'test',
    $noFlag
)
$noFlagResult = Invoke-Package2Process -FileName $vexe -Arguments $noFlagArguments
Write-Package2ProcessOutput -Result $noFlagResult
$noFlagText = @($noFlagResult.Output | ForEach-Object { ([string]$_).Trim() }) -join "`n"
$noFlagSummaryLines = @($noFlagResult.Output | ForEach-Object { ([string]$_).Trim() } |
    Where-Object { $_ -cmatch '^Summary for all V _test\.v files:.*$' })
$noFlagExactSummary = $noFlagSummaryLines.Count -eq 1 `
    -and $noFlagSummaryLines[0] -cmatch '^Summary for all V _test\.v files: 1 passed, 1 total\.(?: .*)?$'
$noFlagFailure = if ($noFlagResult.TimedOut -or $noFlagText -match $timeoutPattern) {
    'timeout'
} elseif ($noFlagResult.InfrastructureError -or $noFlagText -match $infrastructurePattern) {
    'infrastructure'
} elseif ($null -eq $noFlagResult.ExitCode) {
    'unknown-no-exit'
} elseif ($noFlagResult.ExitCode -in $crashExitCodes -or $noFlagText -match $fatalPattern) {
    'crash-or-panic'
} elseif ($noFlagResult.ExitCode -ne 0) {
    "unexpected-exit-$($noFlagResult.ExitCode)"
} elseif (-not $noFlagExactSummary) {
    'unexpected-summary'
} else {
    ''
}
if ($noFlagFailure) {
    Write-Host "NO_FLAG_GATE_FAILURE compiler=$Compiler kind=$noFlagFailure exit=$($noFlagResult.ExitCode)"
    Invoke-Package2NoFlagDiagnostic -VExe $vexe -Compiler $Compiler
    Write-Host '::endgroup::'
    throw "Package 2 no-flag gate failed for ${Compiler}: $noFlagFailure"
}
Write-Host "NO_FLAG_GATE_PASS compiler=$Compiler passed=1 total=1"
Write-Host '::endgroup::'

foreach ($greenCase in $greenCases) {
    $greenWave = $greenCase.Wave
    $testName = $greenCase.Name
    $testFile = $greenCase.File
    Write-Host "::group::Package 2 $greenWave GREEN $Compiler $testName"
    try {
        Write-Host "PACKAGE2_${greenWave}_GREEN_START compiler=$Compiler case=$testName file=$testFile"
        $greenArguments = @(
            '-cc', $Compiler,
            '-no-retry-compilation',
            '-no-parallel',
            '-subsystem', 'console',
            '-d', 'gg_multiwindow',
            '-run-only', $testName,
            'test', $testFile
        )
        if ($greenWave -ceq 'W2') {
            $greenArguments = @('-stats') + $greenArguments
        }
        $greenResult = Invoke-Package2Process -FileName $vexe -Arguments $greenArguments
        Write-Package2ProcessOutput -Result $greenResult
        $greenLines = @(
            $greenResult.Output | ForEach-Object { ([string]$_).Trim() }
        )
        $greenText = $greenLines -join "`n"
        $greenSummaryLines = @(
            $greenLines |
                Where-Object { $_ -cmatch '^Summary for all V _test\.v files:.*$' }
        )
        $greenExactSummary = $greenSummaryLines.Count -eq 1 `
            -and $greenSummaryLines[0] -cmatch '^Summary for all V _test\.v files: 1 passed, 1 total\.(?: .*)?$'
        $greenTerminalLines = @(
            $greenLines |
                Where-Object { $_ -cmatch '^PACKAGE2_(?:RED_TERMINAL|W2_GREEN_TERMINAL)=' }
        )
        $greenExactTerminal = $greenWave -cne 'W2' `
            -or ($greenTerminalLines.Count -eq 1 `
                -and $greenTerminalLines[0] -ceq $w2GreenTerminal)
        $greenSelectionMismatch = $greenText -match '(?im)^\s*retrying\s' `
            -or $greenText -match '(?im)\b(skipped?|0 passed)\b'
        $greenFailure = if ($greenResult.TimedOut -or $greenText -match $timeoutPattern) {
            'timeout'
        } elseif ($greenResult.InfrastructureError -or $greenText -match $infrastructurePattern) {
            'infrastructure'
        } elseif ($null -eq $greenResult.ExitCode) {
            'unknown-no-exit'
        } elseif ($greenResult.ExitCode -in $crashExitCodes -or $greenText -match $fatalPattern) {
            'crash-or-panic'
        } elseif ($greenResult.ExitCode -ne 0) {
            "unexpected-exit-$($greenResult.ExitCode)"
        } elseif (-not $greenExactTerminal) {
            'terminal-mismatch'
        } elseif (-not $greenExactSummary -or $greenSelectionMismatch) {
            'selection-or-summary-mismatch'
        } else {
            ''
        }
        if ($greenFailure) {
            Write-Host "PACKAGE2_${greenWave}_GREEN_FAILURE compiler=$Compiler case=$testName kind=$greenFailure exit=$($greenResult.ExitCode)"
            throw "Package 2 $greenWave GREEN gate failed for ${Compiler}/${testName}: $greenFailure"
        }
        Write-Host "PACKAGE2_${greenWave}_GREEN_PASS compiler=$Compiler case=$testName passed=1 total=1"
    } finally {
        Write-Host '::endgroup::'
    }
}

Write-Host "::group::Package 2 controlled failure-exit probe $Compiler"
$expectedFailureExitCode = Get-Package2ExpectedFailureExitCode -VExe $vexe -Compiler $Compiler
Write-Host '::endgroup::'
Test-Package2RedClassifier -ExpectedFailureExitCode $expectedFailureExitCode

$behavioralRed = 0
$infrastructureFailures = 0
$identityFailures = 0
$fatalFailures = 0
$timeoutFailures = 0
$terminalFailures = 0
$summaryFailures = 0
$unknownExits = 0
$unexpectedGreen = 0

foreach ($case in $cases) {
    Write-Host "::group::Package 2 RED $Compiler $($case.Name)"
    $arguments = @(
        '-cc', $Compiler,
        '-no-retry-compilation',
        '-no-parallel',
        '-subsystem', 'console',
        '-d', 'gg_multiwindow',
        '-run-only', $case.Name,
        'test', $case.File
    )
    $result = Invoke-Package2Process -FileName $vexe -Arguments $arguments
    Write-Package2ProcessOutput -Result $result
    $classification = Get-Package2RedClassification -Case $case -Result $result `
        -ExpectedFailureExitCode $expectedFailureExitCode
    switch ($classification.Kind) {
        'BehavioralRed' {
            $behavioralRed++
            Write-Host "BEHAVIORAL_RED compiler=$Compiler case=$($case.Name) marker=$($case.Marker) exit=$($result.ExitCode)"
        }
        'InfrastructureFailure' {
            $infrastructureFailures++
            Write-Host "INFRASTRUCTURE_FAILURE compiler=$Compiler case=$($case.Name) detail=$($classification.Detail)"
        }
        'IdentityFailure' {
            $identityFailures++
            Write-Host "IDENTITY_FAILURE compiler=$Compiler case=$($case.Name) detail=$($classification.Detail)"
        }
        'FatalFailure' {
            $fatalFailures++
            Write-Host "FATAL_FAILURE compiler=$Compiler case=$($case.Name) detail=$($classification.Detail)"
        }
        'TimeoutFailure' {
            $timeoutFailures++
            Write-Host "TIMEOUT_FAILURE compiler=$Compiler case=$($case.Name) detail=$($classification.Detail)"
        }
        'TerminalFailure' {
            $terminalFailures++
            Write-Host "TERMINAL_FAILURE compiler=$Compiler case=$($case.Name) detail=$($classification.Detail)"
        }
        'SummaryFailure' {
            $summaryFailures++
            Write-Host "SUMMARY_FAILURE compiler=$Compiler case=$($case.Name) detail=$($classification.Detail)"
        }
        'UnknownExit' {
            $unknownExits++
            Write-Host "UNKNOWN_EXIT compiler=$Compiler case=$($case.Name) detail=$($classification.Detail)"
        }
        'UnexpectedGreen' {
            $unexpectedGreen++
            Write-Host "UNEXPECTED_GREEN compiler=$Compiler case=$($case.Name)"
        }
        default {
            throw "Package 2 classifier returned unknown kind '$($classification.Kind)'"
        }
    }
    Write-Host '::endgroup::'
}

$rejected = $cases.Count - $behavioralRed
Write-Host "PACKAGE2_RED_SUMMARY compiler=$Compiler accepted=$behavioralRed rejected=$rejected behavioral_red=$behavioralRed infrastructure_failures=$infrastructureFailures identity_failures=$identityFailures terminal_failures=$terminalFailures summary_failures=$summaryFailures fatal_failures=$fatalFailures timeout_failures=$timeoutFailures unknown_exits=$unknownExits unexpected_green=$unexpectedGreen total=$($cases.Count)"
if ($infrastructureFailures -ne 0) {
    throw "Package 2 RED matrix had $infrastructureFailures infrastructure failure(s) for $Compiler"
}
if ($identityFailures -ne 0) {
    throw "Package 2 RED matrix had $identityFailures identity/marker failure(s) for $Compiler"
}
if ($fatalFailures -ne 0) {
    throw "Package 2 RED matrix had $fatalFailures crash/panic failure(s) for $Compiler"
}
if ($timeoutFailures -ne 0) {
    throw "Package 2 RED matrix had $timeoutFailures timeout failure(s) for $Compiler"
}
if ($terminalFailures -ne 0) {
    throw "Package 2 RED matrix had $terminalFailures missing/wrong terminal failure(s) for $Compiler"
}
if ($summaryFailures -ne 0) {
    throw "Package 2 RED matrix had $summaryFailures selection/summary failure(s) for $Compiler"
}
if ($unknownExits -ne 0) {
    throw "Package 2 RED matrix had $unknownExits unknown child exit(s) for $Compiler"
}
if ($unexpectedGreen -ne 0) {
    throw "Package 2 RED matrix had $unexpectedGreen unexpectedly green case(s) for $Compiler"
}
if ($behavioralRed -ne $cases.Count) {
    throw "Package 2 RED matrix did not observe every expected behavioral RED for $Compiler"
}
