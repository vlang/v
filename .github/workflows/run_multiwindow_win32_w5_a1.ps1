[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [ValidateSet('msvc', 'gcc', 'tcc')]
    [string]$Compiler,

    [Parameter(Mandatory = $true)]
    [ValidateSet('Green')]
    [string]$Expectation,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedTestSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedOracleSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedTupleSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedA0HeaderSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedA0MainSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedA0TupleSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedA0RunnerSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedGgFacadeSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedGgNoFlagSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedGgServiceTypesSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedAppSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedTypesSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedBackendSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedServiceApiSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedServiceTypesSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedServiceRegistrySha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedEventDeliverySha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedPublicRoutingSurfaceSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedServiceBackendSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedEventDispatchSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedWin32BackendSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedWin32BackendHelpersSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedWin32ServiceBackendSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedWin32ServiceNativeSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedGreenProductionSurfaceSha256,

    [Parameter(Mandatory = $true)]
    [ValidatePattern('^[0-9a-fA-F]{64}$')]
    [string]$ExpectedRunnerSha256
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$testFile = 'vlib/gg/testdata/multiwindow_win32_raw_input_w5_a1_public_red_test.v'
$oracle = 'vlib/x/multiwindow/testdata/win32_raw_input_w5_a1_oracle.h'
$caseName = 'test_win32_public_mouse_lock_real_raw_delta_red'
$family = 'mouse_lock_raw_delta_public'
$knownTestSha256 = '005d3f7c7668bd18d593ba5f42e94cf247058c5f61ca46ce6d6a42bac03b5000'
$knownOracleSha256 = '7362f17f06ea6b0ab5a64c74abf2407a0e284cbace2c10809a5235d406d16461'
$knownTupleSha256 = '16739d9b8b73d9615ba3037c9e18219d6bf1271ef1d2f7dba4b2f2e53b1efac5'

$a0Header = 'vlib/x/multiwindow/testdata/win32_raw_input_w5_preflight.h'
$a0Main = 'vlib/x/multiwindow/testdata/win32_raw_input_w5_preflight.c'
$a0Runner = '.github/workflows/run_multiwindow_win32_w5_a0.ps1'
$knownA0HeaderSha256 = '9decdc2e825c91da2b9d2ce0c98cb8ccdad5c4800a690863d166135eb188e916'
$knownA0MainSha256 = '5f30869ae97f680032e622273ea69986d612cc82cad2adf53e4c3b1415154301'
$knownA0TupleSha256 = '99ef24bf101dc83543f26f86cb23a80c82607e0f1b35c32e9dbb431368ecc7c4'
$knownA0RunnerSha256 = 'ce4da58e24730899f970a7566dc65d82eb0c07d38f74efcaeeb02c450a011b68'

$redMarkers = @(
    'PACKAGE2_W5_A1_IDENTITY=win32_public_mouse_lock_raw_delta'
    'PACKAGE2_RED_TEST=test_win32_public_mouse_lock_real_raw_delta_red'
    'PACKAGE2_RED_FAMILY=mouse_lock_raw_delta_public'
    'PACKAGE2_W5_A1_REACHED=live_public_mouse_lock_capability'
    'PACKAGE2_W5_A1_PRODUCT_GAP=mouse_lock_capability_unsupported'
    'PACKAGE2_W5_A1_CLEANUP_OK=no_oracle_mutation'
    'PACKAGE2_W5_A1_SUMMARY=accepted:0 rejected:1 total:1'
    'PACKAGE2_RED_TERMINAL=behavioral_red:mouse_lock_raw_delta_public'
)
$greenMarkers = @(
    'PACKAGE2_W5_A1_IDENTITY=win32_public_mouse_lock_raw_delta'
    'PACKAGE2_RED_TEST=test_win32_public_mouse_lock_real_raw_delta_red'
    'PACKAGE2_RED_FAMILY=mouse_lock_raw_delta_public'
    'PACKAGE2_W5_A1_REACHED=live_public_mouse_lock_raw_delta'
    'PACKAGE2_W5_A1_PRODUCT_OK=mouse_lock_acquire_clipped_delta_explicit_unlock'
    'PACKAGE2_W5_A1_CLEANUP_OK=baseline_restored_without_rescue'
    'PACKAGE2_W5_A1_SUMMARY=accepted:1 rejected:0 total:1'
    'PACKAGE2_W5_A1_TERMINAL=native_pass:mouse_lock_raw_delta_public'
)
$a0Markers = @(
    'PACKAGE2_W5_A0_IDENTITY=win32_raw_input_sendinput_preflight'
    'PACKAGE2_W5_A0_FAMILY=raw_input_environment'
    'PACKAGE2_W5_A0_REACHED=sendinput_rawinput_correlation'
    'PACKAGE2_W5_A0_SOURCE_OK=injected_mouse_extra_tag'
    'PACKAGE2_W5_A0_RAW_OK=real_hrawinput_relative'
    'PACKAGE2_W5_A0_CLEANUP_OK=restored'
    'PACKAGE2_W5_A0_SUMMARY=accepted:1 rejected:0 total:1'
    'PACKAGE2_W5_A0_TERMINAL=native_pass:raw_input_environment'
)

$publicSurface = @(
    [pscustomobject]@{ Path = 'vlib/gg/multiwindow_d_gg_multiwindow.v'; Known = '5823f13606204ce15b8ea41761c4a4fba47261b71666f8280f0023f1e83c4cc2'; Passed = $ExpectedGgFacadeSha256 }
    [pscustomobject]@{ Path = 'vlib/gg/multiwindow_notd_gg_multiwindow.v'; Known = 'ad528c64cf18bce9415dd0e50513fb87d8d0b58c8ac4a99025a5c8955e7d9549'; Passed = $ExpectedGgNoFlagSha256 }
    [pscustomobject]@{ Path = 'vlib/gg/multiwindow_service_types.v'; Known = '9f17334ba5e5aff51f26d55476c83878856a8494a890aa64ee6434a52c1ecfeb'; Passed = $ExpectedGgServiceTypesSha256 }
    [pscustomobject]@{ Path = 'vlib/x/multiwindow/app.v'; Known = 'af32eec55c119907a8bbf544842131018d36d3887347e26486e067a79d78a946'; Passed = $ExpectedAppSha256 }
    [pscustomobject]@{ Path = 'vlib/x/multiwindow/types.v'; Known = '0a9058ba7928945e24284c867eb6182336ea3b1d41e1337786c71105b1fe93b3'; Passed = $ExpectedTypesSha256 }
    [pscustomobject]@{ Path = 'vlib/x/multiwindow/backend.v'; Known = 'dcd0f088c235ae030f8bbc9ab63082c38d93f6f1194373e74d8dcb454d100a3f'; Passed = $ExpectedBackendSha256 }
    [pscustomobject]@{ Path = 'vlib/x/multiwindow/service_api.v'; Known = 'b7e4bc26e85f897a72387e82f4a7a3d545f21230be5c6b9df4fa40a4c82b701f'; Passed = $ExpectedServiceApiSha256 }
    [pscustomobject]@{ Path = 'vlib/x/multiwindow/service_types.v'; Known = 'cd8326b81201727618e758d79970b7a77223fd6418e18afda44a526a3b0b4267'; Passed = $ExpectedServiceTypesSha256 }
    [pscustomobject]@{ Path = 'vlib/x/multiwindow/service_registry.v'; Known = 'db244497fd23974c1739d6c7a8bc57a2e3137c8765d4bc7f789efd80a59066b7'; Passed = $ExpectedServiceRegistrySha256 }
    [pscustomobject]@{ Path = 'vlib/x/multiwindow/event_delivery.v'; Known = 'd38c868f574c02fc46e047da25e770e5b9f672500d796694b5ef35bf042cdae0'; Passed = $ExpectedEventDeliverySha256 }
)
$knownPublicRoutingSurfaceSha256 = 'a4a03215a80f94584f7c9c11f0e58ecdc80175e959b6d3a9145d36606e0cbcd6'

$greenProductionSurface = @(
    [pscustomobject]@{ Path = 'vlib/x/multiwindow/service_backend.v'; Known = '8ad39d63360446ef7049c726544200780eff61ff39917830f7024243dfd363be'; Passed = $ExpectedServiceBackendSha256 }
    [pscustomobject]@{ Path = 'vlib/x/multiwindow/event_dispatch_d_gg_multiwindow.v'; Known = '7ef6c7cbd34511d6c19c1a3a269b46876db791b9412bb88e3a8d4dcd543bfd4e'; Passed = $ExpectedEventDispatchSha256 }
    [pscustomobject]@{ Path = 'vlib/x/multiwindow/win32_backend.c.v'; Known = '8c915ec5bd9b116d98b8711bbd46f3afd5d6761800d7f91fed0deef6ac26cbaf'; Passed = $ExpectedWin32BackendSha256 }
    [pscustomobject]@{ Path = 'vlib/x/multiwindow/win32_backend_helpers.h'; Known = '2265a247f626621d69e823c09a79a5611dd187880120ba493636e73858c74a4d'; Passed = $ExpectedWin32BackendHelpersSha256 }
	[pscustomobject]@{ Path = 'vlib/x/multiwindow/win32_service_backend.c.v'; Known = 'faaf67be3e0381ea8a0e7a753b477b7965dde6e655f83f220eca036da19c5df6'; Passed = $ExpectedWin32ServiceBackendSha256 }
	[pscustomobject]@{ Path = 'vlib/x/multiwindow/win32_service_native.h'; Known = '2ffc93e245a43c4a557fd7cc66ebcdda8874b9783ec3050ba754657a97efd033'; Passed = $ExpectedWin32ServiceNativeSha256 }
)
$knownGreenProductionSurfaceSha256 = 'f570ae10e0479b4628df305ea51ba44779f528a6767886e2b2fed4dbfa4e4f9e'

$fatalPattern = '(?i)(fatal error|unhandled exception|access violation|STATUS_ACCESS_VIOLATION|0xC0000005|segmentation fault|stack overflow|illegal instruction|abort trap|process crashed|application crashed|V panic:)'
$compilerDiagnosticPattern = '(?im)(C compilation error|builder error|failed to compile|cannot compile|fatal error|error C[0-9]{4}|warning C[0-9]{4}|warning:|error:|undefined reference|unresolved external symbol|LNK[0-9]{4})'
$selectionAnomalyPattern = '(?im)(^\s*retrying\s|\bSKIP(?:PED)?\b)'
$crashExitCodes = @(
    -2147483645,
    -1073741819,
    -1073741795,
    -1073741571,
    -1073741510,
    -1073740940,
    -1073740791
)

function Get-W5A1TextSha256 {
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

function Assert-W5A1FileHash {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Path,
        [Parameter(Mandatory = $true)]
        [string]$Expected
    )

    if (-not (Test-Path -LiteralPath $Path -PathType Leaf)) {
        throw "W5 A1 hashed input is missing: $Path"
    }
    $actual = (Get-FileHash -LiteralPath $Path -Algorithm SHA256).Hash.ToLowerInvariant()
    $normalizedExpected = $Expected.ToLowerInvariant()
    if ($actual -cne $normalizedExpected) {
        throw "W5 A1 hash mismatch for ${Path}: expected=$normalizedExpected actual=$actual"
    }
    Write-Host "PACKAGE2_W5_A1_HASH_OK path=$Path sha256=$actual"
}

function Assert-W5A1FailedReap {
    param(
        [Parameter(Mandatory = $true)]
        [AllowEmptyString()]
        [string]$Failure
    )

    if ($Failure) {
        throw "W5 A1 process supervision failed: $Failure"
    }
}

function Receive-W5A1ProcessOutput {
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
        Write-Verbose "W5 A1 output aggregate fault: $($_.Exception.Message)"
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

function Invoke-W5A1BoundedProcess {
    param(
        [Parameter(Mandatory = $true)]
        [string]$FileName,
        [Parameter(Mandatory = $true)]
        [AllowEmptyCollection()]
        [string[]]$Arguments,
        [Parameter(Mandatory = $true)]
        [string]$WorkingDirectory,
        [Parameter(Mandatory = $true)]
        [string]$TempDirectory,
        [int]$TimeoutSeconds = 240,
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
    $startInfo.Environment['VFLAGS'] = ''
    $startInfo.Environment['VJOBS'] = '1'
    $startInfo.Environment['VTEST_RETRY_MAX'] = '0'
    $startInfo.Environment['VTEST_FAIL_FAST'] = '1'
    $startInfo.Environment['V_C_ERROR_BUG_REPORT_DISABLED'] = '1'
    $startInfo.Environment['VCOLORS'] = 'never'
    $startInfo.Environment['NO_COLOR'] = '1'
    $startInfo.Environment['TEMP'] = $TempDirectory
    $startInfo.Environment['TMP'] = $TempDirectory
    $startInfo.Environment['TMPDIR'] = $TempDirectory
    $startInfo.Environment['VCACHE'] = (Join-Path $TempDirectory 'vcache')
    [void]$startInfo.Environment.Remove('VTEST_ONLY_FN')
    [void]$startInfo.Environment.Remove('VTEST_ONLY')
    [void]$startInfo.Environment.Remove('VTEST_RUNNER')
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
        $drain = Receive-W5A1ProcessOutput -StdoutTask $stdoutTask `
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
    Assert-W5A1FailedReap -Failure $failedReap

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

function Write-W5A1ProcessOutput {
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

function Assert-W5A1CommandGreen {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Label,
        [Parameter(Mandatory = $true)]
        [pscustomobject]$Result,
        [switch]$RejectDiagnostics
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
    if ($RejectDiagnostics -and $text -match $compilerDiagnosticPattern) {
        throw "$Label emitted a compiler diagnostic"
    }
}

function Get-W5A1StrictStream {
    param(
        [Parameter(Mandatory = $true)]
        [AllowEmptyString()]
        [string]$Text
    )

    if ($Text.IndexOf([char]0) -ge 0) {
        return [pscustomobject]@{ Valid = $false; Detail = 'NUL byte'; Lines = [string[]]@() }
    }
    $normalized = $Text.Replace("`r`n", "`n")
    if ($normalized.Contains("`r")) {
        return [pscustomobject]@{ Valid = $false; Detail = 'bare CR'; Lines = [string[]]@() }
    }
    $segments = [string[]]$normalized.Split(
        [char[]]@([char]10),
        [System.StringSplitOptions]::None
    )
    if ($segments.Count -gt 0 -and $segments[$segments.Count - 1] -ceq '') {
        $lines = if ($segments.Count -eq 1) {
            [string[]]@()
        } else {
            [string[]]$segments[0..($segments.Count - 2)]
        }
    } else {
        $lines = $segments
    }
    return [pscustomobject]@{ Valid = $true; Detail = ''; Lines = [string[]]$lines }
}

function Get-W5A1Classification {
    param(
        [Parameter(Mandatory = $true)]
        [pscustomobject]$Result,
        [Parameter(Mandatory = $true)]
        [ValidateSet('Red', 'Green')]
        [string]$ExpectedState,
        [int]$ExpectedFailureExitCode = 1
    )

    $stdoutRaw = [string]$Result.Stdout
    $stderrRaw = [string]$Result.Stderr
    if ($Result.InfrastructureError) {
        return [pscustomobject]@{ Kind = 'InfrastructureFailure'; Detail = $Result.InfrastructureError }
    }
    if ($stdoutRaw -cmatch 'PACKAGE2_W5_A1_INFRA=' -or $stderrRaw -cmatch 'PACKAGE2_W5_A1_INFRA=') {
        return [pscustomobject]@{ Kind = 'InfrastructureFailure'; Detail = 'child emitted infra marker' }
    }
    if ($Result.TimedOut) {
        return [pscustomobject]@{ Kind = 'TimeoutFailure'; Detail = 'watchdog timeout' }
    }
    $stdout = Get-W5A1StrictStream -Text $stdoutRaw
    $stderr = Get-W5A1StrictStream -Text $stderrRaw
    if (-not $stdout.Valid -or -not $stderr.Valid) {
        return [pscustomobject]@{ Kind = 'StreamFailure'; Detail = "stdout=$($stdout.Detail) stderr=$($stderr.Detail)" }
    }
    if ($null -eq $Result.ExitCode) {
        return [pscustomobject]@{ Kind = 'UnknownExit'; Detail = 'no exit code' }
    }
    if (($Result.ExitCode -in $crashExitCodes) -or ($stdoutRaw -match $fatalPattern) -or ($stderrRaw -match $fatalPattern)) {
        return [pscustomobject]@{ Kind = 'FatalFailure'; Detail = "exit=$($Result.ExitCode)" }
    }
    if ($stdoutRaw -match $compilerDiagnosticPattern -or $stderrRaw -match $compilerDiagnosticPattern) {
        return [pscustomobject]@{ Kind = 'InfrastructureFailure'; Detail = 'compiler diagnostic' }
    }
    if ($stdoutRaw -match $selectionAnomalyPattern -or $stderrRaw -match $selectionAnomalyPattern) {
        return [pscustomobject]@{ Kind = 'SelectionFailure'; Detail = 'retry or skip output' }
    }

    $stdoutMarkers = @($stdout.Lines | Where-Object { $_ -cmatch 'PACKAGE2_' })
    $stderrMarkers = @($stderr.Lines | Where-Object { $_ -cmatch 'PACKAGE2_' })
    $stdoutSummaries = @($stdout.Lines | Where-Object { $_ -cmatch '^Summary for all V _test\.v files:.*$' })
    $stderrSummaries = @($stderr.Lines | Where-Object { $_ -cmatch '^Summary for all V _test\.v files:.*$' })
    if ($ExpectedState -ceq 'Red') {
        if ($stdoutMarkers.Count -ne 0) {
            return [pscustomobject]@{ Kind = 'WrongStreamFailure'; Detail = 'RED package marker appeared on stdout' }
        }
        $expectedMarkers = $redMarkers
        $actualMarkers = $stderrMarkers
    } else {
        if ($stderrMarkers.Count -ne 0) {
            return [pscustomobject]@{ Kind = 'WrongStreamFailure'; Detail = 'GREEN package marker appeared on stderr' }
        }
        $expectedMarkers = $greenMarkers
        $actualMarkers = $stdoutMarkers
    }
    if ($actualMarkers.Count -ne $expectedMarkers.Count) {
        return [pscustomobject]@{ Kind = 'TranscriptFailure'; Detail = "expected $($expectedMarkers.Count) exact package markers, found $($actualMarkers.Count)" }
    }
    for ($index = 0; $index -lt $expectedMarkers.Count; $index++) {
        if ($actualMarkers[$index] -cne $expectedMarkers[$index]) {
            return [pscustomobject]@{ Kind = 'TranscriptFailure'; Detail = "package marker mismatch at line $($index + 1)" }
        }
    }
    if ($stderrSummaries.Count -ne 0) {
        return [pscustomobject]@{ Kind = 'SummaryStreamFailure'; Detail = 'V test summary appeared on stderr' }
    }

    if ($ExpectedState -ceq 'Red') {
        $exactSummary = $stdoutSummaries.Count -eq 1 `
            -and $stdoutSummaries[0] -cmatch '^Summary for all V _test\.v files: 1 failed, 1 total\.(?: .*)?$'
        if (-not $exactSummary) {
            return [pscustomobject]@{ Kind = 'SummaryFailure'; Detail = 'expected exactly one stdout 1 failed, 1 total summary' }
        }
        if ($Result.ExitCode -ne $ExpectedFailureExitCode) {
            return [pscustomobject]@{ Kind = 'ExitFailure'; Detail = "expected=$ExpectedFailureExitCode actual=$($Result.ExitCode)" }
        }
        return [pscustomobject]@{ Kind = 'BehavioralRed'; Detail = "exit=$ExpectedFailureExitCode" }
    }

    $exactSummary = $stdoutSummaries.Count -eq 1 `
        -and $stdoutSummaries[0] -cmatch '^Summary for all V _test\.v files: 1 passed, 1 total\.(?: .*)?$'
    if (-not $exactSummary) {
        return [pscustomobject]@{ Kind = 'SummaryFailure'; Detail = 'expected exactly one stdout 1 passed, 1 total summary' }
    }
    if ($Result.ExitCode -ne 0) {
        return [pscustomobject]@{ Kind = 'ExitFailure'; Detail = "expected=0 actual=$($Result.ExitCode)" }
    }
    return [pscustomobject]@{ Kind = 'BehavioralGreen'; Detail = 'exit=0' }
}

function Get-W5A1NoFlagClassification {
    param(
        [Parameter(Mandatory = $true)]
        [pscustomobject]$Result
    )

    $stdoutRaw = [string]$Result.Stdout
    $stderrRaw = [string]$Result.Stderr
    if ($Result.InfrastructureError) {
        return [pscustomobject]@{ Kind = 'InfrastructureFailure'; Detail = $Result.InfrastructureError }
    }
    if ($stdoutRaw -cmatch 'PACKAGE2_W5_A1_INFRA=' -or $stderrRaw -cmatch 'PACKAGE2_W5_A1_INFRA=') {
        return [pscustomobject]@{ Kind = 'InfrastructureFailure'; Detail = 'child emitted infra marker' }
    }
    if ($Result.TimedOut) {
        return [pscustomobject]@{ Kind = 'TimeoutFailure'; Detail = 'watchdog timeout' }
    }
    $stdout = Get-W5A1StrictStream -Text $stdoutRaw
    $stderr = Get-W5A1StrictStream -Text $stderrRaw
    if (-not $stdout.Valid -or -not $stderr.Valid) {
        return [pscustomobject]@{ Kind = 'StreamFailure'; Detail = "stdout=$($stdout.Detail) stderr=$($stderr.Detail)" }
    }
    if ($null -eq $Result.ExitCode) {
        return [pscustomobject]@{ Kind = 'UnknownExit'; Detail = 'no exit code' }
    }
    if (($Result.ExitCode -in $crashExitCodes) -or ($stdoutRaw -match $fatalPattern) -or ($stderrRaw -match $fatalPattern)) {
        return [pscustomobject]@{ Kind = 'FatalFailure'; Detail = "exit=$($Result.ExitCode)" }
    }
    if ($stdoutRaw -match $compilerDiagnosticPattern -or $stderrRaw -match $compilerDiagnosticPattern) {
        return [pscustomobject]@{ Kind = 'InfrastructureFailure'; Detail = 'compiler diagnostic' }
    }
    if ($stdoutRaw -match $selectionAnomalyPattern -or $stderrRaw -match $selectionAnomalyPattern) {
        return [pscustomobject]@{ Kind = 'SelectionFailure'; Detail = 'retry or skip output' }
    }

    $stdoutMarkers = @($stdout.Lines | Where-Object { $_ -cmatch 'PACKAGE2_' })
    $stderrMarkers = @($stderr.Lines | Where-Object { $_ -cmatch 'PACKAGE2_' })
    if ($stdoutMarkers.Count -ne 0 -or $stderrMarkers.Count -ne 0) {
        return [pscustomobject]@{ Kind = 'MarkerLeakFailure'; Detail = 'no-flag run emitted a package marker' }
    }
    $stdoutSummaries = @($stdout.Lines | Where-Object { $_ -cmatch '^Summary for all V _test\.v files:.*$' })
    $stderrSummaries = @($stderr.Lines | Where-Object { $_ -cmatch '^Summary for all V _test\.v files:.*$' })
    if ($stderrSummaries.Count -ne 0) {
        return [pscustomobject]@{ Kind = 'SummaryStreamFailure'; Detail = 'no-flag summary appeared on stderr' }
    }
    $exactSummary = $stdoutSummaries.Count -eq 1 `
        -and $stdoutSummaries[0] -cmatch '^Summary for all V _test\.v files: 1 passed, 1 total\.(?: .*)?$'
    if (-not $exactSummary) {
        return [pscustomobject]@{ Kind = 'SummaryFailure'; Detail = 'expected exactly one stdout 1 passed, 1 total summary' }
    }
    if ($Result.ExitCode -ne 0) {
        return [pscustomobject]@{ Kind = 'ExitFailure'; Detail = "expected=0 actual=$($Result.ExitCode)" }
    }
    return [pscustomobject]@{ Kind = 'NoFlagPass'; Detail = 'exit=0 markers=0' }
}

function Get-W5A1ControlledExitClassification {
    param(
        [Parameter(Mandatory = $true)]
        [pscustomobject]$Result
    )

    $stdoutRaw = [string]$Result.Stdout
    $stderrRaw = [string]$Result.Stderr
    if ($Result.InfrastructureError -or $stdoutRaw -cmatch 'PACKAGE2_W5_A1_INFRA=' `
        -or $stderrRaw -cmatch 'PACKAGE2_W5_A1_INFRA=') {
        return [pscustomobject]@{ Kind = 'InfrastructureFailure'; Detail = 'controlled-exit infrastructure failure' }
    }
    if ($Result.TimedOut) {
        return [pscustomobject]@{ Kind = 'TimeoutFailure'; Detail = 'watchdog timeout' }
    }
    $stdout = Get-W5A1StrictStream -Text $stdoutRaw
    $stderr = Get-W5A1StrictStream -Text $stderrRaw
    if (-not $stdout.Valid -or -not $stderr.Valid) {
        return [pscustomobject]@{ Kind = 'StreamFailure'; Detail = "stdout=$($stdout.Detail) stderr=$($stderr.Detail)" }
    }
    if ($null -eq $Result.ExitCode) {
        return [pscustomobject]@{ Kind = 'UnknownExit'; Detail = 'no exit code' }
    }
    if (($Result.ExitCode -in $crashExitCodes) -or ($stdoutRaw -match $fatalPattern) -or ($stderrRaw -match $fatalPattern)) {
        return [pscustomobject]@{ Kind = 'FatalFailure'; Detail = "exit=$($Result.ExitCode)" }
    }
    if ($stdoutRaw -match $compilerDiagnosticPattern -or $stderrRaw -match $compilerDiagnosticPattern) {
        return [pscustomobject]@{ Kind = 'InfrastructureFailure'; Detail = 'compiler diagnostic' }
    }
    if ($stdoutRaw -match $selectionAnomalyPattern -or $stderrRaw -match $selectionAnomalyPattern) {
        return [pscustomobject]@{ Kind = 'SelectionFailure'; Detail = 'retry or skip output' }
    }

    $packageMarkers = @(
        @($stdout.Lines | Where-Object { $_ -cmatch 'PACKAGE2_' }) +
        @($stderr.Lines | Where-Object { $_ -cmatch 'PACKAGE2_' })
    )
    if ($packageMarkers.Count -ne 0) {
        return [pscustomobject]@{ Kind = 'TranscriptFailure'; Detail = 'controlled-exit emitted a package marker' }
    }
    $stdoutMarkers = @($stdout.Lines | Where-Object { $_ -cmatch '^W5_A1_EXIT_PROBE=' })
    $stderrMarkers = @($stderr.Lines | Where-Object { $_ -cmatch '^W5_A1_EXIT_PROBE=' })
    if ($stdoutMarkers.Count -ne 0) {
        return [pscustomobject]@{ Kind = 'WrongStreamFailure'; Detail = 'controlled marker appeared on stdout' }
    }
    if ($stderrMarkers.Count -ne 1 -or $stderrMarkers[0] -cne 'W5_A1_EXIT_PROBE=controlled_assertion') {
        return [pscustomobject]@{ Kind = 'TranscriptFailure'; Detail = 'controlled marker mismatch' }
    }
    $stdoutSummaries = @($stdout.Lines | Where-Object { $_ -cmatch '^Summary for all V _test\.v files:.*$' })
    $stderrSummaries = @($stderr.Lines | Where-Object { $_ -cmatch '^Summary for all V _test\.v files:.*$' })
    if ($stderrSummaries.Count -ne 0) {
        return [pscustomobject]@{ Kind = 'SummaryStreamFailure'; Detail = 'controlled summary appeared on stderr' }
    }
    $exactSummary = $stdoutSummaries.Count -eq 1 `
        -and $stdoutSummaries[0] -cmatch '^Summary for all V _test\.v files: 1 failed, 1 total\.(?: .*)?$'
    if (-not $exactSummary) {
        return [pscustomobject]@{ Kind = 'SummaryFailure'; Detail = 'controlled summary mismatch' }
    }
    if ($Result.ExitCode -ne 1) {
        return [pscustomobject]@{ Kind = 'ExitFailure'; Detail = "expected=1 actual=$($Result.ExitCode)" }
    }
    return [pscustomobject]@{ Kind = 'ControlledFailure'; Detail = 'exit=1' }
}

function New-W5A1SyntheticResult {
    param(
        [string[]]$StdoutLines = @(),
        [string[]]$StderrLines = @(),
        [AllowNull()]
        [object]$ExitCode = 0,
        [bool]$TimedOut = $false,
        [AllowEmptyString()]
        [string]$InfrastructureError = ''
    )

    $stdout = if ($StdoutLines.Count -eq 0) { '' } else { ($StdoutLines -join "`r`n") + "`r`n" }
    $stderr = if ($StderrLines.Count -eq 0) { '' } else { ($StderrLines -join "`r`n") + "`r`n" }
    return [pscustomobject]@{
        ExitCode = $ExitCode
        TimedOut = $TimedOut
        InfrastructureError = $InfrastructureError
        Stdout = $stdout
        Stderr = $stderr
        Output = @($StdoutLines + $StderrLines)
    }
}

function Test-W5A1Classifier {
    $redSummary = 'Summary for all V _test.v files: 1 failed, 1 total. Elapsed time: 1 ms.'
    $greenSummary = 'Summary for all V _test.v files: 1 passed, 1 total. Elapsed time: 1 ms.'
    $redMissingIdentity = @($redMarkers[1..($redMarkers.Count - 1)])
    $redReordered = @($redMarkers[1], $redMarkers[0]) + @($redMarkers[2..($redMarkers.Count - 1)])
    $redWrongGap = @($redMarkers[0..3] + 'PACKAGE2_W5_A1_PRODUCT_GAP=mouse_lock_capability_contract' + $redMarkers[5..7])
    $greenWrongProduct = @($greenMarkers[0..3] + 'PACKAGE2_W5_A1_PRODUCT_OK=synthetic' + $greenMarkers[5..7])
    $synthetic = @(
        @{ Name = 'valid red'; State = 'Red'; Expected = 'BehavioralRed'; Result = New-W5A1SyntheticResult -StdoutLines @($redSummary) -StderrLines $redMarkers -ExitCode 1 }
        @{ Name = 'valid green'; State = 'Green'; Expected = 'BehavioralGreen'; Result = New-W5A1SyntheticResult -StdoutLines @($greenMarkers + $greenSummary) }
        @{ Name = 'missing identity'; State = 'Red'; Expected = 'TranscriptFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($redSummary) -StderrLines $redMissingIdentity -ExitCode 1 }
        @{ Name = 'duplicate identity'; State = 'Red'; Expected = 'TranscriptFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($redSummary) -StderrLines (@($redMarkers[0]) + $redMarkers) -ExitCode 1 }
        @{ Name = 'reordered markers'; State = 'Red'; Expected = 'TranscriptFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($redSummary) -StderrLines $redReordered -ExitCode 1 }
        @{ Name = 'extra marker'; State = 'Red'; Expected = 'TranscriptFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($redSummary) -StderrLines @($redMarkers + 'PACKAGE2_W5_A1_EXTRA=synthetic') -ExitCode 1 }
        @{ Name = 'whitespace spoof'; State = 'Red'; Expected = 'TranscriptFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($redSummary) -StderrLines (@((' ' + $redMarkers[0])) + $redMarkers[1..7]) -ExitCode 1 }
        @{ Name = 'red marker on stdout'; State = 'Red'; Expected = 'WrongStreamFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($redMarkers[0], $redSummary) -StderrLines $redMarkers -ExitCode 1 }
        @{ Name = 'all red markers on stdout'; State = 'Red'; Expected = 'WrongStreamFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($redMarkers + $redSummary) -ExitCode 1 }
        @{ Name = 'red summary on stderr'; State = 'Red'; Expected = 'SummaryStreamFailure'; Result = New-W5A1SyntheticResult -StderrLines @($redMarkers + $redSummary) -ExitCode 1 }
        @{ Name = 'green markers on stderr'; State = 'Green'; Expected = 'WrongStreamFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($greenSummary) -StderrLines $greenMarkers }
        @{ Name = 'green summary on stderr'; State = 'Green'; Expected = 'SummaryStreamFailure'; Result = New-W5A1SyntheticResult -StdoutLines $greenMarkers -StderrLines @($greenSummary) }
        @{ Name = 'infra priority'; State = 'Red'; Expected = 'InfrastructureFailure'; Result = New-W5A1SyntheticResult -StderrLines @('PACKAGE2_W5_A1_INFRA=synthetic') -ExitCode 0 }
        @{ Name = 'wrong product gap'; State = 'Red'; Expected = 'TranscriptFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($redSummary) -StderrLines $redWrongGap -ExitCode 1 }
        @{ Name = 'wrong cleanup'; State = 'Red'; Expected = 'TranscriptFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($redSummary) -StderrLines @($redMarkers[0..4] + 'PACKAGE2_W5_A1_CLEANUP_OK=failure_rescue_verified' + $redMarkers[6..7]) -ExitCode 1 }
        @{ Name = 'wrong terminal'; State = 'Red'; Expected = 'TranscriptFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($redSummary) -StderrLines @($redMarkers[0..6] + 'PACKAGE2_W5_A1_TERMINAL=infra:mouse_lock_raw_delta_public') -ExitCode 1 }
        @{ Name = 'wrong red summary'; State = 'Red'; Expected = 'SummaryFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($greenSummary) -StderrLines $redMarkers -ExitCode 1 }
        @{ Name = 'duplicate summary'; State = 'Red'; Expected = 'SummaryFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($redSummary, $redSummary) -StderrLines $redMarkers -ExitCode 1 }
        @{ Name = 'red wrong exit'; State = 'Red'; Expected = 'ExitFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($redSummary) -StderrLines $redMarkers }
        @{ Name = 'green wrong product'; State = 'Green'; Expected = 'TranscriptFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($greenWrongProduct + $greenSummary) }
        @{ Name = 'green retained red terminal'; State = 'Green'; Expected = 'TranscriptFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($greenMarkers[0..6] + $redMarkers[7] + $greenSummary) }
        @{ Name = 'green wrong exit'; State = 'Green'; Expected = 'ExitFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($greenMarkers + $greenSummary) -ExitCode 1 }
        @{ Name = 'retry'; State = 'Red'; Expected = 'SelectionFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($redSummary, 'retrying synthetic') -StderrLines $redMarkers -ExitCode 1 }
        @{ Name = 'skip'; State = 'Red'; Expected = 'SelectionFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($redSummary, 'SKIPPED synthetic') -StderrLines $redMarkers -ExitCode 1 }
        @{ Name = 'compiler diagnostic'; State = 'Red'; Expected = 'InfrastructureFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($redSummary, 'C compilation error') -StderrLines $redMarkers -ExitCode 1 }
        @{ Name = 'timeout'; State = 'Red'; Expected = 'TimeoutFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($redSummary) -StderrLines $redMarkers -ExitCode 1 -TimedOut $true }
        @{ Name = 'crash'; State = 'Red'; Expected = 'FatalFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($redSummary) -StderrLines $redMarkers -ExitCode (-1073741819) }
        @{ Name = 'unknown exit'; State = 'Red'; Expected = 'UnknownExit'; Result = New-W5A1SyntheticResult -StdoutLines @($redSummary) -StderrLines $redMarkers -ExitCode $null }
        @{ Name = 'supervisor'; State = 'Red'; Expected = 'InfrastructureFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($redSummary) -StderrLines $redMarkers -ExitCode 1 -InfrastructureError 'synthetic supervisor failure' }
        @{ Name = 'drain'; State = 'Green'; Expected = 'InfrastructureFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($greenMarkers + $greenSummary) -InfrastructureError 'synthetic drain failure' }
    )

    if ($synthetic.Count -ne 30) {
        throw "W5 A1 classifier fixture count changed: $($synthetic.Count)"
    }
    $accepted = 0
    foreach ($item in $synthetic) {
        $classification = Get-W5A1Classification -Result $item.Result `
            -ExpectedState $item.State -ExpectedFailureExitCode 1
        if ($classification.Kind -cne $item.Expected) {
            throw "W5 A1 classifier self-test '$($item.Name)' expected $($item.Expected), got $($classification.Kind)"
        }
        if ($classification.Kind -in @('BehavioralRed', 'BehavioralGreen')) {
            $accepted++
        }
    }
    if ($accepted -ne 2) {
        throw "W5 A1 classifier self-test accepted $accepted cases instead of two"
    }
    Write-Host "PACKAGE2_W5_A1_CLASSIFIER_SELF_TEST accepted=$accepted rejected=$($synthetic.Count - $accepted) total=$($synthetic.Count)"
}

function Test-W5A1NoFlagClassifier {
    $summary = 'Summary for all V _test.v files: 1 passed, 1 total. Elapsed time: 1 ms.'
    $synthetic = @(
        @{ Name = 'valid'; Expected = 'NoFlagPass'; Result = New-W5A1SyntheticResult -StdoutLines @('V test stats', $summary) }
        @{ Name = 'stdout marker leak'; Expected = 'MarkerLeakFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($redMarkers[0], $summary) }
        @{ Name = 'stderr marker leak'; Expected = 'MarkerLeakFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($summary) -StderrLines @($redMarkers[0]) }
        @{ Name = 'summary on stderr'; Expected = 'SummaryStreamFailure'; Result = New-W5A1SyntheticResult -StderrLines @($summary) }
        @{ Name = 'failed summary'; Expected = 'SummaryFailure'; Result = New-W5A1SyntheticResult -StdoutLines @('Summary for all V _test.v files: 1 failed, 1 total.') -ExitCode 1 }
        @{ Name = 'nonzero'; Expected = 'ExitFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($summary) -ExitCode 1 }
        @{ Name = 'retry'; Expected = 'SelectionFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($summary, 'retrying synthetic') }
        @{ Name = 'diagnostic'; Expected = 'InfrastructureFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($summary, 'warning: synthetic') }
        @{ Name = 'timeout'; Expected = 'TimeoutFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($summary) -TimedOut $true }
        @{ Name = 'unknown exit'; Expected = 'UnknownExit'; Result = New-W5A1SyntheticResult -StdoutLines @($summary) -ExitCode $null }
        @{ Name = 'supervisor'; Expected = 'InfrastructureFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($summary) -InfrastructureError 'synthetic failed reap' }
    )
    $accepted = 0
    foreach ($item in $synthetic) {
        $classification = Get-W5A1NoFlagClassification -Result $item.Result
        if ($classification.Kind -cne $item.Expected) {
            throw "W5 A1 no-flag classifier self-test '$($item.Name)' expected $($item.Expected), got $($classification.Kind)"
        }
        if ($classification.Kind -ceq 'NoFlagPass') {
            $accepted++
        }
    }
    if ($accepted -ne 1) {
        throw "W5 A1 no-flag classifier accepted $accepted cases instead of one"
    }
    Write-Host "PACKAGE2_W5_A1_NOFLAG_CLASSIFIER_SELF_TEST accepted=$accepted rejected=$($synthetic.Count - $accepted) total=$($synthetic.Count)"
}

function Test-W5A1ControlledExitClassifier {
    $summary = 'Summary for all V _test.v files: 1 failed, 1 total. Elapsed time: 1 ms.'
    $marker = 'W5_A1_EXIT_PROBE=controlled_assertion'
    $synthetic = @(
        @{ Name = 'valid'; Expected = 'ControlledFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($summary) -StderrLines @($marker) -ExitCode 1 }
        @{ Name = 'marker on stdout'; Expected = 'WrongStreamFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($marker, $summary) -ExitCode 1 }
        @{ Name = 'summary on stderr'; Expected = 'SummaryStreamFailure'; Result = New-W5A1SyntheticResult -StderrLines @($marker, $summary) -ExitCode 1 }
        @{ Name = 'wrong exit'; Expected = 'ExitFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($summary) -StderrLines @($marker) }
        @{ Name = 'unknown exit'; Expected = 'UnknownExit'; Result = New-W5A1SyntheticResult -StdoutLines @($summary) -StderrLines @($marker) -ExitCode $null }
        @{ Name = 'package spoof'; Expected = 'TranscriptFailure'; Result = New-W5A1SyntheticResult -StdoutLines @($summary) -StderrLines @($marker, $redMarkers[0]) -ExitCode 1 }
        @{ Name = 'infra priority'; Expected = 'InfrastructureFailure'; Result = New-W5A1SyntheticResult -StderrLines @('PACKAGE2_W5_A1_INFRA=synthetic') -InfrastructureError 'synthetic infra' }
    )
    $accepted = 0
    foreach ($item in $synthetic) {
        $classification = Get-W5A1ControlledExitClassification -Result $item.Result
        if ($classification.Kind -cne $item.Expected) {
            throw "W5 A1 controlled-exit classifier self-test '$($item.Name)' expected $($item.Expected), got $($classification.Kind)"
        }
        if ($classification.Kind -ceq 'ControlledFailure') {
            $accepted++
        }
    }
    if ($accepted -ne 1) {
        throw "W5 A1 controlled-exit classifier accepted $accepted cases instead of one"
    }
    Write-Host "PACKAGE2_W5_A1_CONTROLLED_CLASSIFIER_SELF_TEST accepted=$accepted rejected=$($synthetic.Count - $accepted) total=$($synthetic.Count)"
}

function Test-W5A1FailedReapGate {
    $failure = 'child did not reap within 0 ms after synthetic tree kill'
    $hardAbort = $false
    $nextCaseStarted = $false
    try {
        Assert-W5A1FailedReap -Failure $failure
        $nextCaseStarted = $true
    } catch {
        $expected = "W5 A1 process supervision failed: $failure"
        if ($_.Exception.Message -cne $expected) {
            throw
        }
        $hardAbort = $true
    }
    if (-not $hardAbort -or $nextCaseStarted) {
        throw 'W5 A1 failed-reap self-test did not hard-abort before the next case'
    }
    Write-Host 'PACKAGE2_W5_A1_FAILED_REAP_SELF_TEST injected=true hard_abort=true next_case_started=false'
}

function Test-W5A1SourcePolicy {
    $testText = [System.IO.File]::ReadAllText((Resolve-Path -LiteralPath $testFile).Path)
    $oracleText = [System.IO.File]::ReadAllText((Resolve-Path -LiteralPath $oracle).Path)
    $testPattern = '(?i)(import\s+x\.multiwindow|with_native_window_for_gg|\.backend(?![A-Za-z0-9_])|service_set_mouse_lock|C\.v_multiwindow_win32_)'
    $oraclePattern = '(?<![A-Za-z0-9_])(?:RegisterRawInputDevices|GetRawInputData|DefRawInputProc|HRAWINPUT|WM_INPUT|PostThreadMessage(?:A|W)?|SendNotifyMessage(?:A|W)?|SendMessageCallback(?:A|W)?|SendMessage(?:A|W)?|PostMessage(?:A|W)?|SetWindowLong(?:Ptr)?(?:A|W)?|SetWindowSubclass|CallWindowProc(?:A|W)?|SetCapture|ReleaseCapture|ShowCursor|SetCursor|SetSystemCursor|DestroyCursor|SetClassLong(?:Ptr)?(?:A|W)?|mouse_event|RIDEV_(?:REMOVE|INPUTSINK|NOLEGACY|CAPTUREMOUSE|EXINPUTSINK|DEVNOTIFY))(?![A-Za-z0-9_])'
    if ([regex]::IsMatch($testText, $testPattern, [System.Text.RegularExpressions.RegexOptions]::CultureInvariant)) {
        throw 'W5 A1 public test contains an internal-routing token'
    }
    if ([regex]::IsMatch($oracleText, $oraclePattern, [System.Text.RegularExpressions.RegexOptions]::CultureInvariant)) {
        throw 'W5 A1 oracle contains a forbidden product-mutation or raw-decode token'
    }

    $testClean = @('import gg', 'app.with_native_window(window, callback)', 'app.set_window_mouse_lock(window, true)')
    $testForbiddenTokens = @('import x.multiwindow', 'with_native_window_for_gg', 'app.backend', 'service_set_mouse_lock', 'C.v_multiwindow_win32_raw')
    $testForbidden = [System.Collections.Generic.List[string]]::new()
    foreach ($token in $testForbiddenTokens) {
        [void]$testForbidden.Add($token)
        [void]$testForbidden.Add(('"{0}"' -f $token))
        [void]$testForbidden.Add(('/* {0} */' -f $token))
    }
    $oracleClean = @('SetCursorPos(1, 2);', 'GetRegisteredRawInputDevices(0, 0, 0);', 'RIDEV_PAGEONLY', 'SetWindowsHookExW(0, 0, 0, 0);')
    $oracleForbiddenTokens = @(
        'RegisterRawInputDevices', 'GetRawInputData', 'DefRawInputProc', 'HRAWINPUT',
        'WM_INPUT', 'PostThreadMessageW', 'SendNotifyMessageW', 'SendMessageCallbackW',
        'SendMessageW', 'PostMessageW', 'SetWindowLongW', 'SetWindowLongPtrW',
        'SetWindowSubclass', 'CallWindowProcW', 'SetCapture', 'ReleaseCapture',
        'ShowCursor', 'SetCursor', 'SetSystemCursor', 'DestroyCursor',
        'SetClassLongPtrW', 'mouse_event', 'RIDEV_REMOVE', 'RIDEV_INPUTSINK',
        'RIDEV_NOLEGACY', 'RIDEV_CAPTUREMOUSE', 'RIDEV_EXINPUTSINK', 'RIDEV_DEVNOTIFY'
    )
    $oracleForbidden = [System.Collections.Generic.List[string]]::new()
    foreach ($token in $oracleForbiddenTokens) {
        [void]$oracleForbidden.Add(('{0}(0);' -f $token))
        [void]$oracleForbidden.Add(('"{0}"' -f $token))
        [void]$oracleForbidden.Add(('/* {0} */' -f $token))
    }
    foreach ($fixture in $testClean) {
        if ([regex]::IsMatch($fixture, $testPattern, [System.Text.RegularExpressions.RegexOptions]::CultureInvariant)) {
            throw "W5 A1 public source-token self-test rejected clean fixture: $fixture"
        }
    }
    foreach ($fixture in $testForbidden) {
        if (-not [regex]::IsMatch($fixture, $testPattern, [System.Text.RegularExpressions.RegexOptions]::CultureInvariant)) {
            throw "W5 A1 public source-token self-test accepted forbidden fixture: $fixture"
        }
    }
    foreach ($fixture in $oracleClean) {
        if ([regex]::IsMatch($fixture, $oraclePattern, [System.Text.RegularExpressions.RegexOptions]::CultureInvariant)) {
            throw "W5 A1 oracle source-token self-test rejected clean fixture: $fixture"
        }
    }
    foreach ($fixture in $oracleForbidden) {
        if (-not [regex]::IsMatch($fixture, $oraclePattern, [System.Text.RegularExpressions.RegexOptions]::CultureInvariant)) {
            throw "W5 A1 oracle source-token self-test accepted forbidden fixture: $fixture"
        }
    }
    Write-Host "PACKAGE2_W5_A1_SOURCE_POLICY_OK test_clean=$($testClean.Count) test_rejected=$($testForbidden.Count) oracle_clean=$($oracleClean.Count) oracle_rejected=$($oracleForbidden.Count)"
}

function Get-W5A1SurfaceSha256 {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Schema,
        [Parameter(Mandatory = $true)]
        [string]$StateRecord,
        [Parameter(Mandatory = $true)]
        [object[]]$Entries
    )

    $records = [System.Collections.Generic.List[string]]::new()
    [void]$records.Add("schema=$Schema")
    [void]$records.Add($StateRecord)
    foreach ($entry in $Entries) {
        [void]$records.Add("file=$($entry.Path)|sha256=$($entry.Known)")
    }
    return Get-W5A1TextSha256 -Text (($records -join "`n") + "`n")
}

function Assert-W5A1Surface {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Name,
        [Parameter(Mandatory = $true)]
        [string]$Schema,
        [Parameter(Mandatory = $true)]
        [string]$StateRecord,
        [Parameter(Mandatory = $true)]
        [object[]]$Entries,
        [Parameter(Mandatory = $true)]
        [string]$KnownComposite,
        [Parameter(Mandatory = $true)]
        [string]$PassedComposite
    )

    foreach ($entry in $Entries) {
        if ($entry.Passed.ToLowerInvariant() -cne $entry.Known) {
            throw "W5 A1 $Name parameter mismatch for $($entry.Path)"
        }
        Assert-W5A1FileHash -Path $entry.Path -Expected $entry.Passed
    }
    $computed = Get-W5A1SurfaceSha256 -Schema $Schema -StateRecord $StateRecord -Entries $Entries
    if ($computed -cne $KnownComposite) {
        throw "W5 A1 $Name internal composite mismatch: expected=$KnownComposite actual=$computed"
    }
    if ($PassedComposite.ToLowerInvariant() -cne $KnownComposite) {
        throw "W5 A1 $Name parameter composite mismatch: expected=$KnownComposite passed=$PassedComposite"
    }
    Write-Host "PACKAGE2_W5_A1_SURFACE_OK name=$Name files=$($Entries.Count) sha256=$computed"
}

function Get-W5A1CompilerIdentity {
    param(
        [Parameter(Mandatory = $true)]
        [string]$SelectedCompiler,
        [Parameter(Mandatory = $true)]
        [string]$WorkingDirectory,
        [Parameter(Mandatory = $true)]
        [string]$TempDirectory
    )

    $compilerPath = ''
    $vCompiler = ''
    $versionArguments = @()
    if ($SelectedCompiler -ceq 'tcc') {
        if (-not $env:PINNED_TCC) {
            throw 'PINNED_TCC is not set for W5 A1 TCC validation'
        }
        $compilerPath = (Resolve-Path -LiteralPath $env:PINNED_TCC -ErrorAction Stop).Path
        $resolvedTcc = (Get-Command tcc.exe -CommandType Application -ErrorAction Stop).Source
        if ([IO.Path]::GetFullPath($resolvedTcc) -cne [IO.Path]::GetFullPath($compilerPath)) {
            throw "tcc.exe resolved to '$resolvedTcc', expected PINNED_TCC '$compilerPath'"
        }
        $vCompiler = [IO.Path]::GetFullPath($compilerPath)
        $versionArguments = @('-v')
    } elseif ($SelectedCompiler -ceq 'gcc') {
        $selectedGccCommands = @(
            @(Get-Command x86_64-w64-mingw32-gcc.exe -CommandType Application -All -ErrorAction SilentlyContinue) |
                Select-Object -First 1
        )
        if ($selectedGccCommands.Count -ne 1) {
            throw 'W5 A1 target-prefixed GCC was not found on PATH'
        }
        $gccSource = [string]$selectedGccCommands[0].Source
        if ([string]::IsNullOrWhiteSpace($gccSource)) {
            throw 'W5 A1 target-prefixed GCC resolved to an empty source'
        }
        $compilerPath = [IO.Path]::GetFullPath((Resolve-Path -LiteralPath $gccSource -ErrorAction Stop).Path)
        if (-not (Test-Path -LiteralPath $compilerPath -PathType Leaf)) {
            throw "W5 A1 selected GCC is not a file: '$compilerPath'"
        }
        $vCompiler = $compilerPath
        $machine = Invoke-W5A1BoundedProcess -FileName $compilerPath -Arguments @('-dumpmachine') `
            -WorkingDirectory $WorkingDirectory -TempDirectory $TempDirectory -TimeoutSeconds 30
        Write-W5A1ProcessOutput -Result $machine
        Assert-W5A1CommandGreen -Label 'W5 A1 GCC target identity' -Result $machine
        $target = ([string]$machine.Stdout).Trim()
        if ($target -cnotmatch '^x86_64(?:-w64)?-mingw32$') {
            throw "W5 A1 GCC target is not x64 MinGW: '$target'"
        }
        $versionArguments = @('--version')
    } else {
        $compilerPath = (Get-Command cl.exe -CommandType Application -ErrorAction Stop).Source
        $vCompiler = 'msvc'
        $versionArguments = @('/?')
        if ($env:VSCMD_ARG_TGT_ARCH -cne 'x64') {
            throw "W5 A1 MSVC target architecture is not x64: '$env:VSCMD_ARG_TGT_ARCH'"
        }
    }

    $versionResult = Invoke-W5A1BoundedProcess -FileName $compilerPath `
        -Arguments $versionArguments -WorkingDirectory $WorkingDirectory `
        -TempDirectory $TempDirectory -TimeoutSeconds 30
    Write-W5A1ProcessOutput -Result $versionResult
    Assert-W5A1CommandGreen -Label "W5 A1 $SelectedCompiler version" -Result $versionResult
    $versionLines = @(
        (([string]$versionResult.Stdout) + "`n" + ([string]$versionResult.Stderr)) -split "\r?\n" |
            ForEach-Object { $_.Trim() } | Where-Object { $_ -ne '' }
    )
    if ($versionLines.Count -eq 0) {
        throw "W5 A1 $SelectedCompiler version output was empty"
    }
    if ($SelectedCompiler -ceq 'msvc') {
        $msvcVersionLines = @(
            $versionLines | Where-Object {
                $_ -cmatch '^Microsoft \(R\) C/C\+\+ Optimizing Compiler Version [0-9]+(?:\.[0-9]+){2,3} for x64$'
            }
        )
        if ($msvcVersionLines.Count -ne 1) {
            throw "W5 A1 expected exactly one MSVC x64 banner, found $($msvcVersionLines.Count)"
        }
        $version = [string]$msvcVersionLines[0]
    } else {
        $version = [string]$versionLines[0]
    }
    $sha256 = (Get-FileHash -LiteralPath $compilerPath -Algorithm SHA256).Hash.ToLowerInvariant()
    $encodedPath = [Uri]::EscapeDataString([IO.Path]::GetFullPath($compilerPath))
    $encodedVersion = [Uri]::EscapeDataString($version)
    Write-Host "PACKAGE2_W5_A1_COMPILER_IDENTITY compiler=$SelectedCompiler resolved=$encodedPath sha256=$sha256 version=$encodedVersion"
    return [pscustomobject]@{
        Path = $compilerPath
        VCompiler = $vCompiler
        Version = $version
        Sha256 = $sha256
    }
}

function Assert-W5A1CanonicalSelection {
    param(
        [Parameter(Mandatory = $true)]
        [pscustomobject]$Result,
        [Parameter(Mandatory = $true)]
        [string]$ExpectedTest,
        [Parameter(Mandatory = $true)]
        [string]$ExpectedDirectory
    )

    Assert-W5A1CommandGreen -Label 'W5 A1 absolute test selection' -Result $Result -RejectDiagnostics
    $selected = [System.Collections.Generic.HashSet[string]]::new([System.StringComparer]::OrdinalIgnoreCase)
    foreach ($line in @($Result.Output)) {
        $candidate = ([string]$line).Trim()
        if ($candidate.EndsWith(':parse_text', [System.StringComparison]::Ordinal)) {
            $candidate = $candidate.Substring(0, $candidate.Length - ':parse_text'.Length)
        }
        if (-not [IO.Path]::IsPathRooted($candidate) -or -not (Test-Path -LiteralPath $candidate -PathType Leaf)) {
            continue
        }
        $resolved = [IO.Path]::GetFullPath((Resolve-Path -LiteralPath $candidate).Path)
        $parent = [IO.Path]::GetDirectoryName($resolved)
        if ($parent -ceq $ExpectedDirectory -or $parent.Equals($ExpectedDirectory, [System.StringComparison]::OrdinalIgnoreCase)) {
            [void]$selected.Add($resolved)
        }
    }
    $selectedArray = @($selected)
    if ($selectedArray.Count -ne 1 -or -not $selectedArray[0].Equals($ExpectedTest, [System.StringComparison]::OrdinalIgnoreCase)) {
        throw "W5 A1 absolute selection mismatch: expected only '$ExpectedTest', found '$($selectedArray -join ';')'"
    }
    Write-Host "PACKAGE2_W5_A1_SELECTION_OK target=$ExpectedTest exclude=$ExpectedDirectory/*.v direct_children=1"
}

function Get-W5A1ExpectedFailureExitCode {
    param(
        [Parameter(Mandatory = $true)]
        [string]$VExe,
        [Parameter(Mandatory = $true)]
        [string]$VCompiler,
        [Parameter(Mandatory = $true)]
        [string]$RepositoryRoot,
        [Parameter(Mandatory = $true)]
        [string]$TempDirectory
    )

    $sourcePath = Join-Path $TempDirectory 'w5_a1_controlled_failure_test.v'
    $source = @'
// vtest retry: 0
module main

fn test_w5_a1_controlled_failure_exit() {
	eprintln('W5_A1_EXIT_PROBE=controlled_assertion')
	assert false, 'W5_A1_EXIT_PROBE_ASSERTION=controlled_failure'
}
'@
    [System.IO.File]::WriteAllText($sourcePath, $source, [System.Text.UTF8Encoding]::new($false))
    $result = Invoke-W5A1BoundedProcess -FileName $VExe -Arguments @(
        '-stats', '-cc', $VCompiler, '-nocache', '-no-retry-compilation', '-no-parallel',
        '-gc', 'none', '-subsystem', 'console', '-run-only',
        'test_w5_a1_controlled_failure_exit', 'test', $sourcePath
    ) -WorkingDirectory $RepositoryRoot -TempDirectory $TempDirectory
    Write-W5A1ProcessOutput -Result $result
    $classification = Get-W5A1ControlledExitClassification -Result $result
    if ($classification.Kind -cne 'ControlledFailure') {
        throw "W5 A1 controlled failure-exit probe rejected ${Compiler}: $($classification.Kind): $($classification.Detail)"
    }
    Write-Host "PACKAGE2_W5_A1_EXPECTED_FAILURE_EXIT compiler=$Compiler exit=1"
    return 1
}

if ($ExpectedTestSha256.ToLowerInvariant() -cne $knownTestSha256) {
    throw "W5 A1 test parameter mismatch: expected=$knownTestSha256 passed=$ExpectedTestSha256"
}
if ($ExpectedOracleSha256.ToLowerInvariant() -cne $knownOracleSha256) {
    throw "W5 A1 oracle parameter mismatch: expected=$knownOracleSha256 passed=$ExpectedOracleSha256"
}
if ($ExpectedTupleSha256.ToLowerInvariant() -cne $knownTupleSha256) {
    throw "W5 A1 tuple parameter mismatch: expected=$knownTupleSha256 passed=$ExpectedTupleSha256"
}
if ($ExpectedA0HeaderSha256.ToLowerInvariant() -cne $knownA0HeaderSha256 `
    -or $ExpectedA0MainSha256.ToLowerInvariant() -cne $knownA0MainSha256 `
    -or $ExpectedA0TupleSha256.ToLowerInvariant() -cne $knownA0TupleSha256 `
    -or $ExpectedA0RunnerSha256.ToLowerInvariant() -cne $knownA0RunnerSha256) {
    throw 'W5 A1 A0 authority parameter mismatch'
}

Assert-W5A1FileHash -Path $testFile -Expected $ExpectedTestSha256
Assert-W5A1FileHash -Path $oracle -Expected $ExpectedOracleSha256
Assert-W5A1FileHash -Path $a0Header -Expected $ExpectedA0HeaderSha256
Assert-W5A1FileHash -Path $a0Main -Expected $ExpectedA0MainSha256
Assert-W5A1FileHash -Path $a0Runner -Expected $ExpectedA0RunnerSha256

$a0TupleRecords = @(
    'schema=package2-win32-w5-a0-v1'
    "file=$a0Header|sha256=$knownA0HeaderSha256"
    "file=$a0Main|sha256=$knownA0MainSha256"
    'case=win32_raw_input_sendinput_preflight|family=raw_input_environment'
)
$a0TupleRecords += @($a0Markers | ForEach-Object { "marker=$_" })
$a0TupleSha256 = Get-W5A1TextSha256 -Text (($a0TupleRecords -join "`n") + "`n")
if ($a0TupleSha256 -cne $knownA0TupleSha256) {
    throw "W5 A1 A0 authority tuple mismatch: expected=$knownA0TupleSha256 actual=$a0TupleSha256"
}
Write-Host "PACKAGE2_W5_A1_A0_AUTHORITY_OK tuple=$a0TupleSha256 runner=$knownA0RunnerSha256"

Assert-W5A1Surface -Name 'public-routing-frozen' `
    -Schema 'package2-win32-w5-a1-public-routing-surface-v1' -StateRecord 'state=frozen' `
    -Entries $publicSurface -KnownComposite $knownPublicRoutingSurfaceSha256 `
    -PassedComposite $ExpectedPublicRoutingSurfaceSha256
Assert-W5A1Surface -Name 'production-green' `
    -Schema 'package2-win32-w5-a1-production-surface-v1' -StateRecord 'expectation=Green' `
    -Entries $greenProductionSurface -KnownComposite $knownGreenProductionSurfaceSha256 `
    -PassedComposite $ExpectedGreenProductionSurfaceSha256

Test-W5A1SourcePolicy
Assert-W5A1FileHash -Path $PSCommandPath -Expected $ExpectedRunnerSha256

$tupleRecords = @(
    'schema=package2-win32-w5-a1-v1'
    "selection=target|kind=absolute-explicit|path=$testFile"
    'selection=exclude|kind=absolute-testdata-wildcard|pattern=vlib/gg/testdata/*.v'
    "selection=direct-child|path=$testFile"
    "file=$testFile|sha256=$knownTestSha256"
    "file=$oracle|sha256=$knownOracleSha256"
    "case=noflag|define=disabled|run_only=$caseName|expectation=BehavioralGreen|summary=1_passed_1_total|package_markers=0"
    "case=enabled|define=gg_multiwindow|run_only=$caseName|expectation=BehavioralGreen|summary=1_passed_1_total"
)
$tupleRecords += @($redMarkers | ForEach-Object { "marker=red|$_" })
$tupleRecords += @($greenMarkers | ForEach-Object { "marker=green|$_" })
$tupleSha256 = Get-W5A1TextSha256 -Text (($tupleRecords -join "`n") + "`n")
if ($tupleSha256 -cne $knownTupleSha256) {
    throw "W5 A1 ordered tuple mismatch: expected=$knownTupleSha256 actual=$tupleSha256"
}
Write-Host "PACKAGE2_W5_A1_TUPLE_OK sha256=$tupleSha256 red_markers=$($redMarkers.Count) green_markers=$($greenMarkers.Count)"

Test-W5A1Classifier
Test-W5A1NoFlagClassifier
Test-W5A1ControlledExitClassifier
Test-W5A1FailedReapGate

$repositoryRoot = (Get-Location).Path
$vexe = (Resolve-Path -LiteralPath '.\v.exe' -ErrorAction Stop).Path
$absoluteTest = [IO.Path]::GetFullPath((Resolve-Path -LiteralPath $testFile).Path)
$absoluteTestDirectory = [IO.Path]::GetDirectoryName($absoluteTest)
$absoluteExclude = [IO.Path]::Combine($absoluteTestDirectory, '*.v')
$tempRoot = if ($env:RUNNER_TEMP) { $env:RUNNER_TEMP } else { [IO.Path]::GetTempPath() }
$tempDir = Join-Path $tempRoot "multiwindow_w5_a1_$([guid]::NewGuid().ToString('N'))"
[void](New-Item -ItemType Directory -Path $tempDir)
[void](New-Item -ItemType Directory -Path (Join-Path $tempDir 'vcache'))

try {
    $compilerIdentity = Get-W5A1CompilerIdentity -SelectedCompiler $Compiler `
        -WorkingDirectory $repositoryRoot -TempDirectory $tempDir
    $baseArguments = @(
        '-cc', $compilerIdentity.VCompiler,
        '-nocache', '-no-retry-compilation', '-no-parallel',
        '-gc', 'none', '-subsystem', 'console',
        '-run-only', $caseName, '-exclude', $absoluteExclude
    )

    Write-Host "::group::Win32 W5 A1 canonical selection $Compiler"
    try {
        $selection = Invoke-W5A1BoundedProcess -FileName $vexe `
            -Arguments @($baseArguments + @('-d', 'gg_multiwindow', '-print-v-files', $absoluteTest)) `
            -WorkingDirectory $repositoryRoot -TempDirectory $tempDir
        Write-W5A1ProcessOutput -Result $selection
        Assert-W5A1CanonicalSelection -Result $selection -ExpectedTest $absoluteTest `
            -ExpectedDirectory $absoluteTestDirectory
    } finally {
        Write-Host '::endgroup::'
    }

    Write-Host "::group::Win32 W5 A1 no-flag isolation $Compiler"
    try {
        $noFlag = Invoke-W5A1BoundedProcess -FileName $vexe `
            -Arguments @($baseArguments + @('-stats', 'test', $absoluteTest)) `
            -WorkingDirectory $repositoryRoot -TempDirectory $tempDir
        Write-W5A1ProcessOutput -Result $noFlag
        $noFlagClassification = Get-W5A1NoFlagClassification -Result $noFlag
        if ($noFlagClassification.Kind -cne 'NoFlagPass') {
            throw "W5 A1 no-flag gate rejected ${Compiler}: $($noFlagClassification.Kind): $($noFlagClassification.Detail)"
        }
        Write-Host "PACKAGE2_W5_A1_NOFLAG_PASS compiler=$Compiler passed=1 total=1 markers=0"
    } finally {
        Write-Host '::endgroup::'
    }

    Write-Host "::group::Win32 W5 A1 controlled failure exit $Compiler"
    try {
        $expectedFailureExitCode = Get-W5A1ExpectedFailureExitCode -VExe $vexe `
            -VCompiler $compilerIdentity.VCompiler -RepositoryRoot $repositoryRoot `
            -TempDirectory $tempDir
    } finally {
        Write-Host '::endgroup::'
    }

    Write-Host "::group::Win32 W5 A1 $Expectation $Compiler $caseName"
    try {
        $enabled = Invoke-W5A1BoundedProcess -FileName $vexe `
            -Arguments @($baseArguments + @('-stats', '-d', 'gg_multiwindow', 'test', $absoluteTest)) `
            -WorkingDirectory $repositoryRoot -TempDirectory $tempDir
        Write-W5A1ProcessOutput -Result $enabled
        $classification = Get-W5A1Classification -Result $enabled -ExpectedState $Expectation `
            -ExpectedFailureExitCode $expectedFailureExitCode
        if ($classification.Kind -cne 'BehavioralGreen') {
            throw "W5 A1 $Expectation gate rejected ${Compiler}: $($classification.Kind): $($classification.Detail)"
        }
        Write-Host "PACKAGE2_W5_A1_CASE_ACCEPT compiler=$Compiler expectation=$Expectation case=$caseName family=$family classification=BehavioralGreen"
        Write-Host "PACKAGE2_W5_A1_RUNNER_SUMMARY compiler=$Compiler accepted=1 rejected=0 total=1"
    } finally {
        Write-Host '::endgroup::'
    }
} finally {
    Remove-Item -LiteralPath $tempDir -Recurse -Force -ErrorAction SilentlyContinue
}
