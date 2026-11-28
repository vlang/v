param(
    [string]$Compiler = '',
    [string]$Prebuilt = ''
)

$ErrorActionPreference = 'Stop'
$production = Join-Path (Get-Location) 'vprod.exe'
$savedVFlags = [Environment]::GetEnvironmentVariable('VFLAGS', 'Process')
$savedVExe = [Environment]::GetEnvironmentVariable('VEXE', 'Process')

try {
    # Optimize the compiler independently of the flags used to compile tests.
    Remove-Item Env:VFLAGS, Env:VEXE -ErrorAction SilentlyContinue
    if ($Prebuilt) {
        $candidate = (Resolve-Path -LiteralPath $Prebuilt).Path
        if ([Runtime.InteropServices.RuntimeInformation]::OSArchitecture.ToString() -cne 'X64') {
            throw 'The shared Windows compiler requires an x64 runner'
        }
        $commit = [string](& git rev-parse HEAD)
        if ($LASTEXITCODE -ne 0 -or $commit -cnotmatch '^[a-f0-9]{40}$') {
            throw 'Cannot determine the checked-out commit for the shared compiler'
        }
        if ([IO.Path]::GetFileName($candidate) -cne "${commit}-windows-x64") {
            throw 'The shared compiler does not match the checked-out commit and platform'
        }
        $checksum = Get-Content -LiteralPath "$candidate.sha256" -Raw
        if ($checksum -cnotmatch '^([a-f0-9]{64})[ \t]+\*?([^\r\n]+)\r?\n?$') {
            throw "Invalid compiler checksum file: $candidate.sha256"
        }
        $expectedHash = $Matches[1]
        if ($Matches[2] -cne [IO.Path]::GetFileName($candidate)) {
            throw "Compiler checksum names a different asset: $candidate.sha256"
        }
        if ((Get-FileHash -LiteralPath $candidate -Algorithm SHA256).Hash.ToLowerInvariant() -cne $expectedHash) {
            throw "Compiler checksum does not match: $candidate"
        }
        Copy-Item -LiteralPath $candidate -Destination $production -Force
    } else {
        if (-not (Test-Path -LiteralPath '.\v.exe' -PathType Leaf)) {
            throw 'Build V with makev.bat before preparing the optimized compiler'
        }
        Remove-Item -LiteralPath $production -Force -ErrorAction SilentlyContinue
        # v self selects MSVC, a MinGW Clang, or GCC for -prod. The LLVM Clang
        # shipped on Windows targets the MSVC ABI and is not a MinGW compiler.
        $buildArgs = @('self', '-prod', '-parallel-cc', '-o', $production)
        if ($Compiler) {
            $buildArgs += @('-cc', $Compiler)
        }
        & .\v.exe @buildArgs
        if ($LASTEXITCODE -ne 0) {
            throw "Building the optimized compiler failed (exit $LASTEXITCODE)"
        }
    }

    & $production version
    if ($LASTEXITCODE -ne 0) {
        throw "The optimized compiler failed its version check (exit $LASTEXITCODE)"
    }
    # A just-executed compiler or a scanner may briefly hold an executable.
    # Replace on the same volume without deleting the destination first.
    $destination = Join-Path (Get-Location) 'v.exe'
    for ($attempt = 1; ; $attempt++) {
        try {
            [IO.File]::Move($production, $destination, $true)
            break
        } catch {
            $cause = $_.Exception
            while ($cause.InnerException) { $cause = $cause.InnerException }
            $code = $cause.HResult -band 0xffff
            # ERROR_SHARING_VIOLATION / ERROR_LOCK_VIOLATION only.
            if ($cause -isnot [IO.IOException] -or $code -notin @(32, 33) -or $attempt -ge 20) {
                throw
            }
            Write-Warning "Compiler publication sharing violation (Win32 $code), retry $attempt/19"
            Start-Sleep -Milliseconds 250
        }
    }
} finally {
    [Environment]::SetEnvironmentVariable('VFLAGS', $savedVFlags, 'Process')
    [Environment]::SetEnvironmentVariable('VEXE', $savedVExe, 'Process')
}
