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
    # A scanner can briefly lock either executable after the version check.
    $destination = Join-Path (Get-Location) 'v.exe'
    for ($attempt = 0; ; $attempt++) {
        try {
            [IO.File]::Move($production, $destination, $true)
            break
        } catch {
            $cause = $_.Exception.GetBaseException()
            $win32Error = $cause.HResult -band 0xffff
            $sharingViolation = $cause -is [IO.IOException] -and $win32Error -in @(32, 33)
            # MoveFileEx can report ACCESS_DENIED for a destination held without
            # FILE_SHARE_DELETE. Only retry that error if a DELETE-access probe
            # independently observes an actual sharing or lock violation.
            if (-not $sharingViolation -and $cause -is [UnauthorizedAccessException] -and $win32Error -eq 5) {
                if (-not ('CompilerPublicationLockProbe' -as [type])) {
                    Add-Type -TypeDefinition @'
using System;
using System.Runtime.InteropServices;
using Microsoft.Win32.SafeHandles;
public static class CompilerPublicationLockProbe {
    [DllImport("kernel32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
    private static extern SafeFileHandle CreateFileW(string path, uint access,
        uint share, IntPtr security, uint disposition, uint flags, IntPtr template);
    public static int Probe(string path) {
        using (var handle = CreateFileW(path, 0x10000, 7, IntPtr.Zero, 3, 0x80, IntPtr.Zero)) {
            return handle.IsInvalid ? Marshal.GetLastWin32Error() : 0;
        }
    }
}
'@
                }
                $sourceProbe = [CompilerPublicationLockProbe]::Probe($production)
                $destinationProbe = [CompilerPublicationLockProbe]::Probe($destination)
                $sharingViolation = $sourceProbe -in @(32, 33) -or $destinationProbe -in @(32, 33)
                Write-Host "Compiler publication access denied: Win32 $win32Error, source probe $sourceProbe, destination probe $destinationProbe"
            }
            if ($attempt -ge 12 -or -not $sharingViolation) {
                throw
            }
            Write-Warning "Compiler publication sharing violation (Win32 $win32Error), retry $($attempt + 1)/12"
            Start-Sleep -Milliseconds 500
        }
    }
} finally {
    [Environment]::SetEnvironmentVariable('VFLAGS', $savedVFlags, 'Process')
    [Environment]::SetEnvironmentVariable('VEXE', $savedVExe, 'Process')
}
