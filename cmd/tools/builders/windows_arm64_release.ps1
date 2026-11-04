# Build a native Windows ARM64 compiler and release tools using the existing C backend.
# Run on Windows 11 ARM64 with Visual Studio's x64 and ARM64 C++ tools installed.
param()

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version Latest

function Invoke-Checked {
    param([string]$Program, [string[]]$Arguments)
    & $Program @Arguments
    if ($LASTEXITCODE -ne 0) {
        throw "$Program failed with exit code $LASTEXITCODE"
    }
}

function Assert-ExecutableMachine {
    param([string]$Path, [uint16]$Machine)
    $stream = [IO.File]::OpenRead($Path)
    $reader = [IO.BinaryReader]::new($stream)
    try {
        if ($reader.ReadUInt16() -ne 0x5a4d) {
            throw "$Path has no DOS executable header"
        }
        $stream.Position = 0x3c
        $stream.Position = $reader.ReadUInt32()
        if ($reader.ReadUInt32() -ne 0x00004550 -or $reader.ReadUInt16() -ne $Machine) {
            throw "$Path does not have the expected PE machine type $Machine"
        }
    } finally {
        $reader.Dispose()
    }
}

function Set-MsvcTarget {
    param([string]$InstallDirectory, [string]$Target, [string]$ScratchDirectory)
    $developerCommand = Join-Path $InstallDirectory 'Common7/Tools/VsDevCmd.bat'
    $environmentScript = Join-Path $ScratchDirectory "environment-$Target.cmd"
    @"
@call "$developerCommand" -arch=$Target -host_arch=arm64 -no_logo >nul
@if errorlevel 1 exit /b %errorlevel%
@set
"@ | Set-Content -LiteralPath $environmentScript -Encoding ascii
    $environment = & $env:ComSpec /d /c $environmentScript
    if ($LASTEXITCODE -ne 0) {
        throw "Could not initialize the MSVC $Target target environment."
    }
    foreach ($line in $environment) {
        if ($line -match '^([^=]+)=(.*)$') {
            [Environment]::SetEnvironmentVariable($matches[1], $matches[2], 'Process')
        }
    }
    if ($env:VSCMD_ARG_TGT_ARCH -ne $Target) {
        throw "The selected MSVC toolchain does not target $Target."
    }
    Write-Host "MSVC target $Target using $((Get-Command cl).Source)"
}

if (-not $IsWindows -or [Runtime.InteropServices.RuntimeInformation]::ProcessArchitecture -ne 'Arm64') {
    throw 'Run this script with native ARM64 PowerShell 7 on Windows ARM64.'
}

$root = (Resolve-Path (Join-Path $PSScriptRoot '../../..')).Path
$work = Join-Path ([IO.Path]::GetTempPath()) ('v-windows-arm64-' + [Guid]::NewGuid().ToString('N'))
$bootstrap = Join-Path $root 'v_arm64_bootstrap.exe'
$stage = Join-Path $root 'v_arm64_stage.exe'
$savedEnvironment = @{}
foreach ($name in @('VFLAGS', 'VEXE')) {
    $savedEnvironment[$name] = [Environment]::GetEnvironmentVariable($name, 'Process')
    [Environment]::SetEnvironmentVariable($name, $null, 'Process')
}

New-Item -ItemType Directory -Path $work | Out-Null
Push-Location $root
try {
    # An ARM64 VS installation can live in Program Files instead of Program Files (x86).
    $vswhere = @(${env:ProgramFiles(x86)}, $env:ProgramFiles) |
        Where-Object { $_ } |
        ForEach-Object { Join-Path $_ 'Microsoft Visual Studio/Installer/vswhere.exe' } |
        Where-Object { Test-Path -LiteralPath $_ } |
        Select-Object -First 1
    if (-not $vswhere) {
        throw 'Install Visual Studio with the C++ x64 and ARM64 build tools and a Windows SDK.'
    }
    $install = & $vswhere -latest -products '*' -requires Microsoft.VisualStudio.Component.VC.Tools.ARM64 Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath
    if ($LASTEXITCODE -ne 0 -or -not $install) {
        throw 'Visual Studio C++ x64 and ARM64 build tools were not found.'
    }

    if (-not (Test-Path 'vc/v_win.c')) {
        Invoke-Checked git @('clone', '--depth=1', 'https://github.com/vlang/vc', 'vc')
    }
    Invoke-Checked git @('-C', 'vc', 'rev-parse', 'HEAD')
    $snapshot = Join-Path $work 'v_win_compat.c'
    & ./cmd/tools/vc_compat_uint.ps1 -Source 'vc/v_win.c' -Destination $snapshot
    $bootstrapNative = @('vlib/time/ticks_windows.c', 'vlib/v/winthread_helper_windows.c', 'cmd/v/toolcache_windows_helpers.c')
    $stageNative = @('vlib/builtin/core_windows.c', 'vlib/v/modulecache/file_metadata.c') + $bootstrapNative + @('vlib/sync/thread_helper.c')
    $common = @('/nologo', '/volatile:ms', '/bigobj', '/MD', '/utf-8', '/w', '/std:c11', '/D_CRT_DECLARE_NONSTDC_NAMES=1', "/Fo$work\")
    $libraries = @('kernel32.lib', 'user32.lib', 'dbghelp.lib', 'ws2_32.lib', 'bcrypt.lib', 'advapi32.lib', '/link', '/STACK:33554432')

    # The published Windows snapshot contains x64 MSVC intrinsics. Run this
    # transient compiler under Windows 11's emulation, then explicitly target
    # ARM64 when generating the native stage. Only ARM64 executables are shipped.
    Set-MsvcTarget $install 'x64' $work
    Invoke-Checked cl ($common + @('-DCUSTOM_DEFINE_v1_fallback', "/Fe$bootstrap", $snapshot) + $bootstrapNative + $libraries)
    Assert-ExecutableMachine $bootstrap 0x8664
    Set-MsvcTarget $install 'arm64' $work
    $stageC = Join-Path $work 'v_stage.c'
    Invoke-Checked $bootstrap @('-no-parallel', '-gc', 'none', '-g', '-cc', 'msvc', '-os', 'windows', '-arch', 'arm64', '-o', $stageC, 'cmd/v')
    # This direct C link also needs the ARM64 atomic pause mapping normally
    # supplied by sync.stdatomic's target-specific #flag.
    Invoke-Checked cl ($common + @('/D_mm_pause=__yield', "/Fe$stage", $stageC) + $stageNative + $libraries)
    Assert-ExecutableMachine $stage 0xaa64
    Invoke-Checked $stage @('-no-parallel', '-prod', '-cc', 'msvc', '-arch', 'arm64', '-no-retry-compilation', '-o', 'v.exe', 'cmd/v')
    Assert-ExecutableMachine (Join-Path $root 'v.exe') 0xaa64

    # Verify the compiled target and a normal GC-enabled application before packaging.
    $smokeSource = Join-Path $work 'smoke.v'
    @'
fn main() {
    $if !windows || !arm64 {
        $compile_error('expected native Windows ARM64')
    }
    assert sizeof(voidptr) == 8
    mut values := map[string]int{}
    values['arm64'] = 64
    assert values['arm64'] == 64
    println('Windows ARM64 OK')
}
'@ | Set-Content -LiteralPath $smokeSource -Encoding utf8
    $smokeExecutable = Join-Path $work 'smoke.exe'
    Invoke-Checked ./v.exe @('-cc', 'msvc', '-no-retry-compilation', '-o', $smokeExecutable, $smokeSource)
    Assert-ExecutableMachine $smokeExecutable 0xaa64
    Invoke-Checked $smokeExecutable @()
    foreach ($tool in @('vup', 'vdoctor')) {
        $executable = "cmd/tools/$tool.exe"
        Invoke-Checked ./v.exe @('-prod', '-cc', 'msvc', '-no-retry-compilation', '-o', $executable, "cmd/tools/$tool.v")
        Assert-ExecutableMachine (Join-Path $root $executable) 0xaa64
    }
    Invoke-Checked ./v.exe @('version')
} finally {
    Pop-Location
    foreach ($name in $savedEnvironment.Keys) {
        [Environment]::SetEnvironmentVariable($name, $savedEnvironment[$name], 'Process')
    }
    Remove-Item -LiteralPath $bootstrap, $stage -Force -ErrorAction SilentlyContinue
    Remove-Item -LiteralPath $work -Recurse -Force -ErrorAction SilentlyContinue
}
