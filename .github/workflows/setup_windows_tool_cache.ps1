param([switch]$Verify, [string]$CacheParent = '')

$ErrorActionPreference = 'Stop'

if ($Verify) {
    $previousVerbose = $env:VTOOLS_CACHE_VERBOSE
    try {
        $env:VTOOLS_CACHE_VERBOSE = '1'
        # The second invocation must reuse the first one's binary. Otherwise
        # test-cleancode would rebuild vet for each of its thousands of inputs.
        foreach ($attempt in 1..2) {
            $output = (& .\v.exe vet examples/hello_world.v 2>&1 | Out-String)
            Write-Host $output
            if ($LASTEXITCODE -ne 0) {
                throw "tool cache probe failed with exit code $LASTEXITCODE"
            }
        }
        if (-not $output.Contains('reusing the cached `vvet`')) {
            throw 'the second vet invocation did not reuse its cached executable'
        }
    } finally {
        $env:VTOOLS_CACHE_VERBOSE = $previousVerbose
    }
    return
}

# Elevated Windows processes can create directories owned by Administrators.
# V deliberately requires a cache owned by the current user, with no other
# unprivileged writers. Set both the owner and an explicit inheritable DACL.
# MSVC also writes helper objects beneath the tool's staging directory. Keep
# the root short enough that those nested filenames stay below MAX_PATH.
if ($CacheParent -eq '') {
    $CacheParent = [IO.Path]::GetPathRoot($env:RUNNER_TEMP)
}
$cache = Join-Path $CacheParent ('v' + [guid]::NewGuid().ToString('N').Substring(0, 6))
$null = New-Item -ItemType Directory -Path $cache
$user = [Security.Principal.WindowsIdentity]::GetCurrent().User
$acl = [Security.AccessControl.DirectorySecurity]::new()
$acl.SetOwner($user)
$acl.SetAccessRuleProtection($true, $false)
foreach ($sid in @($user, [Security.Principal.SecurityIdentifier]::new('S-1-5-18'),
        [Security.Principal.SecurityIdentifier]::new('S-1-5-32-544'))) {
    $rule = [Security.AccessControl.FileSystemAccessRule]::new(
        $sid, 'FullControl', 'ContainerInherit, ObjectInherit', 'None', 'Allow')
    $acl.AddAccessRule($rule)
}
Set-Acl -LiteralPath $cache -AclObject $acl
Add-Content -LiteralPath $env:GITHUB_ENV -Value "VTOOLS_CACHE_DIR=$cache"
# Tests create their own module caches beneath VTMP, including nested staging
# filenames. The runner's default user temp path leaves too little MAX_PATH room.
$temporary = Join-Path $cache 'tmp'
$null = New-Item -ItemType Directory -Path $temporary
Add-Content -LiteralPath $env:GITHUB_ENV -Value "VTMP=$temporary"
Write-Host "Private tool cache: $cache"
