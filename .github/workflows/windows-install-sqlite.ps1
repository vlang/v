$ErrorActionPreference = 'Stop'

$downloadPage = 'https://sqlite.org/download.html'
Write-Host "> Getting $downloadPage ..."
$page = Invoke-WebRequest -Uri $downloadPage -UseBasicParsing
$line = ($page.Content -split "`n") |
    Where-Object { $_ -match '^PRODUCT,.*amalgamation' } |
    Select-Object -First 1
if (-not $line) {
    throw "Could not find amalgamation PRODUCT line in $downloadPage"
}
$parts = $line.Split(',')
if ($parts.Length -lt 5) {
    throw "Malformed PRODUCT line: $line"
}
$version      = $parts[1]
$relpath      = $parts[2]
$expectedSize = [int64]$parts[3]
$expectedSha3 = $parts[4].Trim().ToLower()
$zipName      = [System.IO.Path]::GetFileName($relpath)
$zipUrl       = "https://sqlite.org/$relpath"
$amalgName    = ($zipName.ToLower() -replace '\.zip$','')

Write-Host "> SQLite amalgamation version: $version"
Write-Host "> URL: $zipUrl"
Write-Host "> expected size: $expectedSize"
Write-Host "> expected SHA3: $expectedSha3"

Write-Host "> Downloading $zipUrl ..."
Invoke-WebRequest -Uri $zipUrl -OutFile $zipName -UseBasicParsing

$actualSize = (Get-Item $zipName).Length
if ($actualSize -ne $expectedSize) {
    throw "Download size $actualSize != expected $expectedSize"
}
Write-Host "> download size: $actualSize matches expected size: $expectedSize"

# SHA3_256 is supported by Get-FileHash starting in PowerShell 7.4. Probe support
# by attempting one call; if the algorithm is not recognized, fall back to a
# warning rather than failing the build.
$sha3Supported = $true
try {
    [void](Get-FileHash -Algorithm SHA3_256 -Path $zipName)
} catch {
    $sha3Supported = $false
    Write-Warning "SHA3_256 not supported on this PowerShell ($($PSVersionTable.PSVersion)); skipping checksum verification"
}
if ($sha3Supported) {
    $hash = (Get-FileHash -Algorithm SHA3_256 -Path $zipName).Hash.ToLower()
    if ($hash -ne $expectedSha3) {
        throw "SHA3 mismatch: got $hash, expected $expectedSha3"
    }
    Write-Host "> download sha3: $hash matches"
}

Write-Host "> Extracting $zipName to thirdparty\ ..."
if (-not (Test-Path thirdparty)) {
    New-Item -ItemType Directory -Path thirdparty | Out-Null
}
Expand-Archive -Path $zipName -DestinationPath thirdparty -Force

if (Test-Path thirdparty\sqlite) {
    Remove-Item -Recurse -Force thirdparty\sqlite
}
Move-Item -Path "thirdparty\$amalgName" -Destination thirdparty\sqlite

if (Test-Path thirdparty\sqlite\shell.c) {
    Remove-Item thirdparty\sqlite\shell.c
}

Get-ChildItem thirdparty\sqlite -File | ForEach-Object {
    Write-Host ('> extracted file: {0,-40} | size: {1,8}' -f $_.FullName, $_.Length)
}

Write-Host "> removing $zipName ..."
Remove-Item $zipName
Write-Host '> done'
