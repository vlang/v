# Keep downloaded snapshots intact, including string literals with non-UTF-8 bytes.
param(
    [Parameter(Mandatory = $true)][string]$Source,
    [Parameter(Mandatory = $true)][string]$Destination
)
$ErrorActionPreference = 'Stop'
$encoding = [Text.Encoding]::GetEncoding(28591)
$text = [IO.File]::ReadAllText($Source, $encoding)
if ($text -match '(?m)^u32 flat__source_type_usize;\r?$' -and
    $text -notmatch '(?m)^u32 flat__source_type_uint;\r?$') {
    $text = $text.Replace('"usize", 5', '"uint", 4')
    # Rewrite codegen calls using the selected target's integer type. The
    # snapshot's interned strings and its own allocator ABI remain unchanged.
    $prototypes = '(?m)^_SL\(([^,]+), "(?:u8\* malloc_noscan\(ptrdiff_t n\);|void\* memdup\(void\* src, ptrdiff_t sz\);|void\* memdup_align\(void\* src, ptrdiff_t sz, ptrdiff_t alignment\);)",'
    foreach ($match in [regex]::Matches($text, $prototypes)) {
        $id = $match.Groups[1].Value
        $text = $text.Replace("c__FlatGen__writeln(g, $id);",
            ('c__FlatGen__writeln(g, string__replace(' + $id + ', (string){(u8*)"ptrdiff_t", 9, 1}, g->int_ct));'))
    }
}

# Relocate V-owned includes in portable snapshots without opening their headers.
# Keep existing bytes intact, including non-UTF-8 literals; encode the new path as UTF-8.
$bootstrapRoot = (Resolve-Path -LiteralPath (Join-Path $PSScriptRoot '../..')).Path.Replace('\', '/')
$bootstrapRootBytes = $encoding.GetString([Text.Encoding]::UTF8.GetBytes($bootstrapRoot))
$includePattern = '(?m)^(?<prefix>[ \t]*#[ \t]*include[ \t]+")(?<path>[^"\r\n]+)(?<suffix>"[^\r\n]*)'
$text = [regex]::Replace($text, $includePattern, {
    param($includeMatch)
    $includePath = $includeMatch.Groups['path'].Value.Replace('\', '/')
    $ownedPath = [regex]::Match($includePath, '/(?:vlib|thirdparty|cmd)/')
    if ($includePath -match '^(?:/|[A-Za-z]:/)' -and $ownedPath.Success) {
        return $includeMatch.Groups['prefix'].Value + $bootstrapRootBytes +
            $includePath.Substring($ownedPath.Index) + $includeMatch.Groups['suffix'].Value
    }
    return $includeMatch.Value
})
[IO.File]::WriteAllText($Destination, $text, $encoding)
