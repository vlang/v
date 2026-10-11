The `v.vmod` module reads and writes `v.mod` project metadata.

`Manifest.catalog` stores package names mapped to version strings.
`Manifest.workspaces` stores workspace path strings. Both fields survive
`decode`/`encode` round trips. Catalog keys may be identifiers or quoted names;
duplicate keys and non-string values are errors. Workspaces require an array of strings.

```v oksyntax
import v.vmod

fn main() {
	manifest := vmod.decode("Module { catalog: { foo: '^1.2.0' } workspaces: ['packages/*'] }")!
	assert manifest.catalog['foo'] == '^1.2.0'
	assert manifest.workspaces == ['packages/*']
	decoded := vmod.decode(vmod.encode(manifest))!
	assert decoded.catalog == manifest.catalog
}
```

Catalog and workspace fields represent metadata; dependency strings retain their
literal meaning in the package manager.
