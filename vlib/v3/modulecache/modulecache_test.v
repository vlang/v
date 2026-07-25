module modulecache

fn test_source_signature_cache_content_requires_stable_metadata() {
	details := SourceSignatureDetails{
		signature:  'content-signature'
		validation: ['env=NAME\tvalue']
	}
	if _ := source_signature_cache_content('before', 'after', details) {
		assert false, 'changed metadata must prevent source signature caching'
	}
	if _ := source_signature_cache_content('', '', details) {
		assert false, 'missing metadata must prevent source signature caching'
	}

	content := source_signature_cache_content('stable', 'stable', details) or {
		assert false, 'stable metadata should allow source signature caching'
		return
	}
	assert content.contains('metadata=stable\n')
	assert content.contains('source=content-signature\n')
	assert content.ends_with('complete=1\n')
}

fn test_source_uses_pseudo_in_quoted_compile_time_paths() {
	roots := ['@VMODROOT', '@VMOD_FILE', '@VROOT']
	assert source_uses_pseudo("module m\n\nconst data = \$embed_file('@VMODROOT/data.bin')",
		roots)
	assert source_uses_pseudo('module m\n\n#include "@VMODROOT/header.h"', roots)
	assert source_uses_pseudo('module m\n\nconst p = r"@VROOT/x"', roots)
	// a pseudo after a string containing `//` must still be seen
	assert source_uses_pseudo("module m\n\nconst u = 'http://x' + \$embed_file('@VMODROOT/y')",
		roots)
	// comments stay inert
	assert !source_uses_pseudo('module m\n\n// mentions @VMODROOT only in a comment', roots)
	assert !source_uses_pseudo("module m\n\nconst s = 'plain text'", roots)
	// name-boundary check still applies inside literals
	assert !source_uses_pseudo("module m\n\nconst s = '@VROOTX/not-a-pseudo'", roots)
}
