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
