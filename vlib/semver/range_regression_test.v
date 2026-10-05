import semver

fn test_operator_ranges_with_a_wildcard_major() {
	for wildcard in ['*', 'x', 'X'] {
		for operator in ['>=', '<=', '='] {
			for version in ['0.0.0', '1.2.3', '9.9.9'] {
				assert semver.from(version)!.satisfies(operator + wildcard)
			}
			assert !semver.from('1.2.3-alpha')!.satisfies(operator + wildcard)
		}
		for operator in ['>', '<'] {
			assert !semver.from('0.0.0')!.satisfies(operator + wildcard)
			assert !semver.from('9.9.9')!.satisfies(operator + wildcard)
		}
	}
}

fn test_operator_ranges_with_a_partial_or_patch_wildcard() {
	for partial in ['1.2', '1.2.x', '1.2.X', '1.2.*'] {
		assert semver.from('1.2.0')!.satisfies('>=' + partial)
		assert !semver.from('1.1.9')!.satisfies('>=' + partial)
		assert semver.from('1.2.9')!.satisfies('<=' + partial)
		assert !semver.from('1.3.0')!.satisfies('<=' + partial)
		assert !semver.from('1.2.1')!.satisfies('>' + partial)
		assert semver.from('1.3.0')!.satisfies('>' + partial)
		assert semver.from('1.1.9')!.satisfies('<' + partial)
		assert !semver.from('1.2.0')!.satisfies('<' + partial)
		assert semver.from('1.2.9')!.satisfies('=' + partial)
		assert !semver.from('1.3.0')!.satisfies('=' + partial)
	}
}

fn test_prerelease_hyphens_and_metadata_wildcard_characters_are_identifiers() {
	for version in ['1.2.3-a-.b', '1.2.3-a.-b', '1.2.3-a.-', '1.2.3--'] {
		assert semver.from(version)!.satisfies(version)
		assert semver.from(version)!.satisfies('>=' + version)
	}
	for version in ['1.2.3+x', '1.2.3+build.X', '1.2.3-alpha+x'] {
		assert semver.from(version)!.satisfies('>=' + version)
		assert semver.from(version)!.satisfies('<=' + version)
	}
	for malformed in ['1.2.3-a..b', '1.2.3-.a', '1.2.3-a.', '1.2-a', '1.2.'] {
		assert !semver.from('1.2.3')!.satisfies(malformed)
	}
}

fn test_composed_ranges_exclude_the_next_series_prerelease() {
	for range in ['1.2.x >=1.3.0-alpha', '~1.2.3 >=1.3.0-alpha'] {
		assert !semver.from('1.3.0-alpha')!.satisfies(range)
	}
	assert !semver.from('2.0.0-alpha')!.satisfies('^1.2.3 >=2.0.0-alpha')
	assert !semver.from('1.0.0-alpha')!.satisfies('0.x >=1.0.0-alpha')
	assert !semver.from('0.2.0-alpha')!.satisfies('0.1.x >=0.2.0-alpha')
	assert !semver.from('0.1.0-alpha')!.satisfies('^0.0 >=0.1.0-alpha')
	assert semver.from('1.2.3-beta.2')!.satisfies('^1.2.3-beta.1 >=1.2.3-beta.2')
}
