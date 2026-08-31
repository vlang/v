module bin

import os

// HygieneFinding reports only the affected path and count, never matched private text.
pub struct HygieneFinding {
pub:
	path  string
	count int
}

// public_hygiene_findings checks public files for attribution and private-host residue.
pub fn public_hygiene_findings(paths []string) ![]HygieneFinding {
	mut findings := []HygieneFinding{}
	for path in paths {
		if !os.is_file(path) {
			return error('hygiene input is not a regular file')
		}
		content := os.read_file(path)!
		count := public_hygiene_count(content)
		if count > 0 {
			findings << HygieneFinding{
				path:  path
				count: count
			}
		}
	}
	return findings
}

// public_hygiene_count returns a sanitized count for one in-memory public artifact.
pub fn public_hygiene_count(content string) int {
	lower := content.to_lower()
	patterns := [
		'pullrequest' + 'review-',
		'generated' + ' by an ai',
		'written' + ' by an ai',
		'reviewed' + ' by an ai',
		'assist' + 'ant attribution',
		'internal ag' + 'ent',
		'/ho' + 'me/',
		'c:\\us' + 'ers\\',
	]
	mut count := 0
	for pattern in patterns {
		count += lower.count(pattern)
	}
	return count
}
