// Tests the `$if bsd` family predicate, which should evaluate to true on
// any BSD-family host (macos, freebsd, openbsd, netbsd, dragonfly) and
// false on every other host.  Prior to the fix in
// vlib/v/ast/comptime_valid_idents.v, `$if bsd` was accepted by the parser
// but silently evaluated to false everywhere.

// host_is_bsd returns the runtime truth: whether this build targets a
// BSD-family OS.  Uses the exact-OS predicates, which are independently
// known to work.
fn host_is_bsd() bool {
	$if macos {
		return true
	}
	$if freebsd {
		return true
	}
	$if openbsd {
		return true
	}
	$if netbsd {
		return true
	}
	$if dragonfly {
		return true
	}
	return false
}

// comptime_bsd returns what `$if bsd` resolves to on this host.
fn comptime_bsd() bool {
	$if bsd {
		return true
	}
	return false
}

// comptime_not_bsd returns what `$if !bsd` resolves to on this host.
fn comptime_not_bsd() bool {
	$if !bsd {
		return true
	}
	return false
}

fn test_bsd_matches_exact_bsd_family_targets() {
	assert comptime_bsd() == host_is_bsd()
}

fn test_not_bsd_is_the_inverse_of_bsd() {
	assert comptime_not_bsd() == !comptime_bsd()
}

// An exact-OS predicate implies `bsd` on that OS.  This guards against a
// regression where `$if freebsd` and `$if bsd` could disagree on a FreeBSD
// build (for example if someone changed the is_bsd_target() list but not
// the comptime evaluator, or vice versa).
fn test_exact_bsd_os_implies_bsd() {
	$if macos {
		$if !bsd {
			assert false, '`\$if bsd` should be true on macos'
		}
	}
	$if freebsd {
		$if !bsd {
			assert false, '`\$if bsd` should be true on freebsd'
		}
	}
	$if openbsd {
		$if !bsd {
			assert false, '`\$if bsd` should be true on openbsd'
		}
	}
	$if netbsd {
		$if !bsd {
			assert false, '`\$if bsd` should be true on netbsd'
		}
	}
	$if dragonfly {
		$if !bsd {
			assert false, '`\$if bsd` should be true on dragonfly'
		}
	}
}

// Non-BSD hosts must see `$if bsd` as false.
fn test_non_bsd_os_implies_not_bsd() {
	$if linux {
		$if bsd {
			assert false, '`\$if bsd` should be false on linux'
		}
	}
	$if windows {
		$if bsd {
			assert false, '`\$if bsd` should be false on windows'
		}
	}
	$if solaris {
		$if bsd {
			assert false, '`\$if bsd` should be false on solaris'
		}
	}
	$if haiku {
		$if bsd {
			assert false, '`\$if bsd` should be false on haiku'
		}
	}
}

// `bsd` must compose with other comptime conditions the same as any
// other OS predicate.
fn test_bsd_in_compound_conditions() {
	mut linux_or_bsd := false
	$if linux || bsd {
		linux_or_bsd = true
	}
	// On any supported Unix-like host, at least one of these is true.
	$if linux {
		assert linux_or_bsd
	}
	$if bsd {
		assert linux_or_bsd
	}
	// On Windows, neither should be true.
	$if windows {
		assert !linux_or_bsd
	}

	mut bsd_and_not_windows := false
	$if bsd && !windows {
		bsd_and_not_windows = true
	}
	assert bsd_and_not_windows == comptime_bsd()
}
