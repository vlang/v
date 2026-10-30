# Keep the downloaded snapshot intact and stream compatibility fixes to the C compiler.
# Only the pre-uint compiler needs its primitive spelling updated. Check the
# compiler's type-ID declaration, not arbitrary strings or C qualifier IDs.
BEGIN {
	while ((getline line < ARGV[1]) > 0) {
		if (line ~ /^u32 flat__source_type_usize;$/) legacy_uint = 1
		if (line ~ /^u32 flat__source_type_uint;$/) native_uint = 1
		if (line ~ /^_SL\([^,]+, "(u8\* malloc_noscan\(ptrdiff_t n\);|void\* memdup\(void\* src, ptrdiff_t sz\);|void\* memdup_align\(void\* src, ptrdiff_t sz, ptrdiff_t alignment\);)",/) {
			id = line
			sub(/^_SL\(/, "", id)
			sub(/,.*/, "", id)
			allocator_prototypes[id] = 1
		}
	}
	close(ARGV[1])
}

# Relocate only include directives in generated bootstrap C. Header contents and
# their nested includes remain entirely with the C preprocessor.
ENVIRON["V_VC_COMPAT_ROOT"] != "" && /^[ \t]*#[ \t]*include[ \t]+"/ {
	match($0, /^[ \t]*#[ \t]*include[ \t]+"/)
	prefix = substr($0, 1, RLENGTH)
	rest = substr($0, RLENGTH + 1)
	end_quote = index(rest, "\"")
	if (end_quote > 0) {
		header = substr(rest, 1, end_quote - 1)
		gsub(/\\/, "/", header)
		if (header ~ /^\// || header ~ /^[A-Za-z]:\//) {
			if (match(header, /\/(vlib|thirdparty|cmd\/v)\//)) {
				header = substr(header, RSTART + 1)
			}
		}
		if (header ~ /^(vlib|thirdparty|cmd\/v)\//) {
			$0 = prefix ENVIRON["V_VC_COMPAT_ROOT"] "/" header substr(rest, end_quote)
		}
	}
}

legacy_uint && !native_uint {
	gsub(/"usize", 5/, "\"uint\", 4")
	# The migrated allocator declarations use int. Rewrite their codegen calls
	# using the selected target type, without changing the snapshot's own ABI.
	for (id in allocator_prototypes) {
		sub("c__FlatGen__writeln\\(g, " id "\\);",
			"c__FlatGen__writeln(g, string__replace(" id ", (string){(u8*)\"ptrdiff_t\", 9, 1}, g->int_ct));")
	}
}

# Older portable snapshots return long long here but declare the function as i64.
# On Linux, i64 is int64_t (long), which conflicts even though both types are 64-bit.
/^(V_PEAK_RSS_SCOPE|static|static inline) long long v3_bench_peak_rss_kb\(void\)[ \t]*\{/ {
	sub(/long long/, "i64")
}

# Bootstrap compatibility for vc snapshots predating the diagserver prctl guards.
/^[ \t]*#include[ \t]+<sys\/prctl.h>[ \t]*$/ {
	print "#if defined(__linux__) && !defined(__ANDROID__)"
	print
	print "#endif"
	next
}

/^[ \t]*if \(prctl\(PR_SET_PDEATHSIG,/ {
	print "#if defined(__linux__) && !defined(__ANDROID__)"
	# Generated C closes this block at the same indentation as its opening if.
	prctl_end = $0
	sub(/if.*/, "}", prctl_end)
}

{
	print
	if (prctl_end != "" && $0 == prctl_end) {
		print "#endif"
		prctl_end = ""
	}
}

END {
	if (prctl_end != "") {
		print "Unterminated prctl block in the vc bootstrap snapshot" > "/dev/stderr"
		exit 1
	}
}
