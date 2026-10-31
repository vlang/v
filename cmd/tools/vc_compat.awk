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

# Snapshots retain native includes rather than embedding headers. Relocate only
# V-owned include paths in generated C to the checkout doing the bootstrap.
# Never open or expand the included files here.
/^[ \t]*#[ \t]*include[ \t]+"/ && ENVIRON["V_BOOTSTRAP_VROOT"] != "" {
	path = $0
	sub(/^[^"]*"/, "", path)
	sub(/".*/, "", path)
	gsub(/\\/, "/", path)
	if (path ~ /^(\/|[A-Za-z]:\/)/ && match(path, /\/(vlib|thirdparty|cmd)\//)) {
		root = ENVIRON["V_BOOTSTRAP_VROOT"]
		gsub(/\\/, "/", root)
		$0 = substr($0, 1, index($0, "\"")) root "/" substr(path, RSTART + 1) "\""
	}
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
