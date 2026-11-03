# Keep the downloaded snapshot intact and stream compatibility fixes to the C compiler.
# Only the pre-uint compiler needs its primitive spelling updated. Check the
# compiler's type-ID declaration, not arbitrary strings or C qualifier IDs.
BEGIN {
	while ((getline line < ARGV[1]) > 0) {
		snapshot_line++
		if (line ~ /^#define CUSTOM_DEFINE_cross([ \t]|$)/) portable_snapshot = 1
		if (line ~ /^bool machosign__fastc_hash_code_pages\([^;]*\) \{$/) portable_macho_hash = 1
		if (line ~ /^__v_result machosign__fastc_sign_macho_adhoc_buffered\(string path\) \{$/) portable_macho_signer = 1
		if (line ~ /^[ \t]*[A-Za-z_][A-Za-z0-9_* \t]*[ \t*]fastc_hash_code_pages_native\(/) native_macho_hash = 1
		if (line ~ /^[ \t]*[A-Za-z_][A-Za-z0-9_* \t]*[ \t*]fastc_sign_macho_adhoc_mapped\(/) native_macho_signer = 1
		if (previous_line ~ /^[ \t]*\{$/ && before_previous_line ~ /^[ \t]*#if \(defined\(__APPLE__\) && !defined\(__ENVIRONMENT_IPHONE_OS_VERSION_MIN_REQUIRED__\)\)$/) {
			if (line ~ /^[ \t]*fastc_hash_code_pages_native\(data_ptr, hashes_ptr, n_pages, code_limit\);$/) macho_hash_guard[snapshot_line - 2] = 1
			if (line ~ /^[ \t]*fastc_sign_macho_adhoc_mapped\(path\);$/) macho_signer_guard[snapshot_line - 2] = 1
		}
		before_previous_line = previous_line
		previous_line = line
		if (line ~ /^u32 flat__source_type_usize;$/) legacy_uint = 1
		if (line ~ /^u32 flat__source_type_uint;$/) native_uint = 1
		if (line ~ /^static int v3_linux_memory_file\(const char[ \t]*\*/) memory_file_const = 1
		if (line ~ /^_SL\([^,]+, "(u8\* malloc_noscan\(ptrdiff_t n\);|void\* memdup\(void\* src, ptrdiff_t sz\);|void\* memdup_align\(void\* src, ptrdiff_t sz, ptrdiff_t alignment\);)",/) {
			id = line
			sub(/^_SL\(/, "", id)
			sub(/,.*/, "", id)
			allocator_prototypes[id] = 1
		}
	}
	close(ARGV[1])
	repair_macho_hash = portable_snapshot && portable_macho_hash && portable_macho_signer && !native_macho_hash
	repair_macho_signer = portable_snapshot && portable_macho_hash && portable_macho_signer && !native_macho_signer
}

# Linux-generated snapshots briefly retained calls to omitted Darwin helpers.
# Select their existing portable #else branches, including Result propagation.
# Only match bare unresolved calls; native declarations and embedded strings
# are left intact. Both portable implementations must be present in the snapshot.
(repair_macho_hash && FNR in macho_hash_guard) || (repair_macho_signer && FNR in macho_signer_guard) {
	sub(/#if.*/, "#if 0 /* portable Mach-O signing */")
	print
	next
}

# The native helper takes const char*, but affected snapshots emitted a char*
# prototype for its c_extern declaration. Only fix snapshots with that helper;
# leave older non-const definitions and embedded source strings untouched.
memory_file_const && /^[ \t]*int v3_linux_memory_file\(char[ \t]*\*[ \t]*[A-Za-z_][A-Za-z0-9_]*\);[ \t]*$/ {
	sub(/\(char/, "(const char")
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
