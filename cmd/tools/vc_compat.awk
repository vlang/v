# Keep the downloaded snapshot intact and stream compatibility fixes to the C compiler.
# Only the pre-uint compiler needs its primitive spelling updated. Check the
# compiler's type-ID declaration, not arbitrary strings or C qualifier IDs.
BEGIN {
	while ((getline line < ARGV[1]) > 0) {
		snapshot_line++
		if (line ~ /^#define CUSTOM_DEFINE_cross([ \t]|$)/) portable_snapshot = 1
		# Only a top-level poll include escapes the snapshot's libc guard.
		if (line ~ /^[ \t]*#[ \t]*if/) pp_depth++
		else if (line ~ /^[ \t]*#[ \t]*endif/) pp_depth--
		else if (pp_depth == 0 && line ~ /^#include <poll.h>[ \t]*$/) bare_poll_line = snapshot_line
		# The published vc snapshot calls select() but has no sys/select.h prototype.
		if (line ~ /select\(fd \+ 1, &read_set,/) snapshot_uses_select = 1
		if (line ~ /^[ \t]*#[ \t]*include[ \t]+<sys\/select\.h>[ \t]*$/) snapshot_has_select_header = 1
		if (line ~ /^bool machosign__fastc_hash_code_pages\([^;]*\) \{$/) portable_macho_hash = 1
		if (line ~ /^__v_result machosign__fastc_sign_macho_adhoc_buffered\(string path\) \{$/) portable_macho_signer = 1
		if (line ~ /^__v_result_bool driver__publish_native_c_object\(string temporary, string destination\) \{$/) cache_publish_start = snapshot_line
		if (cache_publish_start && line ~ /^[ \t]*__v_result [A-Za-z_][A-Za-z0-9_]* = os__link\(temporary, destination\);$/) {
			cache_publish_functions[cache_publish_start] = 1
			cache_publish_calls[snapshot_line] = 1
		}
		if (line ~ /^\}$/) cache_publish_start = 0
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

# Fresh Termux builds run the downloaded compiler before the fixed driver exists.
# Bridge only its old native-object cache publication call, preserving os.link
# everywhere else and leaving regenerated snapshots unchanged.
ENVIRON["V_BOOTSTRAP_VROOT"] != "" && FNR in cache_publish_functions {
	root = ENVIRON["V_BOOTSTRAP_VROOT"]
	gsub(/\\/, "/", root)
	print "#if defined(__linux__)"
	print "#include \"" root "/vlib/v/driver/native_object_cache.h\""
	print "#endif"
	print "static __v_result v_bootstrap_cache_link(string temporary, string destination) {"
	print "#if defined(__linux__)"
	print "    if (v_native_object_cache_publish((const char*)temporary.str, (const char*)destination.str) == 0)"
	print "        return (__v_result){.ok = true};"
	print "#endif"
	print "    return os__link(temporary, destination);"
	print "}"
}

ENVIRON["V_BOOTSTRAP_VROOT"] != "" && FNR in cache_publish_calls {
	sub(/os__link\(temporary, destination\)/, "v_bootstrap_cache_link(temporary, destination)")
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

# The current vc snapshot uses select() but includes only poll.h, not its
# defining header. Insert the native declaration on Unix, leaving unrelated
# snapshots and the Winsock path unchanged.
/^[ \t]*#[ \t]*include[ \t]+<poll\.h>[ \t]*$/ {
	if (FNR == bare_poll_line) print "#if !__V_HEADERLESS_LIBC"
	print
	if (snapshot_uses_select && !snapshot_has_select_header && !select_header_inserted) {
		print "#if !defined(_WIN32) && !__V_HEADERLESS_LIBC"
		print "#include <sys/select.h>"
		print "#endif"
		select_header_inserted = 1
	}
	if (FNR == bare_poll_line) print "#endif"
	next
}

# A bare poll include made os header-owned, suppressing its C prototypes.
# Restore only the affected snapshot's missing declarations and constants after
# its preamble types are available, without importing another libc header.
bare_poll_line && /^bool os__fd_is_pending\(__v_int fd\) \{$/ {
	print "#if __V_HEADERLESS_LIBC && !defined(_WIN32)"
	print "int select(int, fd_set*, fd_set*, fd_set*, struct timeval*);"
	print "#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__DragonFly__) || defined(__ANDROID__)"
	print "int poll(struct pollfd*, unsigned int, int);"
	print "#else"
	print "int poll(struct pollfd*, size_t, int);"
	print "#endif"
	print "#ifndef POLLIN"
	print "#define POLLIN 0x001"
	print "#define POLLERR 0x008"
	print "#define POLLHUP 0x010"
	print "#define POLLNVAL 0x020"
	print "#endif"
	print "#endif"
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

# The f5b8e41 vc snapshot includes POSIX C bindings from os/fd.c.v.
# Earlier snapshots did not; until vc regenerates with the libc-owned binding,
# its struct pollfd would redefine the type already supplied by <poll.h>.
# Headerless snapshots still need their own type, with the native int-sized fd.
/^struct pollfd[ \t]*\{[ \t]*$/ {
	print "#if defined(_WIN32) || __V_HEADERLESS_LIBC"
	vc_pollfd_guard = 1
}

vc_pollfd_guard && /^\t__v_int fd;$/ {
	print "\ti32 fd;"
	next
}

vc_pollfd_guard && /^};[ \t]*$/ {
	print
	print "#endif"
	vc_pollfd_guard = 0
	next
}

# The same snapshot also redeclares types from BSD system headers.
# Keep the headerless declarations, including on FreeBSD where no system
# header supplies these types in that mode.
/^struct timespec[ \t]*\{.*\};[ \t]*$/ {
	print "#if !defined(__FreeBSD__) || __V_HEADERLESS_LIBC"
	print
	print "#endif"
	next
}

/^struct itimerspec[ \t]*\{[ \t]*$/ {
	print "#if !defined(__FreeBSD__) || __V_HEADERLESS_LIBC"
	vc_itimerspec_guard = 1
}

vc_itimerspec_guard && /^};[ \t]*$/ {
	print
	print "#endif"
	vc_itimerspec_guard = 0
	next
}

/^typedef union \{ unsigned char _opaque\[128\]; long long _align; \} sigset_t;[ \t]*$/ {
	print "#if !defined(__OpenBSD__)"
	print
	print "#endif"
	next
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
