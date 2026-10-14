# Keep the downloaded snapshot intact and stream compatibility fixes to the C compiler.
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
