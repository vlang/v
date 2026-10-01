# Bootstrap compatibility for vc snapshots predating the diagserver prctl guards.
# Keep the downloaded snapshot intact and stream the guards to the C compiler.
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
