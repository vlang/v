import os

const vinix_native_header_prefix = 'typedef __SIZE_TYPE__ size_t;\ntypedef __PTRDIFF_TYPE__ isize;\n'

fn vinix_native_header_compiler() string {
	return if @CCOMPILER in ['tinyc', 'tcc'] { 'cc' } else { @CCOMPILER }
}

fn test_vinix_hosted_prelude_uses_native_stdio_and_string_types() ! {
	$if !linux {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'vinix_hosted_headers_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	header := os.join_path(@VEXEROOT, 'vlib/v/gen/c/manual_stdlib_c_headers.h')
	source := os.join_path(dir, 'hosted.c')
	binary := os.join_path(dir, 'hosted')
	os.write_file(source, vinix_native_header_prefix + '#include "${header}"\n' + r'
#include <stdio.h>
#include <string.h>
static int (*compare)(const char *, const char *) = strcmp;
static int (*compare_n)(const char *, const char *, size_t) = strncmp;
static size_t (*length)(const char *) = strlen;
static char *(*read_line)(char *, int, FILE *) = fgets;
int main(void) {
	FILE *stream = tmpfile();
	if (!stream) return 1;
	if (fputs("vinix", stream) == EOF) return 2;
	rewind(stream);
	char text[16];
	if (!read_line(text, sizeof text, stream)) return 3;
	if (compare(text, "vinix") || compare_n(text, "vin", 3) || length(text) != 5) return 4;
	return fclose(stream);
}
')!
	compiled := os.exec([vinix_native_header_compiler(), '-std=gnu11', '-Werror', '-Ulinux', '-U__linux',
		'-U__linux__', '-U__gnu_linux__', '-D__vinix__', source, '-o', binary])
	assert compiled.exit_code == 0, compiled.output
	result := os.exec([binary])
	assert result.exit_code == 0, result.output
}

fn test_vinix_freestanding_prelude_keeps_its_headerless_abi() ! {
	$if !linux {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'vinix_freestanding_headers_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	header := os.join_path(@VEXEROOT, 'vlib/v/gen/c/manual_stdlib_c_headers.h')
	source := os.join_path(dir, 'freestanding.c')
	os.write_file(source, vinix_native_header_prefix + '#include "${header}"\n' + r'
_Static_assert(__builtin_types_compatible_p(FILE, struct __file), "Vinix FILE type");
int (*compare)(char *, char *) = strcmp;
int (*compare_n)(char *, char *, size_t) = strncmp;
size_t (*length)(char *) = strlen;
char *(*read_line)(char *, size_t, FILE *) = fgets;
')!
	compiled := os.exec([vinix_native_header_compiler(), '-std=gnu11', '-Werror', '-ffreestanding',
		'-nostdinc', '-Ulinux', '-U__linux', '-U__linux__', '-U__gnu_linux__', '-D__vinix__', '-c',
		source, '-o', os.join_path(dir, 'freestanding.o')])
	assert compiled.exit_code == 0, compiled.output
}
