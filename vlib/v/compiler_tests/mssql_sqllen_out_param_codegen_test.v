import os

fn test_mssql_sqllen_out_parameters_preserve_native_storage() ! {
	root := os.join_path(os.vtmp_dir(), 'mssql_sqllen_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	main_v := os.join_path(root, 'main.v')
	out_c := os.join_path(root, 'out.c')
	os.write_file(main_v, 'import db.mssql\n\nfn main() {\n\t_ = mssql.Connection{}\n}\n')!
	generated := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-no-skip-unused',
		'-o', out_c, main_v])
	assert generated.exit_code == 0, generated.output
	c_source := os.read_file(out_c)!
	mut row_call := ''
	mut attribute_call := ''
	for line in c_source.split_into_lines() {
		if line.contains('SQLRowCount(') && line.contains('hstmt') {
			assert row_call == '', line
			row_call = line.trim_space()
		}
		if line.contains('SQLColAttribute(') && line.contains('hstmt') {
			assert attribute_call == '', line
			attribute_call = line.trim_space()
		}
	}
	assert row_call != '', 'SQLRowCount call was not generated'
	assert attribute_call != '', 'SQLColAttribute call was not generated'

	// Run the actual generated call statements against a native pointer-sized
	// writer. This needs neither ODBC headers/libraries nor a database server.
	// The high bits detect a 32-bit temporary on 64-bit targets; the surrounding
	// canaries also check that the native writes stay inside their output slots.
	row_probe := row_call.replace('h.hstmt', '(SQLHSTMT)0').replace('i16 retcode',
		'SQLRETURN retcode')
	attribute_probe := attribute_call.replace('h->hstmt', '(SQLHSTMT)0')
	header := '#include <stdint.h>
#include <stddef.h>
typedef intptr_t SQLLEN;
typedef void *SQLHSTMT;
typedef void *SQLPOINTER;
typedef int16_t SQLRETURN;
typedef int16_t SQLSMALLINT;
typedef uint16_t SQLUSMALLINT;
#define SQL_DESC_LENGTH 1003

static SQLLEN sqllen_probe_value(void) {
    return (SQLLEN)UINT64_C(0x1122334455667788);
}
static SQLRETURN SQLRowCount(SQLHSTMT statement, SQLLEN *value) {
    *value = sqllen_probe_value();
    return 0;
}
static SQLRETURN SQLColAttribute(SQLHSTMT statement, SQLUSMALLINT column,
    SQLUSMALLINT field, SQLPOINTER text, SQLSMALLINT text_size,
    SQLSMALLINT *text_length, SQLLEN *value) {
    *value = sqllen_probe_value();
    return 0;
}
static int mssql_sqllen_probe(void) {
    struct slot { uintptr_t before; SQLLEN value; uintptr_t after; };
    struct slot rows = { 0x13572468, 0, 0x24681357 };
    struct slot length = { 0x13572468, 0, 0x24681357 };
    SQLUSMALLINT i_col = 1;
    #define count_ret rows.value
    #define size_ret length.value
    ${row_probe}
    if (retcode != 0 || rows.value != sqllen_probe_value()
        || rows.before != 0x13572468 || rows.after != 0x24681357) return 1;
    ${attribute_probe}
    if (retcode != 0 || length.value != sqllen_probe_value()
        || length.before != 0x13572468 || length.after != 0x24681357) return 2;
    #undef count_ret
    #undef size_ret
    return 0;
}
'
	os.write_file(os.join_path(root, 'native.h'), header)!
	probe := os.join_path(root, 'probe.c.v')
	os.write_file(probe, '#include "@DIR/native.h"\nfn C.mssql_sqllen_probe() i32\nfn main() { assert C.mssql_sqllen_probe() == 0 }\n')!
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-cc', @CCOMPILER,
		'run', probe])
	assert result.exit_code == 0, result.output
	assert !row_call.contains('_cabi_out_val'), row_call
	assert !attribute_call.contains('_cabi_out_val'), attribute_call
}
