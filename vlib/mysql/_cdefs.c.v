module mysql

struct C.MYSQL
struct C.MYSQL_RES
struct C.MYSQL_FIELD {
	name             byteptr /* Name of column */
	org_name         byteptr /* Original column name, if an alias */
	table            byteptr /* Table of column if column was a field */
	org_table        byteptr /* Org table name, if table was an alias */
	db               byteptr /* Database for table */
	catalog          byteptr /* Catalog for table */
	def              byteptr /* Default value (set by mysql_list_fields) */
	length           int     /* Width of column (create length) */
	max_length       int     /* Max width for selected set */
	name_length      u32
	org_name_length  u32
	table_length     u32
	org_table_length u32
	db_length        u32
	catalog_length   u32
	def_length       u32
	flags            u32     /* Div flags */
	decimals         u32     /* Number of decimals in field */
	charsetnr        u32     /* Character set */
	@type            int     /* Type of field. See mysql_com.h for types */
}

fn C.mysql_init(mysql &C.MYSQL) &C.MYSQL
fn C.mysql_real_connect(mysql &C.MYSQL, host byteptr, user byteptr, passwd byteptr, db byteptr, port u32, unix_socket byteptr, clientflag u64) &C.MYSQL
fn C.mysql_query(mysql &C.MYSQL, q byteptr) int
fn C.mysql_select_db(mysql &C.MYSQL, db byteptr) int
fn C.mysql_change_user(mysql &C.MYSQL, user byteptr, password byteptr, db byteptr) bool
fn C.mysql_affected_rows(mysql &C.MYSQL) u64
fn C.mysql_options(mysql &C.MYSQL, option int, arg voidptr) int
fn C.mysql_get_option(mysql &C.MYSQL, option int, arg voidptr) int
fn C.mysql_num_fields(res &C.MYSQL_RES) int
fn C.mysql_autocommit(mysql MYSQL, mode bool)
fn C.mysql_refresh(mysql MYSQL, options u32) int
fn C.mysql_reset_connection(mysql MYSQL) int
fn C.mysql_ping(mysql MYSQL) int
fn C.mysql_store_result(mysql &C.MYSQL) &C.MYSQL_RES
fn C.mysql_fetch_row(res &C.MYSQL_RES) &byteptr
fn C.mysql_fetch_fields(res &C.MYSQL_RES) &C.MYSQL_FIELD
fn C.mysql_free_result(res &C.MYSQL_RES)
fn C.mysql_real_escape_string_quote(mysql &C.MYSQL, to byteptr, from byteptr, len u64, quote byte) u64
fn C.mysql_close(sock &C.MYSQL)

/* INFO & VERSION */
fn C.mysql_info(mysql &C.MYSQL) byteptr
fn C.mysql_get_host_info(mysql &C.MYSQL) byteptr
fn C.mysql_get_server_info(mysql &C.MYSQL) byteptr
fn C.mysql_get_server_version(mysql &C.MYSQL) u64
fn C.mysql_get_client_version() u64
fn C.mysql_get_client_info() byteptr

/* DEBUG & ERROR INFO */
fn C.mysql_error(mysql &C.MYSQL) byteptr
fn C.mysql_errno(mysql &C.MYSQL) int
fn C.mysql_dump_debug_info(mysql &C.MYSQL) int
fn C.mysql_debug(debug byteptr)
