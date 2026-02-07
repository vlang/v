module mysql

#flag windows -I@VEXEROOT/thirdparty/mysql/include

$if tinyc {
	#flag windows @VEXEROOT/thirdparty/mysql/lib/libmysql.dll
} $else {
	#flag windows @VEXEROOT/thirdparty/mysql/lib/libmysql.lib
}

#include <mysql.h> # Please install https://dev.mysql.com/downloads/installer/ , then put the include/ and lib/ folders in thirdparty/mysql. Do not forget to add thirdparty/mysql/bin and thirdparty/mysql/lib to PATH
