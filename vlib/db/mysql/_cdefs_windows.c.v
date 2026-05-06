module mysql

#flag windows -I@VEXEROOT/thirdparty/mysql/include

$if tinyc {
	#flag windows @VEXEROOT/thirdparty/mysql/lib/libmysql.dll
} $else {
	#flag windows @VEXEROOT/thirdparty/mysql/lib/libmysql.lib
}

#include <mysql.h> # Please install https://dev.mysql.com/downloads/installer/ , then copy the include, lib, and bin folders to thirdparty/mysql. Make sure thirdparty/mysql/bin and thirdparty/mysql/lib are on PATH, or copy libmysql.dll next to the produced .exe
