// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module http

import strings 

#flag windows -I @VROOT/thirdparty/vschannel
#flag -lws2_32 -lsecurity -lCRYPT32 -lwininet
 

// #include "vschannel.h"
#include "vschannel.c"


fn schannel_do(method, host_name, path string) string { 
    a := int(C.joe_test())
    println('A: '+a.str())
	mut buf := malloc(10000)
	mut length := 0
	println('Here C')
    C.schannel_request(host_name.str, path.str, buf, &length)
    println('Here D')
	
    z := string(buf)
    println(z)

	return z
}
