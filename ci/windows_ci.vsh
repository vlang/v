import os
import windows

windows.run(os.args[1..]) or {
	eprintln(err)
	exit(1)
}
