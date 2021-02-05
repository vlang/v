module main
import os

// basic example which shows how to use the Command function



fn exec(path string, redirect bool) {
	// mut args := []string
	mut line := ""
	mut line_err := ""
	mut cmd := os.new_process("/bin/bash")

	if redirect{
		cmd.set_args(["-c '/bin/bash $path 2>&1'"])
	}else{
		cmd.set_args([path])
	}
	
	cmd.set_redirect_stdio()
	cmd.run()
	if cmd.is_alive() {
		for {

			line = cmd.stdout_read()
			println(line)

			line_err = cmd.stderr_read()
			println("E:$line_err")

			if ! cmd.is_alive() {
				break
			}
		}
	}
	if cmd.code > 0 {
		println("ERROR:")
		println(cmd)
		// println(cmd.stderr_read())
	}
}

fn main() {

	script:="
echo 1
#will use some stderr now
echo 2 1>&2
echo 3
"

	os.write_file("/tmp/test.sh",script) or {panic(err)}

	//this will work because stderr/stdout are smaller than 4096 chars, once larger there can be deadlocks
	exec("/tmp/test.sh",false)

	//this will always work
	exec("/tmp/test.sh",true)


}
