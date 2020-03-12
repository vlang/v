// test pattern created by BurntSushi and Iarna.
import (
	os
	toml
	yaml
	json
	net.http
)

fn test_BruntSushi(){
	burntsushi_message := 'BruntSushi\'s TOML Test Suite'
	eprintln(term.header(burntsushi_message, '-'))
	vexe := os.getenv('VEXE')
	if vexe.len == 0 || !os.exists(vexe){
		eprintln('VEXE must be set')
		exit(error_missing_vexe)
	}
	vroot := os.dir(vexe)
}

fn test_iarna(){
	burntsushi_message := 'iarna\'s TOML Test Suite'
	eprintln(term.header(burntsushi_message, '-'))
	vexe := os.getenv('VEXE')
	if vexe.len == 0 || !os.exists(vexe){
		eprintln('VEXE must be set')
		exit(error_missing_vexe)
	}
	vroot := os.dir(vexe)
}