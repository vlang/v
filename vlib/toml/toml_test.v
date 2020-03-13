// test pattern created by BurntSushi and Iarna.
import (
	os
	toml
	yaml
	json
	net.http
)

fn test_BruntSushi_toml(){
	burntsushi_message := 'BruntSushi\'s TOML Test Suite(TOML-Valid)'
	eprintln(term.header(burntsushi_message, '-'))
	vexe := os.getenv('VEXE')
	if vexe.len == 0 || !os.exists(vexe){
		eprintln('VEXE must be set')
		exit(error_missing_vexe)
	}
	vroot := os.dir(vexe)
	input_files := os.walk_ext('$vroot/vlib/v/toml/tests/valid', '*.toml')
}

fn test_BruntSushi_json(){

}

fn test_iarna_toml(){
	message := 'iarna\'s TOML Test Suite(TOML-Valid)'
	eprintln(term.header(message, '-'))
	vexe := os.getenv('VEXE')
	if vexe.len == 0 || !os.exists(vexe){
		eprintln('VEXE must be set')
		exit(error_missing_vexe)
	}
	vroot := os.dir(vexe)
	input_files := os.walk_ext('$vroot/vlib/v/toml/tests/iarna/values', '*.toml')
}

fn test_iarna_yaml(){	
	message := 'iarna\'s TOML Test Suite(YAML)'
	eprintln(term.header(message, '-'))
	vexe := os.getenv('VEXE')
	if vexe.len == 0 || !os.exists(vexe){
		eprintln('VEXE must be set')
		exit(error_missing_vexe)
	}

	vroot := os.dir(vexe)
	input_files := os
}