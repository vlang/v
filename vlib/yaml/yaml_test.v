import (
	yaml
	json
	net.http
)

fn load_test_yaml(temp string){
	mut temp // yaml temp data.
	temp = temp.replace('<SPC>',' ')
	temp = temp.replace('<TAB>','\t')
}

fn event_check(temp string){
	
}

fn parse_test_suite(){

}