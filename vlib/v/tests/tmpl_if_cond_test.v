fn test_tmpl_if_cond() {
	cond := true
	processed := $tmpl('tmpl/if_cond.txt')
	assert processed == 'aaa
bbb
ccc

aaa
bbb
ccc
'
}

fn test_tmpl_if_else_cond() {
	cond := false
	processed := $tmpl('tmpl/if_cond.txt')
	assert processed == 'aaa
zzz
ccc

aaa
ccc
'
}
