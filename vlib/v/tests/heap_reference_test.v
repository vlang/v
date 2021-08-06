[heap]
struct GitStructure {
pub mut:
	root  string
	repos []&GitRepo
}

[heap]
struct GitRepo {
	id int [skip]
pub:
	path string // path on filesystem
	name string
}

pub fn (mut gitstructure GitStructure) repo_get(name string) ?&GitRepo {
	for r in gitstructure.repos {
		if r.name == name {
			if name != '' {
				r2 := gitstructure.repos[r.id]
				return r2
			}
		}
	}
	return error("Could not find repo for account name: '${name}'")
}

fn test_opt_ref_return() {
	mut gitstruct := GitStructure{
		root: 'r'
		repos: [
			&GitRepo{
				id: 0
				path: 'testpath'
				name: 'thename'
			},
		]
	}
	mut err_msg := ''
	a := gitstruct.repo_get('thename') or {
		err_msg = '${err}'
		r := &GitRepo{}
		r
	}
	assert a.path == 'testpath'
	assert err_msg == ''
	b := gitstruct.repo_get('wrong_name') or {
		err_msg = '${err}'
		r := &GitRepo{}
		r
	}
	assert b.path == ''
	assert err_msg == "Could not find repo for account name: 'wrong_name'"
}

[heap]
struct GitStructureNoRef {
pub mut:
	root  string
	repos []GitRepo
}

pub fn (mut gitstructure GitStructureNoRef) repo_get(name string) ?&GitRepo {
	for r in gitstructure.repos {
		if r.name == name {
			if name != '' {
				r2 := &gitstructure.repos[r.id]
				return r2
			}
		}
	}
	return error("Could not find repo for account name: '${name}'")
}

fn test_opt_return() {
	mut gitstruct := &GitStructureNoRef{
		root: 'r'
		repos: [
			GitRepo{
				id: 0
				path: 'testpath2'
				name: 'thename2'
			},
		]
	}
	mut err_msg := ''
	a := gitstruct.repo_get('thename2') or {
		err_msg = '${err}'
		r := &GitRepo{}
		r
	}
	assert a.path == 'testpath2'
	assert err_msg == ''
	b := gitstruct.repo_get('wrong_name2') or {
		err_msg = '${err}'
		r := &GitRepo{}
		r
	}
	assert b.path == ''
	assert err_msg == "Could not find repo for account name: 'wrong_name2'"
}
