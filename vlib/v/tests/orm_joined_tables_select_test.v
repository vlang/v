import sqlite

struct VieterDb {
	conn sqlite.DB
}

pub struct GitRepoArch {
pub:
	id      int [primary; sql: serial]
	repo_id int [nonull]
	// repo string
	value string [nonull]
}

pub struct GitRepo {
pub mut:
	id   int           [optional; primary; sql: serial]
	repo string        [nonull]
	arch []GitRepoArch [fkey: 'repo_id']
}

pub fn (db &VieterDb) get_git_repos() []GitRepo {
	// NB: the query here, uses the `repo` field on GitRepo,
	// while GitRepo is joined to GitRepoArch,
	// which does *not* have a `repo` field.
	// When this test was added, the checker used `GitRepoArch`
	// to check for `repo`'s presence (which is wrong), because of
	// a lingering c.cur_orm_ts state. The fix was to save/restore c.cur_orm_ts .
	res := sql db.conn {
		select from GitRepo where repo == 'something' order by id
	}
	return res
}

fn test_compiles() {
	assert true
}
