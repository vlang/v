module user

// User represents a user account, similar to Go's os/user.User.
pub struct User {
pub:
	uid      string
	gid      string
	username string
	name     string
	home_dir string
}

// Group represents a group account, similar to Go's os/user.Group.
pub struct Group {
pub:
	gid  string
	name string
}
