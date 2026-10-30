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
