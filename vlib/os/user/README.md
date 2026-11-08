# user

Module `user` provides user account information, similar to Go's `os/user`.

## API

- `user.User{ uid, gid, username, name, home_dir }`
- `user.current() !User`
- `user.lookup(name string) !User`
- `user.lookup_id(uid string) !User`