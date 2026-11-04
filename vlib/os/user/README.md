# user

Module `user` provides user account information, similar to Go's `os/user`.

## API

- `user.User{ uid, gid, username, name, home_dir }`
- `user.Group{ gid, name }`
- `user.current() !User`
- `user.lookup(name string) !User`
- `user.lookup_id(uid string) !User`
- `user.lookup_group(name string) !Group`
- `user.lookup_group_id(gid string) !Group`
