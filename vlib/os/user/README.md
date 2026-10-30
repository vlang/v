# user

`user` provides user account information, similar to Go's `os/user`.

```v
import os.user

u := user.current()!
println(u.username)
println(u.home_dir)
```
