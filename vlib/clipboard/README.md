## Description:

`clipboard` provides access to the platform's clipboard mechanism.
You can use it to read from the system clipboard, and write to it
from your applications.

## Examples:

```v
import clipboard

fn main() {
	mut c := clipboard.new()
	println(c.get_text())
}

```
