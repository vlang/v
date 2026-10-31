# macos

The `macos` module provides typed access to the Objective-C runtime on macOS. It includes
Objective-C object, selector, rectangle, point, and range types together with typed message
senders.

Choose the message sender whose return and argument types exactly match the Objective-C method.
For example:

```v ignore
import macos

value := macos.msg_id_range(
	macos.get_class('NSValue'),
	'valueWithRange:',
	macos.range(2, 5),
)
result := macos.msg_range(value, 'rangeValue')
println('${result.location}, ${result.length}')
```

The typed senders hide the platform-specific `objc_msgSend` casts. Framework-specific methods
still require the corresponding `#flag darwin -framework ...` declaration in the importing
program.

The bridge supports the bundled TCC on older macOS SDKs whose runtime headers expose
an unguarded Blocks declaration. Blocks-based runtime APIs still require a C compiler
with Blocks support.
