module dl

// An interface value only carries its concrete type tag, so a program cannot
// dispatch methods of a type that is implemented in a shared library it loaded.
// Every V shared library therefore exports `_v_interface_exports`: a table of
// its own interface dispatchers, keyed by interface, method and type tag. The
// generated dispatcher of the loading program consults the tables registered
// here for tags that it does not know itself.

const interface_exports_symbol = '_v_interface_exports'

// InterfaceExport mirrors the entries of the table emitted by the C backend.
struct InterfaceExport {
	iface  &char = unsafe { nil }
	method &char = unsafe { nil }
	typ    u32
	fn_ptr voidptr
}

@[heap]
struct InterfaceExportTable {
mut:
	entries &InterfaceExport = unsafe { nil }
	refs    int
	next    &InterfaceExportTable = unsafe { nil }
}

__global g_dl_interface_exports = &InterfaceExportTable(unsafe { nil })

fn interface_exports_of(handle voidptr) &InterfaceExport {
	entries := sym(handle, interface_exports_symbol)
	if entries == unsafe { nil } {
		// Most libraries are not V libraries; do not leave their lookup failure
		// behind for a later `dlerror()` call by the user.
		dlerror()
	}
	return unsafe { &InterfaceExport(entries) }
}

fn register_interface_exports(handle voidptr) {
	entries := interface_exports_of(handle)
	if entries == unsafe { nil } {
		return
	}
	mut table := g_dl_interface_exports
	for table != unsafe { nil } {
		if table.entries == entries {
			table.refs++
			return
		}
		table = table.next
	}
	g_dl_interface_exports = &InterfaceExportTable{
		entries: entries
		refs:    1
		next:    g_dl_interface_exports
	}
}

fn unregister_interface_exports(handle voidptr) {
	entries := interface_exports_of(handle)
	if entries == unsafe { nil } {
		return
	}
	mut prev := &InterfaceExportTable(unsafe { nil })
	mut table := g_dl_interface_exports
	for table != unsafe { nil } {
		if table.entries == entries {
			table.refs--
			if table.refs > 0 {
				return
			}
			if prev == unsafe { nil } {
				g_dl_interface_exports = table.next
			} else {
				prev.next = table.next
			}
			return
		}
		prev = table
		table = table.next
	}
}

// interface_export_find returns the dispatcher that a loaded shared library
// exports for `method` of `iface` on values tagged `typ`, or nil. Generated
// interface dispatchers call it for type tags the program does not implement.
fn interface_export_find(iface &char, method &char, typ u32) voidptr {
	mut table := g_dl_interface_exports
	for table != unsafe { nil } {
		mut entry := table.entries
		for entry.iface != unsafe { nil } {
			if entry.typ == typ && C.strcmp(entry.iface, iface) == 0
				&& C.strcmp(entry.method, method) == 0 {
				return entry.fn_ptr
			}
			entry = unsafe { entry + 1 }
		}
		table = table.next
	}
	return unsafe { nil }
}
