module main

import encoding.xml
import os

fn test_large_gtk_file() ! {
	// Note: If you are contributing to this project, you should download the
	// GIR file from https://raw.githubusercontent.com/gtk-rs/gir-files/master/Gtk-4.0.gir
	// and place it in the same directory as this file.
	path := os.join_path(os.dir(@FILE), 'Gtk-4.0.gir')
	if !os.exists(path) {
		println('Skipping test_large_gtk_file because file does not exist.')
		return
	}

	actual := xml.XMLDocument.from_file(path) or {
		return error('Failed to parse large GTK XML file')
	}

	mut valid := false
	for elm in actual.get_elements_by_tag('class') {
		if 'c:type' in elm.attributes && elm.attributes['c:type'] == 'GtkWindow' {
			assert elm.attributes['parent'] == 'Widget'
			assert elm.attributes['c:symbol-prefix'] == 'window'
			valid = true
		}
	}
	assert valid, 'GtkWindow class not found!'

	valid = false
	for elm in actual.get_elements_by_tag('constructor') {
		if 'c:identifier' in elm.attributes && elm.attributes['c:identifier'] == 'gtk_window_new' {
			assert elm == xml.XMLNode{
				name:       'constructor'
				attributes: {
					'name':         'new'
					'c:identifier': 'gtk_window_new'
				}
				children:   [
					xml.XMLNodeContents(xml.XMLNode{
						name:       'doc'
						attributes: {
							'xml:space': 'preserve'
						}
						children:   [
							xml.XMLNodeContents('Creates a new `GtkWindow`.

To get an undecorated window (no window borders), use
[method@Gtk.Window.set_decorated].

All top-level windows created by gtk_window_new() are stored
in an internal top-level window list. This list can be obtained
from [func@Gtk.Window.list_toplevels]. Due to GTK keeping a
reference to the window internally, gtk_window_new() does not
return a reference to the caller.

To delete a `GtkWindow`, call [method@Gtk.Window.destroy].'),
						]
					}),
					xml.XMLNodeContents(xml.XMLNode{
						name:       'return-value'
						attributes: {
							'transfer-ownership': 'none'
						}
						children:   [
							xml.XMLNodeContents(xml.XMLNode{
								name:       'doc'
								attributes: {
									'xml:space': 'preserve'
								}
								children:   [xml.XMLNodeContents('a new `GtkWindow`.')]
							}),
							xml.XMLNodeContents(xml.XMLNode{
								name:       'type'
								attributes: {
									'name':   'Widget'
									'c:type': 'GtkWidget*'
								}
								children:   []
							}),
						]
					}),
				]
			}
			valid = true
		}
	}
	assert valid, 'gtk_window_new constructor not found!'
}
