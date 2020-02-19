module table

import os

// Once we have a module format we can read from module file instead
// this is not optimal
pub fn (table &Table) qualify_module(mod string, file_path string) string {
	for m in table.imports {
		println('TABLE IMPORT: $m')
		if m.contains('.') && m.contains(mod) {
			m_parts := m.split('.')
			m_path := m_parts.join(os.path_separator)
			println('IMPORT B: $m - $m_path')
			if mod == m_parts[m_parts.len - 1] && file_path.contains(m_path) {
				return m
			}
		}
	}
	return mod
}
