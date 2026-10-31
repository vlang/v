module fastc

import v.pref

fn fastc_native_source_context(sources []FastcSourceFile, prefs &pref.Preferences) FastcNativeSourceContext {
	return fastc_native_source_context_chunk(sources, prefs, 0, sources.len)
}
