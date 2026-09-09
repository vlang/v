struct App {}

type AliasApp = App

fn (app AliasApp) alias_method() int {
	return 1
}

fn (app App) parent_method() int {
	return 2
}

fn collect_method_names[T]() []string {
	mut names := []string{}
	$for method in T.methods {
		names << method.name
	}
	return names
}

fn test_comptime_for_alias_methods_includes_alias_and_parent_methods() {
	names := collect_method_names[AliasApp]()
	assert 'alias_method' in names
	assert 'parent_method' in names
}
