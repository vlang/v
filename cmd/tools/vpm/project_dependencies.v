module main

import v.vmod

// project_dependencies includes development requirements only for the root project.
fn project_dependencies(manifest vmod.Manifest) []string {
	mut dependencies := manifest.dependencies.clone()
	dependencies << manifest.unknown['dev_dependencies'] or { []string{} }
	return dependencies
}

fn set_project_dependency(mut manifest vmod.Manifest, index int, requirement string) {
	if index < manifest.dependencies.len {
		manifest.dependencies[index] = requirement
		return
	}
	mut dev_dependencies := (manifest.unknown['dev_dependencies'] or { []string{} }).clone()
	dev_dependencies[index - manifest.dependencies.len] = requirement
	manifest.unknown['dev_dependencies'] = dev_dependencies
}
