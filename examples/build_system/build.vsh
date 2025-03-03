#!/usr/bin/env v

import build

// Define variables that can be used to change tasks in the buildscript
const app_name = 'hello'
const program_args = 'World'
const build_dir = 'target'

// Make the build context
mut context := build.context()

// Add a few simple tasks
context.task(name: 'doc', run: || system('echo "Nothing to do"'))
context.task(name: 'run', run: || system('v run . ${program_args}'))
context.task(name: 'build', run: || system('v .'))
context.task(name: 'build.prod', run: || system('v -prod -o ${app_name} .'))

// `_` to denote "private" tasks. Nothing stops the user from using it, but
// this tells them that the task is not meant to be used by them.
context.task(
	name: '_mkdirs'
	// The `help` field is displayed in `--tasks` to give a short summary of what the task does.
	help: 'Makes the directories used by the application'
	run:  fn () ! {
		if !exists(build_dir) {
			mkdir_all(build_dir) or { panic(err) }
		}
	}
)

// Add a more complex task
context.task(
	name:    'release'
	help:    'Build the app in production mode, generates documentation, and releases the build on Git'
	depends: ['_mkdirs', 'doc']
	run:     fn () ! {
		system('v -prod -o ${build_dir}/${app_name} .')
		// Pretend we are using Git to publish the built file as a release here.
	}
)

// Run the build context. This will iterate over os.args and each corresponding
// task, skipping any arguments that start with a hyphen (-)
context.run()
