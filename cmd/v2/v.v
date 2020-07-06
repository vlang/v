module main

import cli
import os
import v.util

fn main() {
	mut v_cmd := cli.Command{
		name: 'v'
		description: 'V is a tool for managing V source code'
		version: util.full_v_version(false)
		execute: fn(cmd cli.Command) {
			if cmd.args.len == 0 {
				is_verbose := cmd.flags.get_bool_or('v', false)
				util.launch_tool(is_verbose, 'vrepl', cmd.args)
			} else {
				run_cmd_func(cmd)
			}
		}
	}

	v_cmd.add_flags(global_flags())

	v_cmd.add_command(build_cmd())
	v_cmd.add_command(run_cmd())
	v_cmd.add_command(init_cmd())

	// Development tool commands
	v_cmd.add_command(tool_cmd('repl', 'Run the V interactive language shell (REPL)'))
	v_cmd.add_command(tool_cmd('doc', 'Generate the documentation for a V module'))
	v_cmd.add_command(tool_cmd('vlib-docs', 'Generate and open the documentation of all the vlib modules'))
	v_cmd.add_command(tool_cmd('translate', 'Translate C code to V code (coming soon in 0.3)'))
	v_cmd.add_command(tool_cmd('bin2v', 'Converts a list of arbitrary files into a single v module file'))
	v_cmd.add_command(tool_cmd('build-examples', 'Build V examples'))
	v_cmd.add_command(tool_cmd('build-tools', 'Build V command tools'))
	v_cmd.add_command(tool_cmd('build-vbinaries', 'Build V binaries')) // ?

	// Test tool commands
	v_cmd.add_command(tool_cmd('test', 'Run all test files in the provided directory'))
	v_cmd.add_command(tool_cmd('test-compiler', 'Run all V compiler tests'))
	v_cmd.add_command(tool_cmd('test-fixed', 'Run all fixed V compiler tests'))
	v_cmd.add_command(tool_cmd('test-fmt', 'Run \'v fmt\' over all V files'))
	v_cmd.add_command(tool_cmd('fmt', 'Formats the provided V source file'))
	v_cmd.add_command(tool_cmd('vet', 'Checks intentation of provided V source file'))

	// Installation/Update commands
	v_cmd.add_command(tool_cmd('symlink', 'Create a symbolic link for V'))
	v_cmd.add_command(tool_cmd('up', 'Run the V self-updater'))
	v_cmd.add_command(tool_cmd('self', 'Run the V self-compiler (use -prod to optimize compilation)'))
	v_cmd.add_command(tool_cmd('setup-freetype', 'Install freetype'))

	// VPM tool commands
	v_cmd.add_command(tool_vpm_cmd('install', 'Install a module from VPM'))
	v_cmd.add_command(tool_vpm_cmd('remove', 'Remove a module that was installed from VPM'))
	v_cmd.add_command(tool_vpm_cmd('search', 'Search for a module from VPM'))
	v_cmd.add_command(tool_vpm_cmd('update', 'Update an installed module from VPM'))

	v_cmd.parse(os.args)
}

fn tool_cmd(name string, description string) cli.Command {
	return cli.Command{
		name: name
		description: description
		execute: fn(cmd cli.Command) {
			is_verbose := cmd.flags.get_bool_or('v', false)
			util.launch_tool(is_verbose, 'v'+cmd.name, cmd.args)
		}
	}
}

fn tool_vpm_cmd(name string, description string) cli.Command {
	return cli.Command{
		name: name
		description: description
		execute: fn(cmd cli.Command) {
			is_verbose := cmd.flags.get_bool_or('v', false)
			mut args := cmd.args
			args.prepend(cmd.name)

			util.launch_tool(is_verbose, 'vpm', args)
		}
	}
}

fn global_flags() []cli.Flag {
	mut flags := []cli.Flag{}
	flags << cli.Flag{
		flag: .bool
		global: true
		name: 'v'
		description: 'Enables verbose output'
	}
	flags << cli.Flag{
		flag: .bool
		global: true
		name: 'silent'
		description: 'Disables output'
	}
	return flags
}
