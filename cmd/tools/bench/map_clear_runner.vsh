#!/usr/bin/env -S v -raw-vsh-tmp-prefix tmp

import os

const time_fmt = '"CPU: %Us\tReal: %es\tElapsed: %E\tRAM: %MKB\t%C"'
const flags = os.getenv('FLAGS')

unbuffer_stdout()

start := os.args[1] or { '1_000_000' }.int()
end := os.args[2] or { '10_000_000' }.int()
step := os.args[3] or { '500_000' }.int()

os.chdir(os.dir(@VEXE))!
vcmd := 'v ${flags} cmd/tools/bench/map_clear.v'

println('>> start: ${start} | end: ${end} | step: ${step} | workdir: "${os.getwd()}" | flags: "${flags}" | vcmd: "${vcmd}"')
assert os.system(vcmd) == 0

println('running...')
for i := start; i <= end; i += step {
	os.system('/usr/bin/time -f ${time_fmt} cmd/tools/bench/map_clear ${i}') == 0
}
