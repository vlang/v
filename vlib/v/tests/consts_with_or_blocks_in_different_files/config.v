module consts_with_or_blocks_in_different_files

import os

const a_const = os.config_dir() or { panic(err) } + '/a'
