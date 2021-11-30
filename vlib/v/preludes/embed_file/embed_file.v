module embed_file

// This prelude is loaded in every v program that uses `$embed_file`,
// in both the main executable, and in the shared library.
import v.embed_file

const (
	no_warning_embed_file_is_used = embed_file.is_used
)
