module main

// This prelude is loaded in every v program compiled with -embed,
// in the main executable
import embed

const (
	no_warning_embed_is_used = embed.is_used
)
