pub struct Client {
pub:
	token   string
	intents int
mut:
	ready    bool
	sequence ?int
}

enum Intents {
	foo
}

pub type IntentsOrInt = Intents | int

@[params]
pub struct BotConfig {
pub:
	intents IntentsOrInt
}

pub fn bot(token string, config BotConfig) Client {
	return Client{
		token:   'Bot ${token}'
		intents: match config.intents {
			Intents { int(config.intents) }
			int { config.intents }
		}
	}
}

fn test_main() {
	assert true
}
