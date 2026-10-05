struct Quest {
	creatures []string
}

struct GateState {
	known  [2]map[int]bool
	quests map[int]Quest
}

fn differs_from_default[T](value T) bool {
	return value != T{}
}

fn test_struct_literal_map_equality() {
	assert GateState{} == GateState{}
	assert !differs_from_default(GateState{})
	assert differs_from_default(GateState{
		quests: {
			1: Quest{ creatures: ['guard'] }
		}
	})
	assert GateState{
		quests: {
			1: Quest{ creatures: ['guard'] }
		}
	} == GateState{
		quests: {
			1: Quest{ creatures: ['guard'] }
		}
	}
	assert GateState{
		quests: {
			1: Quest{ creatures: ['guard'] }
		}
	} != GateState{}
}
