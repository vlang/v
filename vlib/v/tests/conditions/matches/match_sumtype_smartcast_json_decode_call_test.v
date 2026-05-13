import json

type Issue26826Value = Issue26826Nil | string

struct Issue26826Nil {}

struct Issue26826Session {
	id string
}

fn issue_26826_load(v Issue26826Value) !Issue26826Session {
	match v {
		string {
			loaded_session := json.decode(Issue26826Session, v)!
			return loaded_session
		}
		else {
			return error('bad')
		}
	}
}

fn test_match_sumtype_smartcast_json_decode_call() {
	session := issue_26826_load('{"id":"a"}') or {
		assert false, err.msg()
		return
	}
	assert session.id == 'a'
}
