import x.json2

const data = '
{
  "comments": {
    "26788945": {
      "id": "26788945",
      "message": "some comment 1"
    },
    "26788946": {
      "id": "26788946",
      "message": "some comment 2"
    },
    "26788947": {
      "id": "26788947",
      "message": "some comment 3"
    }
  },
  "comments2": {
  	"26788945": true,
	"26788946": false,
	"26788947": true
  },
  "comments3": {
  	"26788945": 1,
	"26788946": 2,
	"26788947": 3
  }
}
'

pub struct Comment {
	id      string
	message string
}

struct Comments {
mut:
	comments  map[string]Comment
	comments2 map[string]bool
	comments3 map[string]int
}

fn test_main() {
	mut root := json2.decode[Comments](data)!
	assert root.comments.len == 3
	assert root.comments['26788945']!.id == '26788945'
	assert root.comments['26788946']!.id == '26788946'
	assert root.comments['26788947']!.id == '26788947'

	assert root.comments2['26788945']! == true
	assert root.comments2['26788946']! == false
	assert root.comments2['26788947']! == true

	assert root.comments3['26788945']! == 1
	assert root.comments3['26788946']! == 2
	assert root.comments3['26788947']! == 3
}
