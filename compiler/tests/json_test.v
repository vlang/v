import json

struct Color {
    space string
    point string [raw]
}

fn test_raw_json_field() {
    color := json.decode(Color, '{"space": "YCbCr", "point": {"Y": 123}}') or {
        println('text')
        return
    }
    assert color.point == '{"Y":123}'
}