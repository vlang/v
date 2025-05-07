import arrays.diff

fn test_diff_array() {
	mut aa := ['hi', '1', '5', '3']
	mut bb := aa.clone()
	mut ctx := diff.diff(aa, bb)
	assert ctx.changes.len == 0
	bb.insert(2, 'max')
	// aa := ['hi', '1', '5', '3']
	// bb := ['hi', '1', 'max', '5', '3']
	ctx = diff.diff(aa, bb)
	assert ctx.changes == [
		diff.DiffChange{
			a:   2
			b:   2
			del: 0
			ins: 1
		},
	]

	bb.delete(4)
	// aa := ['hi', '1', '5', '3']
	// bb := ['hi', '1', 'max', '5']
	ctx = diff.diff(aa, bb)
	assert ctx.changes == [
		diff.DiffChange{
			a:   2
			b:   2
			del: 0
			ins: 1
		},
		diff.DiffChange{
			a:   3
			b:   4
			del: 1
			ins: 0
		},
	]

	str1 := ctx.generate_patch()
	assert str1 == 'hi
1
-5
-3
+max
+5
'

	str2 := ctx.generate_patch(block_header: true)
	assert str2 == '@@ -1,7 +1,7 @@
hi
1
-5
-3
+max
+5
'

	str3 := ctx.generate_patch(block_header: true, unified: 1)
	assert str3 == '@@ -2,4 +2,4 @@
1
-5
-3
+max
+5
'
	str4 := ctx.generate_patch(block_header: true, unified: 10)
	assert str4 == '@@ -1,14 +1,14 @@
hi
1
-5
-3
+max
+5
'

	str5 := ctx.generate_patch(block_header: true, unified: -1)
	assert str5 == '@@ -3,2 +3,2 @@
-5
-3
+max
+5
'
}

fn test_diff_runes() {
	mut aa := 'brown fox jumps over the lazy dog'.runes()
	mut bb := 'brwn faax junps ovver the lay daog'.runes()
	mut ctx := diff.diff(aa, bb)

	str1 := ctx.generate_patch()
	assert str1 == 'b
r
-o
w
n
 
f
-o
+a
+a
x
 
j
u
-m
+n
p
s
 
o
+v
v
e
r
 
l
a
-z
-y
- 
-d
+y
+ 
+d
+a
o
g
'
}
