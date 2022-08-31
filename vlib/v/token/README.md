## Description:

`v.token` is a module providing the basic building blocks of the V
syntax - the tokens, as well as utilities for working with them.

## KeywordsMatcherTrie 
KeywordsMatcherTrie provides a faster way of determinining whether a given name is a reserved
word (belongs to a given set of previously known words `R`). It works by exploiting the fact,
that the set of reserved words is small, and the words short.

KeywordsMatcherTrie uses an ordered set of [tries](https://en.wikipedia.org/wiki/Trie),
one per each word length, that was added, so that rejecting that something is a reserved
word, can be done in constant time for words smaller or larger in length than all the
reserved ones.

After a word `w`, is confirmed by this initial check by length `n`, that it could belong
to a trie `Tn`, responsible for all known reserved words of that length, then `Tn` is used
to further verify or reject the word quickly. In order to do so, `Tn` prepares in advance
an array of all possible continuations (letters), at each index of the words `R`, after
any given prefix, belonging to `R`.

For example, if we have added the word `asm` to the trie T3, its tree (its nodes) may look
like this (note that the 0 pointers in children, mean that there was no word in `R`, that had
that corresponding letter at that specific index):
```
TrieNode 0:  a b c d e f g h i j k l m n o p q r s t u v w x y z ... |
| children:  1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ... | children[`a`] = 1 -> TrieNode 1
|   prefix so far: ''    | value: 0                                  |
|
TrieNode 1:  a b c d e f g h i j k l m n o p q r s t u v w x y z ... |
| children:  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 ... | children[`s`] = 2 -> TrieNode 2
|   prefix so far: 'a'   | value: 0                                  |
|
TrieNode 2:  a b c d e f g h i j k l m n o p q r s t u v w x y z ... |
| children:  0 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 ... | children[`m`] = 3 -> TrieNode 3
|   prefix so far: 'as'  | value: 0                                  | Note: `as` is a keyword with length 2,
|                                                                      but we are searching in T3 trie.
|
TrieNode 3:  a b c d e f g h i j k l m n o p q r s t u v w x y z ... |
| children:  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ... | all of children are 0
|   prefix so far: 'asm' | value: int(token.Kind.asm)                |
```

Matching any given `word` in the trie, after you have prepared it, is then simple:
just read each character of the `word`, and follow the corresponding pointer from
the `children` array (indexed by character). When the pointer is nil, there was NO
match, and the word is rejected, which happens very often, and early for most words
that are not in the set of the previously added reserved words. One significant 
benefit compared to just comparing the checked `word` against a linear list of all
known words, is that once you have found that a word is not a match at any given
level/trie node, then you know that it is not a match to *any* of them.

Note: benchmarking shows that it is ~300% to 400% faster, compared to just using 
`token.keywords[name]` on average, when there is a match, but it can be 17x faster
in the case, where there is a length mismatch. After changes to KeywordsMatcherTrie,
please do `v -prod run vlib/v/tests/bench/bench_compare_tokens.v` to verify, 
that there is no performance regression.
