-module('v.datatypes').
-export(['BloomFilter.free'/1, new_bloom_filter_fast/1, new_bloom_filter/3, 'BloomFilter.add'/2, 'BloomFilter.exists'/2, 'BloomFilter.union'/2, 'BloomFilter.intersection'/2, new_root_node/1, new_node/2, new_none_node/1, 'BSTreeNode.bind'/3, 'BSTree.insert'/2, 'BSTree.insert_helper'/3, 'BSTree.contains'/2, 'BSTree.contains_helper'/3, 'BSTree.remove'/2, 'BSTree.remove_helper'/4, 'BSTree.get_max_from_right'/2, 'BSTree.get_min_from_left'/2, 'BSTree.is_empty'/1, 'BSTree.in_order_traversal'/1, 'BSTree.in_order_traversal_helper'/3, 'BSTree.post_order_traversal'/1, 'BSTree.post_order_traversal_helper'/3, 'BSTree.pre_order_traversal'/1, 'BSTree.pre_order_traversal_helper'/3, 'BSTree.get_node'/3, 'BSTree.to_left'/2, 'BSTree.to_right'/2, 'BSTree.max'/1, 'BSTree.min'/1, 'DoublyLinkedList.is_empty'/1, 'DoublyLinkedList.len'/1, 'DoublyLinkedList.first'/1, 'DoublyLinkedList.last'/1, 'DoublyLinkedList.push_back'/2, 'DoublyLinkedList.push_front'/2, 'DoublyLinkedList.push_many'/3, 'DoublyLinkedList.pop_back'/1, 'DoublyLinkedList.pop_front'/1, 'DoublyLinkedList.insert'/3, 'DoublyLinkedList.insert_back'/3, 'DoublyLinkedList.insert_front'/3, 'DoublyLinkedList.node'/2, 'DoublyLinkedList.index'/2, 'DoublyLinkedList.delete'/2, 'DoublyLinkedList.str'/1, 'DoublyLinkedList.array'/1, 'DoublyLinkedList.next'/1, 'DoublyLinkedList.iterator'/1, 'DoublyLinkedList.back_iterator'/1, 'DoublyListIter.next'/1, 'DoublyListIterBack.next'/1, 'MinHeap.insert'/2, 'MinHeap.insert_many'/2, 'MinHeap.pop'/1, 'MinHeap.peek'/1, 'MinHeap.len'/1, 'MinHeap.left_child'/2, 'MinHeap.right_child'/2, 'MinHeap.parent'/2, 'LinkedList.is_empty'/1, 'LinkedList.len'/1, 'LinkedList.first'/1, 'LinkedList.last'/1, 'LinkedList.index'/2, 'LinkedList.push'/2, 'LinkedList.push_many'/2, 'LinkedList.pop'/1, 'LinkedList.shift'/1, 'LinkedList.insert'/3, 'LinkedList.prepend'/2, 'LinkedList.str'/1, 'LinkedList.array'/1, 'LinkedList.next'/1, 'LinkedList.iterator'/1, 'ListIter.next'/1, 'Quadtree.create'/8, 'Quadtree.insert'/2, 'Quadtree.retrieve'/2, 'Quadtree.clear'/1, 'Quadtree.get_nodes'/1, 'Quadtree.split'/1, 'Quadtree.get_index'/2, 'Queue.is_empty'/1, 'Queue.len'/1, 'Queue.peek'/1, 'Queue.last'/1, 'Queue.index'/2, 'Queue.push'/2, 'Queue.pop'/1, 'Queue.str'/1, 'Queue.array'/1, new_ringbuffer/1, 'RingBuffer.push'/2, 'RingBuffer.pop'/1, 'RingBuffer.push_many'/2, 'RingBuffer.pop_many'/2, 'RingBuffer.is_empty'/1, 'RingBuffer.is_full'/1, 'RingBuffer.capacity'/1, 'RingBuffer.clear'/1, 'RingBuffer.occupied'/1, 'RingBuffer.remaining'/1, 'RingBuffer.move_reader'/1, 'RingBuffer.move_writer'/1, 'Set.exists'/2, 'Set.add'/2, 'Set.remove'/2, 'Set.pick'/1, 'Set.rest'/1, 'Set.pop'/1, 'Set.clear'/1, 'Set.=='/2, 'Set.is_empty'/1, 'Set.size'/1, 'Set.copy'/1, 'Set.add_all'/2, 'Set.union'/2, 'Set.intersection'/2, 'Set.-'/2, 'Set.subset'/2, 'Set.array'/1, 'Stack.is_empty'/1, 'Stack.len'/1, 'Stack.peek'/1, 'Stack.push'/2, 'Stack.pop'/1, 'Stack.str'/1, 'Stack.array'/1, 'Direction__static__from'/1]).

'BloomFilter.free'(B) ->
    todo,
    ok.

new_bloom_filter_fast(Hash_func) ->
    #{hash_func => Hash_func, table_size => 16384, num_functions => 4, table => [], {vbeam, type} => 'BloomFilter'}.

new_bloom_filter(Hash_func, Table_size, Num_functions) ->
    case Table_size =< 0 of
        true -> error(<<"table_size should great that 0">>);
        false -> 
            case Num_functions < 1 orelse Num_functions > length(Salts) of
                true -> error(<<"num_functions should between 1~">>);
                false -> #{hash_func => Hash_func, table_size => Table_size, num_functions => Num_functions, table => [], {vbeam, type} => 'BloomFilter'}
                        end
                end.

'BloomFilter.add'(B, Element) ->
    Hash = 'unknown.hash_func'(B, Element),
    {Subhash, Index, Bb} = lists:foldl(fun(I, {SubhashAcc, IndexAcc, BbAcc}) ->
        SubhashOut = Hash ^ lists:nth(I + 1, Salts),
        IndexOut = todo,
        BbOut = todo,
        {SubhashOut, IndexOut, BbOut}
    end, {Subhash, Index, Bb}, lists:seq(0, maps:get(num_functions, B) - 1)),

'BloomFilter.exists'(B, Element) ->
    Hash = 'unknown.hash_func'(B, Element),
    {Subhash, Index, Bb, Bit} = lists:foldl(fun(I, {SubhashAcc, IndexAcc, BbAcc, BitAcc}) ->
        SubhashOut = Hash ^ lists:nth(I + 1, Salts),
        IndexOut = todo,
        BbOut = lists:nth(Index / 8 + 1, maps:get(table, B)),
        BitOut = 1 << (IndexAcc % 8),
        case Bb band Bit == 0 of
            true -> false;
            false -> ok
        end,
        {SubhashOut, IndexOut, BbOut, BitOut}
    end, {Subhash, Index, Bb, Bit}, lists:seq(0, maps:get(num_functions, B) - 1)),
    true.

'BloomFilter.union'(L, R) ->
    case maps:get(table_size, L) /= maps:get(table_size, R) orelse maps:get(num_functions, L) /= maps:get(num_functions, R) orelse maps:get(hash_func, L) /= maps:get(hash_func, R) of
        true -> error(<<"Both filters must be created with the same values.">>);
        false -> begin
            New_f = #{hash_func => maps:get(hash_func, L), table_size => maps:get(table_size, L), num_functions => maps:get(num_functions, L), table => [], {vbeam, type} => 'BloomFilter'},
            lists:foreach(fun(I) ->
                ok
            end, lists:seq(0, length(maps:get(table, L)) - 1)),
            New_f
        end
        end.

'BloomFilter.intersection'(L, R) ->
    case maps:get(table_size, L) /= maps:get(table_size, R) orelse maps:get(num_functions, L) /= maps:get(num_functions, R) orelse maps:get(hash_func, L) /= maps:get(hash_func, R) of
        true -> error(<<"Both filters must be created with the same values.">>);
        false -> begin
            New_f = #{hash_func => maps:get(hash_func, L), table_size => maps:get(table_size, L), num_functions => maps:get(num_functions, L), table => [], {vbeam, type} => 'BloomFilter'},
            lists:foreach(fun(I) ->
                ok
            end, lists:seq(0, length(maps:get(table, L)) - 1)),
            New_f
        end
        end.

new_root_node(Value) ->
    #{is_init => true, value => Value, parent => new_none_node(true), left => new_none_node(false), right => new_none_node(false), {vbeam, type} => 'BSTreeNode'}.

new_node(Parent, Value) ->
    #{is_init => true, value => Value, parent => Parent, {vbeam, type} => 'BSTreeNode'}.

new_none_node(Init) ->
    #{is_init => Init, {vbeam, type} => 'BSTreeNode'}.

'BSTreeNode.bind'(Node, To_bind, _left) ->
    To_bind = new_none_node(false),

'BSTree.insert'(Bst, Value) ->
    case 'BSTree.is_empty'(Bst) of
        true -> true;
        false -> 'BSTree.insert_helper'(Bst, maps:get(root, Bst), Value)
        end.

'BSTree.insert_helper'(Bst, Node, Value) ->
    case maps:get(value, Node) < Value of
        true -> begin
            case todo andalso maps:get(is_init, maps:get(right, Node)) of
                true -> 'BSTree.insert_helper'(Bst, maps:get(right, Node), Value);
                false -> ok
            end,
            true
        end;
        false -> case maps:get(value, Node) > Value of
            true -> begin
                case todo andalso maps:get(is_init, maps:get(left, Node)) of
                    true -> 'BSTree.insert_helper'(Bst, maps:get(left, Node), Value);
                    false -> ok
                end,
                true
            end;
            false -> ok
        end
    end,
    false.

'BSTree.contains'(Bst, Value) ->
    'BSTree.contains_helper'(Bst, maps:get(root, Bst), Value).

'BSTree.contains_helper'(Bst, Node, Value) ->
    case todo orelse not maps:get(is_init, Node) of
        true -> false;
        false -> begin
            case maps:get(value, Node) < Value of
                true -> 'BSTree.contains_helper'(Bst, maps:get(right, Node), Value);
                false -> case maps:get(value, Node) > Value of
                    true -> 'BSTree.contains_helper'(Bst, maps:get(left, Node), Value);
                    false -> ok
                end
            end,
            % TODO: unhandled stmt type
            ok            true
        end
        end.

'BSTree.remove'(Bst, Value) ->
    case 'BSTree.is_empty'(Bst) of
        true -> false;
        false -> 'BSTree.remove_helper'(Bst, maps:get(root, Bst), Value, false)
        end.

'BSTree.remove_helper'(Bst, Node, Value, Left) ->
    case not maps:get(is_init, Node) of
        true -> false;
        false -> 
            case maps:get(value, Node) == Value of
                true -> true;
                false -> 
                    case maps:get(value, Node) < Value of
                        true -> 'BSTree.remove_helper'(Bst, maps:get(right, Node), Value, false);
                        false -> 'BSTree.remove_helper'(Bst, maps:get(left, Node), Value, true)
                                        end
                                        end
                end.

'BSTree.get_max_from_right'(Bst, Node) ->
    case todo of
        true -> new_none_node(false);
        false -> begin
            Right_node = maps:get(right, Node),
            case todo orelse not maps:get(is_init, Right_node) of
                true -> Node;
                false -> 'BSTree.get_max_from_right'(Bst, Right_node)
                        end
        end
        end.

'BSTree.get_min_from_left'(Bst, Node) ->
    case todo of
        true -> new_none_node(false);
        false -> begin
            Left_node = maps:get(left, Node),
            case todo orelse not maps:get(is_init, Left_node) of
                true -> Node;
                false -> 'BSTree.get_min_from_left'(Bst, Left_node)
                        end
        end
        end.

'BSTree.is_empty'(Bst) ->
    todo.

'BSTree.in_order_traversal'(Bst) ->
    Result = [],
    'BSTree.in_order_traversal_helper'(Bst, maps:get(root, Bst), Result),
    Result.

'BSTree.in_order_traversal_helper'(Bst, Node, Result) ->
    case todo orelse not maps:get(is_init, Node) of
        true -> ok;
        false -> begin
            'BSTree.in_order_traversal_helper'(Bst, maps:get(left, Node), Result),
            Result bsl maps:get(value, Node),
            'BSTree.in_order_traversal_helper'(Bst, maps:get(right, Node), Result),
            ok
        end
        end.

'BSTree.post_order_traversal'(Bst) ->
    Result = [],
    'BSTree.post_order_traversal_helper'(Bst, maps:get(root, Bst), Result),
    Result.

'BSTree.post_order_traversal_helper'(Bst, Node, Result) ->
    case todo orelse not maps:get(is_init, Node) of
        true -> ok;
        false -> begin
            'BSTree.post_order_traversal_helper'(Bst, maps:get(left, Node), Result),
            'BSTree.post_order_traversal_helper'(Bst, maps:get(right, Node), Result),
            Result bsl maps:get(value, Node),
            ok
        end
        end.

'BSTree.pre_order_traversal'(Bst) ->
    Result = [],
    'BSTree.pre_order_traversal_helper'(Bst, maps:get(root, Bst), Result),
    Result.

'BSTree.pre_order_traversal_helper'(Bst, Node, Result) ->
    case todo orelse not maps:get(is_init, Node) of
        true -> ok;
        false -> begin
            Result bsl maps:get(value, Node),
            'BSTree.pre_order_traversal_helper'(Bst, maps:get(left, Node), Result),
            'BSTree.pre_order_traversal_helper'(Bst, maps:get(right, Node), Result),
            ok
        end
        end.

'BSTree.get_node'(Bst, Node, Value) ->
    case todo orelse not maps:get(is_init, Node) of
        true -> new_none_node(false);
        false -> 
            case maps:get(value, Node) == Value of
                true -> Node;
                false -> 
                    case maps:get(value, Node) < Value of
                        true -> 'BSTree.get_node'(Bst, maps:get(right, Node), Value);
                        false -> 'BSTree.get_node'(Bst, maps:get(left, Node), Value)
                                        end
                                        end
                end.

'BSTree.to_left'(Bst, Value) ->
    case 'BSTree.is_empty'(Bst) of
        true -> error(<<"BSTree is empty">>);
        false -> begin
            Node = 'BSTree.get_node'(Bst, maps:get(root, Bst), Value),
            case not maps:get(is_init, Node) of
                true -> error(<<"BSTree is not initialized">>);
                false -> begin
                    Left_node = maps:get(left, Node),
                    maps:get(value, Left_node)
                end
                        end
        end
        end.

'BSTree.to_right'(Bst, Value) ->
    case 'BSTree.is_empty'(Bst) of
        true -> error(<<"BSTree is empty">>);
        false -> begin
            Node = 'BSTree.get_node'(Bst, maps:get(root, Bst), Value),
            case not maps:get(is_init, Node) of
                true -> error(<<"BSTree is not initialized">>);
                false -> begin
                    Right_node = maps:get(right, Node),
                    maps:get(value, Right_node)
                end
                        end
        end
        end.

'BSTree.max'(Bst) ->
    case 'BSTree.is_empty'(Bst) of
        true -> error(<<"BSTree is empty">>);
        false -> begin
            Max = 'BSTree.get_max_from_right'(Bst, maps:get(root, Bst)),
            case not maps:get(is_init, Max) of
                true -> error(<<"BSTree is not initialized">>);
                false -> maps:get(value, Max)
                        end
        end
        end.

'BSTree.min'(Bst) ->
    case 'BSTree.is_empty'(Bst) of
        true -> error(<<"BSTree is empty">>);
        false -> begin
            Min = 'BSTree.get_min_from_left'(Bst, maps:get(root, Bst)),
            case not maps:get(is_init, Min) of
                true -> error(<<"BSTree is not initialized">>);
                false -> maps:get(value, Min)
                        end
        end
        end.

'DoublyLinkedList.is_empty'(List) ->
    length(List) == 0.

'DoublyLinkedList.len'(List) ->
    length(List).

'DoublyLinkedList.first'(List) ->
    case 'unknown.is_empty'(List) of
        true -> error(<<"Linked list is empty">>);
        false -> maps:get(data, maps:get(head, List))
        end.

'DoublyLinkedList.last'(List) ->
    case 'unknown.is_empty'(List) of
        true -> error(<<"Linked list is empty">>);
        false -> maps:get(data, maps:get(tail, List))
        end.

'DoublyLinkedList.push_back'(List, Item) ->
    New_node = #{data => Item, {vbeam, type} => 'DoublyListNode'},
    case 'unknown.is_empty'(List) of
        true -> begin
        end;
        false -> begin
        end
    end,

'DoublyLinkedList.push_front'(List, Item) ->
    New_node = #{data => Item, {vbeam, type} => 'DoublyListNode'},
    case 'unknown.is_empty'(List) of
        true -> begin
        end;
        false -> begin
        end
    end,

'DoublyLinkedList.push_many'(List, Elements, Direction) ->
    case Direction of
        front -> ok;
        back -> ok
    end.

'DoublyLinkedList.pop_back'(List) ->
    case 'unknown.is_empty'(List) of
        true -> error(<<"Linked list is empty">>);
        false -> begin
            % TODO: unhandled stmt type
            ok            case length(List) == 1 of
                true -> Value;
                false -> begin
                    Value = maps:get(data, maps:get(tail, List)),
                    Value
                end
                        end
        end
        end.

'DoublyLinkedList.pop_front'(List) ->
    case 'unknown.is_empty'(List) of
        true -> error(<<"Linked list is empty">>);
        false -> begin
            % TODO: unhandled stmt type
            ok            case length(List) == 1 of
                true -> Value;
                false -> begin
                    Value = maps:get(data, maps:get(head, List)),
                    Value
                end
                        end
        end
        end.

'DoublyLinkedList.insert'(List, Idx, Item) ->
    case Idx < 0 orelse Idx > length(List) of
        true -> error(<<"Index ", " out of bounds [0..", "]">>);
        false -> case Idx == 0 of
            true -> 'unknown.push_front'(List, Item);
            false -> case Idx == length(List) of
                true -> 'unknown.push_back'(List, Item);
                false -> case Idx =< length(List) / 2 of
                    true -> 'unknown.insert_front'(List, Idx, Item);
                    false -> 'unknown.insert_back'(List, Idx, Item)
                end
            end
        end
    end.

'DoublyLinkedList.insert_back'(List, Idx, Item) ->
    Node = 'unknown.node'(List, Idx),
    Prev = maps:get(prev, Node),
    New = #{data => Item, next => Node, prev => Prev, {vbeam, type} => 'DoublyListNode'},

'DoublyLinkedList.insert_front'(List, Idx, Item) ->
    Node = 'unknown.node'(List, Idx - 1),
    Next = maps:get(next, Node),
    New = #{data => Item, next => Next, prev => Node, {vbeam, type} => 'DoublyListNode'},

'DoublyLinkedList.node'(List, Idx) ->
    case Idx =< length(List) / 2 of
        true -> Node;
        false -> begin
            Node = maps:get(tail, List),
            % TODO: unhandled stmt type
            ok            Node
        end
        end.

'DoublyLinkedList.index'(List, Item) ->
    Hn = maps:get(head, List),
    Tn = maps:get(tail, List),
    % TODO: unhandled stmt type
    ok    error(<<"none">>).

'DoublyLinkedList.delete'(List, Idx) ->
    case Idx < 0 orelse Idx >= length(List) of
        true -> ok;
        false -> case Idx == 0 of
            true -> begin
                'unknown.pop_front'(List),
            end;
            false -> case Idx == length(List) - 1 of
                true -> begin
                    'unknown.pop_back'(List),
                end;
                false -> ok
            end
        end
    end,
    Node = 'unknown.node'(List, Idx),

'DoublyLinkedList.str'(List) ->
    'unknown.str'('unknown.array'(List)).

'DoublyLinkedList.array'(List) ->
    Result_array = [],
    Node = maps:get(head, List),
    % TODO: unhandled stmt type
    ok    Result_array.

'DoublyLinkedList.next'(List) ->
    case maps:get(iter, List) == todo of
        true -> 'unknown.next'(List);
        false -> 
            case maps:get(node, maps:get(iter, List)) == todo of
                true -> todo;
                false -> begin
                    % TODO: unhandled stmt type
                    ok                    maps:get(data, maps:get(node, maps:get(iter, List)))
                end
                        end
                end.

'DoublyLinkedList.iterator'(List) ->
    #{node => maps:get(head, List), {vbeam, type} => 'DoublyListIter'}.

'DoublyLinkedList.back_iterator'(List) ->
    #{node => maps:get(tail, List), {vbeam, type} => 'DoublyListIterBack'}.

'DoublyListIter.next'(Iter) ->
    case maps:get(node, Iter) == todo of
        true -> todo;
        false -> begin
            Res = maps:get(data, maps:get(node, Iter)),
            Res
        end
        end.

'DoublyListIterBack.next'(Iter) ->
    case maps:get(node, Iter) == todo of
        true -> todo;
        false -> begin
            Res = maps:get(data, maps:get(node, Iter)),
            Res
        end
        end.

'MinHeap.insert'(Heap, Item) ->
    maps:get(data, Heap) bsl Item,
    Child = length(maps:get(data, Heap)) - 1,
    Parent = 'unknown.parent'(Heap, Child),
    % TODO: unhandled stmt type
    ok
'MinHeap.insert_many'(Heap, Elements) ->
    lists:foreach(fun(V) ->
        'unknown.insert'(Heap, V),
        ok.
        ok
    end, Elements),

'MinHeap.pop'(Heap) ->
    case length(maps:get(data, Heap)) == 0 of
        true -> error(<<"Heap is empty">>);
        false -> case length(maps:get(data, Heap)) == 1 of
            true -> 'unknown.pop'(maps:get(data, Heap));
            false -> ok
        end
    end,
    Item = lists:nth(1, maps:get(data, Heap)),
    Parent = 0,
    Left = 'unknown.left_child'(Heap, Parent),
    Right = 'unknown.right_child'(Heap, Parent),
    % TODO: unhandled stmt type
    ok    Item.

'MinHeap.peek'(Heap) ->
    case length(maps:get(data, Heap)) == 0 of
        true -> error(<<"Heap is empty">>);
        false -> lists:nth(1, maps:get(data, Heap))
        end.

'MinHeap.len'(Heap) ->
    length(maps:get(data, Heap)).

'MinHeap.left_child'(Heap, Idx) ->
    Child = 2 * Idx + 1,
    case Child >= length(maps:get(data, Heap)) of
        true -> error(<<"none">>);
        false -> Child
        end.

'MinHeap.right_child'(Heap, Idx) ->
    Child = 2 * Idx + 2,
    case Child >= length(maps:get(data, Heap)) of
        true -> error(<<"none">>);
        false -> Child
        end.

'MinHeap.parent'(Heap, Idx) ->
    (Idx - 1) / 2.

'LinkedList.is_empty'(List) ->
    length(List) == 0.

'LinkedList.len'(List) ->
    length(List).

'LinkedList.first'(List) ->
    case not 'unknown.is_empty'(List) of
        true -> maps:get(data, maps:get(head, List));
        false -> error(<<"Linked list is empty">>)
    end.

'LinkedList.last'(List) ->
    case not 'unknown.is_empty'(List) of
        true -> maps:get(data, maps:get(tail, List));
        false -> error(<<"Linked list is empty">>)
    end.

'LinkedList.index'(List, Idx) ->
    case 'unknown.is_empty'(List) of
        true -> error(<<"Linked list is empty">>);
        false -> 
            case Idx < 0 orelse Idx >= length(List) of
                true -> error(<<"Index ", " out of bounds">>);
                false -> begin
                    Node = maps:get(head, List),
                    Node1 = lists:foldl(fun(_, NodeAcc) ->
                        NodeOut = maps:get(next, Node1),
                        NodeOut
                    end, Node, lists:seq(0, Idx - 1)),
                    maps:get(data, Node1)
                end
                        end
                end.

'LinkedList.push'(List, Item) ->
    New_node = #{data => Item, {vbeam, type} => 'ListNode'},
    case 'unknown.is_empty'(List) of
        true -> ok;
        false -> ok
    end,

'LinkedList.push_many'(List, Elements) ->
    lists:foreach(fun(V) ->
        'unknown.push'(List, V),
        ok.
        ok
    end, Elements),

'LinkedList.pop'(List) ->
    case 'unknown.is_empty'(List) of
        true -> error(<<"Linked list is empty">>);
        false -> begin
            Node = maps:get(head, List),
            To_return = todo,
            case isnil(maps:get(next, Node)) of
                true -> begin
                end;
                false -> begin
                    % TODO: unhandled stmt type
                    ok                    To_return1 = todo,
                end
            end,
            To_return1
        end
        end.

'LinkedList.shift'(List) ->
    case 'unknown.is_empty'(List) of
        true -> error(<<"Linked list is empty">>);
        false -> begin
            Node = maps:get(head, List),
            case 'unknown.is_empty'(List) of
                true -> ok;
                false -> ok
            end,
            maps:get(data, Node)
        end
    end.

'LinkedList.insert'(List, Idx, Item) ->
    case Idx < 0 orelse Idx > length(List) of
        true -> error(<<"Index ", " out of bounds [0..", "]">>);
        false -> 
            case Idx == length(List) of
                true -> ok;
                false -> begin
                    New_node = #{data => Item, {vbeam, type} => 'ListNode'},
                    case 'unknown.is_empty'(List) of
                        true -> begin
                        end;
                        false -> begin
                            Node = maps:get(head, List),
                            case Idx == 0 of
                                true -> begin
                                end;
                                false -> begin
                                    % TODO: unhandled stmt type
                                    ok                                    case isnil(maps:get(next, New_node)) of
                                        true -> ok;
                                        false -> ok
                                    end
                                end
                            end
                        end
                    end,
                end
                        end
                end.

'LinkedList.prepend'(List, Item) ->
    'unknown.insert'(List, 0, Item),
    ok.

'LinkedList.str'(List) ->
    'unknown.str'('unknown.array'(List)).

'LinkedList.array'(List) ->
    Result_array = [],
    Node = maps:get(head, List),
    % TODO: unhandled stmt type
    ok    Result_array.

'LinkedList.next'(List) ->
    case isnil(maps:get(iter, List)) of
        true -> 'unknown.next'(List);
        false -> 
            case isnil(maps:get(node, maps:get(iter, List))) of
                true -> todo;
                false -> begin
                    % TODO: unhandled stmt type
                    ok                    maps:get(data, maps:get(node, maps:get(iter, List)))
                end
                        end
                end.

'LinkedList.iterator'(List) ->
    #{node => maps:get(head, List), {vbeam, type} => 'ListIter'}.

'ListIter.next'(Iter) ->
    case isnil(maps:get(node, Iter)) of
        true -> todo;
        false -> begin
            Res = todo,
            Res
        end
        end.

'Quadtree.create'(Q, X, Y, Width, Height, Capacity, Depth, Level) ->
    #{perimeter => #{x => X, y => Y, width => Width, height => Height, {vbeam, type} => 'AABB'}, capacity => Capacity, depth => Depth, level => Level, particles => [], nodes => [], {vbeam, type} => 'Quadtree'}.

'Quadtree.insert'(Q, P) ->
    Indexes = [],
    case length(maps:get(nodes, Q)) > 0 of
        true -> ok;
        false -> begin
            maps:get(particles, Q) bsl P,
            case length(maps:get(particles, Q)) > maps:get(capacity, Q) andalso maps:get(level, Q) < maps:get(depth, Q) of
                true -> begin
                    case length(maps:get(nodes, Q)) == 0 of
                        true -> 'Quadtree.split'(Q);
                        false -> ok
                    end,
                    Indexes1 = lists:foldl(fun(J, IndexesAcc) ->
                        IndexesOut = 'Quadtree.get_index'(Q, lists:nth(J + 1, maps:get(particles, Q))),
                        lists:foreach(fun(K) ->
                            'Quadtree.insert'(lists:nth(lists:nth(K + 1, Indexes1) + 1, maps:get(nodes, Q)), lists:nth(J + 1, maps:get(particles, Q))),
                            ok
                            ok
                        end, lists:seq(0, length(Indexes1) - 1)),
                        IndexesOut
                    end, Indexes, lists:seq(0, length(maps:get(particles, Q)) - 1)),
                end;
                false -> ok
            end
        end
        end.

'Quadtree.retrieve'(Q, P) ->
    Indexes = 'Quadtree.get_index'(Q, P),
    Detected_particles = 'AABB.clone'(maps:get(particles, Q)),
    case length(maps:get(nodes, Q)) > 0 of
        true -> ok;
        false -> ok
    end,
    Detected_particles.

'Quadtree.clear'(Q) ->
    lists:foreach(fun(J) ->
        case length(maps:get(nodes, Q)) > 0 of
            true -> 'Quadtree.clear'(lists:nth(J + 1, maps:get(nodes, Q)));
            false -> ok
        end,
        ok
    end, lists:seq(0, length(maps:get(nodes, Q)) - 1)),

'Quadtree.get_nodes'(Q) ->
    Nodes = [],
    case length(maps:get(nodes, Q)) > 0 of
        true -> ok;
        false -> ok
    end,
    Nodes.

'Quadtree.split'(Q) ->
    case length(maps:get(nodes, Q)) == 4 of
        true -> ok;
        false -> begin
            Next_level = maps:get(level, Q) + 1,
            Child_width = maps:get(width, maps:get(perimeter, Q)) / 2,
            Child_height = maps:get(height, maps:get(perimeter, Q)) / 2,
            X = maps:get(x, maps:get(perimeter, Q)),
            Y = maps:get(y, maps:get(perimeter, Q)),
            maps:get(nodes, Q) bsl #{perimeter => #{x => X + Child_width, y => Y, width => Child_width, height => Child_height, {vbeam, type} => 'AABB'}, capacity => maps:get(capacity, Q), depth => maps:get(depth, Q), level => Next_level, particles => [], nodes => [], {vbeam, type} => 'Quadtree'},
            maps:get(nodes, Q) bsl #{perimeter => #{x => X, y => Y, width => Child_width, height => Child_height, {vbeam, type} => 'AABB'}, capacity => maps:get(capacity, Q), depth => maps:get(depth, Q), level => Next_level, particles => [], nodes => [], {vbeam, type} => 'Quadtree'},
            maps:get(nodes, Q) bsl #{perimeter => #{x => X, y => Y + Child_height, width => Child_width, height => Child_height, {vbeam, type} => 'AABB'}, capacity => maps:get(capacity, Q), depth => maps:get(depth, Q), level => Next_level, particles => [], nodes => [], {vbeam, type} => 'Quadtree'},
            maps:get(nodes, Q) bsl #{perimeter => #{x => X + Child_width, y => Y + Child_height, width => Child_width, height => Child_height, {vbeam, type} => 'AABB'}, capacity => maps:get(capacity, Q), depth => maps:get(depth, Q), level => Next_level, particles => [], nodes => [], {vbeam, type} => 'Quadtree'},
            ok
        end
        end.

'Quadtree.get_index'(Q, P) ->
    Indexes = [],
    V_midpoint = maps:get(x, maps:get(perimeter, Q)) + (maps:get(width, maps:get(perimeter, Q)) / 2),
    H_midpoint = maps:get(y, maps:get(perimeter, Q)) + (maps:get(height, maps:get(perimeter, Q)) / 2),
    North = maps:get(y, P) < H_midpoint,
    South = maps:get(y, P) + maps:get(height, P) > H_midpoint,
    West = maps:get(x, P) < V_midpoint,
    East = maps:get(x, P) + maps:get(width, P) > V_midpoint,
    case North andalso East of
        true -> Indexes bsl 0;
        false -> ok
    end,
    case North andalso West of
        true -> Indexes bsl 1;
        false -> ok
    end,
    case South andalso West of
        true -> Indexes bsl 2;
        false -> ok
    end,
    case South andalso East of
        true -> Indexes bsl 3;
        false -> ok
    end,
    Indexes.

'Queue.is_empty'(Queue) ->
    'unknown.is_empty'(maps:get(elements, Queue)).

'Queue.len'(Queue) ->
    'unknown.len'(maps:get(elements, Queue)).

'Queue.peek'(Queue) ->
    'unknown.first'(maps:get(elements, Queue)).

'Queue.last'(Queue) ->
    'unknown.last'(maps:get(elements, Queue)).

'Queue.index'(Queue, Idx) ->
    'unknown.index'(maps:get(elements, Queue), Idx).

'Queue.push'(Queue, Item) ->
    'unknown.push'(maps:get(elements, Queue), Item),
    ok.

'Queue.pop'(Queue) ->
    'unknown.shift'(maps:get(elements, Queue)).

'Queue.str'(Queue) ->
    'unknown.str'(maps:get(elements, Queue)).

'Queue.array'(Queue) ->
    'unknown.array'(maps:get(elements, Queue)).

new_ringbuffer(S) ->
    #{content => [], {vbeam, type} => 'RingBuffer'}.

'RingBuffer.push'(Rb, Element) ->
    case 'unknown.is_full'(Rb) of
        true -> error(<<"Buffer overflow">>);
        false -> begin
            'unknown.move_writer'(Rb)
        end
    end.

'RingBuffer.pop'(Rb) ->
    V = lists:nth(maps:get(reader, Rb) + 1, maps:get(content, Rb)),
    case 'unknown.is_empty'(Rb) of
        true -> error(<<"Buffer is empty">>);
        false -> 'unknown.move_reader'(Rb)
    end,
    V.

'RingBuffer.push_many'(Rb, Elements) ->
    lists:foreach(fun(V) ->
        'unknown.push'(Rb, V),
        ok.
        ok
    end, Elements),

'RingBuffer.pop_many'(Rb, N) ->
    Elements = [],
    lists:foreach(fun(_) ->
        Elements bsl 'unknown.pop'(Rb),
        ok
    end, lists:seq(0, N - 1)),
    Elements.

'RingBuffer.is_empty'(Rb) ->
    maps:get(reader, Rb) == maps:get(writer, Rb).

'RingBuffer.is_full'(Rb) ->
    case maps:get(writer, Rb) + 1 == maps:get(reader, Rb) of
        true -> true;
        false -> case maps:get(writer, Rb) == length(maps:get(content, Rb)) - 1 andalso maps:get(reader, Rb) == 0 of
            true -> true;
            false -> false
        end
    end.

'RingBuffer.capacity'(Rb) ->
    maps:get(cap, maps:get(content, Rb)) - 1.

'RingBuffer.clear'(Rb) ->
    Rb = #{content => [], {vbeam, type} => 'RingBuffer'},

'RingBuffer.occupied'(Rb) ->
    Reader = maps:get(reader, Rb),
    V = 0,
    case 'unknown.is_empty'(Rb) of
        true -> V;
        false -> begin
            % TODO: unhandled stmt type
            ok            V
        end
        end.

'RingBuffer.remaining'(Rb) ->
    'unknown.capacity'(Rb) - 'unknown.occupied'(Rb).

'RingBuffer.move_reader'(Rb) ->
    todo,
    case maps:get(reader, Rb) > length(maps:get(content, Rb)) - 1 of
        true -> ok;
        false -> ok
    end.

'RingBuffer.move_writer'(Rb) ->
    todo,
    case maps:get(writer, Rb) > length(maps:get(content, Rb)) - 1 of
        true -> ok;
        false -> ok
    end.

'Set.exists'(Set, Element) ->
    lists:member(Element, maps:get(elements, Set)).

'Set.add'(Set, Element) ->

'Set.remove'(Set, Element) ->
    'unknown.delete'(maps:get(elements, Set), Element),
    ok.

'Set.pick'(Set) ->
    lists:foreach(fun(_) ->
        K,
        ok
    end, maps:get(elements, Set)),
    error(<<"Set is empty.">>).

'Set.rest'(Set) ->
    Element = 'unknown.pick'(Set),
    'unknown.filter'('unknown.keys'(maps:get(elements, Set)), It /= Element).

'Set.pop'(Set) ->
    Element = 'unknown.pick'(Set),
    'unknown.delete'(maps:get(elements, Set), Element),
    Element.

'Set.clear'(Set) ->

'Set.=='(L, R) ->
    case length(maps:get(elements, L)) /= length(maps:get(elements, R)) of
        true -> false;
        false -> begin
            lists:foreach(fun(_) ->
                case (not lists:member(E, maps:get(elements, L))) of
                    true -> false;
                    false -> ok
                end,
                ok
            end, maps:get(elements, R)),
            true
        end
        end.

'Set.is_empty'(Set) ->
    'unknown.size'(Set) == 0.

'Set.size'(Set) ->
    length(maps:get(elements, Set)).

'Set.copy'(Set) ->
    #{elements => 'unknown.clone'(maps:get(elements, Set)), {vbeam, type} => 'Set'}.

'Set.add_all'(Set, Elements) ->
    lists:foreach(fun(Element) ->
        'unknown.add'(Set, Element),
        ok.
        ok
    end, Elements),

'Set.union'(L, R) ->
    Set = 'unknown.copy'(L),
    lists:foreach(fun(_) ->
        'unknown.add'(Set, E),
        ok
    end, maps:get(elements, R)),
    Set.

'Set.intersection'(L, R) ->
    Set = 'unknown.copy'(L),
    lists:foreach(fun(_) ->
        case not 'unknown.exists'(R, E) of
            true -> 'unknown.remove'(Set, E);
            false -> ok
        end,
        ok
    end, maps:get(elements, L)),
    lists:foreach(fun(_) ->
        case not 'unknown.exists'(L, E) of
            true -> 'unknown.remove'(Set, E);
            false -> ok
        end,
        ok
    end, maps:get(elements, R)),
    Set.

'Set.-'(L, R) ->
    Set = 'unknown.copy'(L),
    lists:foreach(fun(_) ->
        case 'unknown.exists'(R, E) of
            true -> 'unknown.remove'(Set, E);
            false -> ok
        end,
        ok
    end, maps:get(elements, L)),
    Set.

'Set.subset'(L, R) ->
    lists:foreach(fun(_) ->
        case (not lists:member(E, maps:get(elements, L))) of
            true -> false;
            false -> ok
        end,
        ok
    end, maps:get(elements, R)),
    true.

'Set.array'(L) ->
    'unknown.keys'(maps:get(elements, L)).

'Stack.is_empty'(Stack) ->
    length(maps:get(elements, Stack)) == 0.

'Stack.len'(Stack) ->
    length(maps:get(elements, Stack)).

'Stack.peek'(Stack) ->
    case not 'unknown.is_empty'(Stack) of
        true -> 'unknown.last'(maps:get(elements, Stack));
        false -> error(<<"Stack is empty">>)
    end.

'Stack.push'(Stack, Item) ->
    maps:get(elements, Stack) bsl Item,
    ok.

'Stack.pop'(Stack) ->
    case not 'unknown.is_empty'(Stack) of
        true -> 'unknown.pop'(maps:get(elements, Stack));
        false -> error(<<"Stack is empty">>)
    end.

'Stack.str'(Stack) ->
    'unknown.str'(maps:get(elements, Stack)).

'Stack.array'(Stack) ->
    maps:get(elements, Stack).

'Direction__static__from'(Input) ->
    error(<<"invalid value">>).
