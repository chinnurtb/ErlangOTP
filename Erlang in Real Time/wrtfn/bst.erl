-module(bst).
-export([insert/2, prefix/1, list_to_bst/1]).

% A BST is composed of nodes {Left, Item, Right} 
% an empty tree is denoted by a nil.

% insert(Tree, Item) - Insert an item into a tree

insert(nil, Item) ->
  {nil, Item, nil};
insert({Left, Val, Right}, Item) ->
  if 
    Item =< Val ->
      {insert(Left, Item), Val, Right};
    Item > Val ->
      {Left, Val, insert(Right, Item)}
  end.

% prefix(Tree) - Prefix Search

prefix(nil) ->
  nil;
prefix({Left, Val, Right}) ->
  LR = prefix(Left),
  RR = prefix(Right),
  if
    LR =/= nil, RR =/= nil ->
      lists:append(LR, lists:append([Val], RR));
    LR =/= nil ->
      lists:append(LR, [Val]);
    RR =/= nil ->
      lists:append([Val], RR);
    true ->
      [Val]
  end.

% list_to_bst(List) - Convert a list to a bst

list_to_bst(L) ->
  list_to_bst(L, nil).

list_to_bst([], Tree) ->
  Tree;
list_to_bst([H|T], Tree) ->
  list_to_bst(T, insert(Tree, H)).
