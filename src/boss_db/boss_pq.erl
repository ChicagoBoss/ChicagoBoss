%% A priority queue based on gb_trees
-module(boss_pq).

-export([prune/4, delete_value/3, insert_value/3, move_value/4]).

prune(Function, State, {Size, TreeNode}, Now) ->
    {Acc1, Tree1, NumDeleted} = prune_expired_nodes(Function, State, TreeNode, Now),
    {Acc1, {Size - NumDeleted, Tree1}}.

prune_expired_nodes(Function, Acc, {K, V, S, L}, Now) when K > Now ->
    {Acc1, Tree1, NumDeleted} = prune_expired_nodes(Function, Acc, S, Now),
    {Acc1, {K, V, Tree1, L}, NumDeleted};
prune_expired_nodes(Function, Acc, {K, V, S, L}, Now) when K =< Now ->
    Acc1 = lists:foldr(Function, Acc, V),
    {Acc2, _, NumDeleted_S} = prune_expired_nodes(Function, Acc1, S, Now),
    {Acc3, Tree3, NumDeleted_L} = prune_expired_nodes(Function, Acc2, L, Now),
    {Acc3, Tree3, NumDeleted_S + NumDeleted_L + 1};
prune_expired_nodes(_Function, Acc, nil, _Now) ->
    {Acc, nil, 0}.

delete_value(Time, Value, Tree) ->
    case gb_trees:lookup(Time, Tree) of
        {value, [Value]} ->
            gb_trees:delete(Time, Tree);
        {value, Values} ->
            gb_trees:enter(Time, lists:delete(Value, Values), Tree)
    end.

insert_value(FutureTime, WatchId, Tree) ->
    NewVal = case gb_trees:lookup(FutureTime, Tree) of
        none -> [WatchId];
        {value, Watches} -> [WatchId|Watches]
    end,
    gb_trees:enter(FutureTime, NewVal, Tree).

move_value(OldTime, NewTime, Value, Tree) ->
    insert_value(NewTime, Value, delete_value(OldTime, Value, Tree)).
