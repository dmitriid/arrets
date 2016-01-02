%%%-----------------------------------------------------------------------------
%%% @author dmitriid
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 30. Dec 2015 11:50
%%%-----------------------------------------------------------------------------
-module(arrets).

%%_* Exports ===================================================================

%% Lifecycle
-export([ new/0
        , new/1
        , teardown/1
        ]).

%% Info
-export([ length/1
        , length/2
        ]).

%% Push/pop
%% Data manipulation. Destructive
-export([ push/2
        , push/3
        , pop/1
        , pop/2
        , pop_n/2
        , pop_n/3
        ]).

%%_* Types =====================================================================

-type handle() :: atom() | integer().

%%_* API =======================================================================

%_* Lifecycle ------------------------------------------------------------------
-spec new() -> integer().
new() -> ets:new('_', [set]).
new() -> ets:new('arrets$array', [set]).

-spec new(atom()) -> atom().
new(Handle) ->
  try ets:new(Handle, [set, named_table])
  catch _:_ -> ets:delete_all_objects(Handle)
  end,
  Handle.

-spec teardown(handle()) -> boolean().
teardown(Handle) ->
  ets:delete(Handle).

%_* Info -----------------------------------------------------------------------
-spec length(handle()) -> integer().
length(Handle) ->
  length(Handle, 0).

-spec length(handle(), integer()) -> integer().
length(Handle, Row) ->
  %% ets:fun2ms(fun({{X,_},_})  when X == Row -> true end).
  Spec = [{{{'$1', '_'}, '_'}, [{'==', '$1', {const, Row}}], [true]}],
  ets:select_count(Handle, Spec).

%_* Push/Pop -------------------------------------------------------------------
-spec push(handle(), term()) -> boolean().
push(Handle, Item) ->
  push(Handle, 0, Item).

-spec push(handle(), integer(), term()) -> boolean().
push(Handle, Row, Item) ->
  insert_at(Handle, Row, 0, Item).

-spec pop(handle()) -> term().
pop(Handle) ->
  pop(Handle, 0).

-spec pop(handle(), integer()) -> term().
pop(Handle, Row) ->
  [Item] = pop_n(Handle, Row, 1),
  Item.

-spec pop_n(handle(), integer()) -> [term()].
pop_n(Handle, N) ->
  pop_n(Handle, 0, N).

-spec pop_n(handle(), integer(), integer()) -> [term()].
pop_n(_Handle, _Row, 0) ->
  [];
pop_n(Handle, Row, N) ->
  Items = range(Handle, Row, 0, N),
  Count = length(Handle, Row),
  slice(Handle, Row, N, Count),
  Items.

-spec range(handle(), integer(), integer(), integer()) -> [term()].
range(Handle, Row, From0, Count) ->
  From = idx(Handle, Row, From0),

  %% fun({{R, X}, Y}) when R == Row, X >= From, X =< X + Count -> {X, Y} end
  Spec = [{{{'$1', '$2'}, '$3'},
           [{'==', '$1', {const, Row}},
            {'>=', '$2', {const, From}},
            {'<', '$2', {const, From + Count}}],
           [{{'$2', '$3'}}]}],
  Items = lists:sort(ets:select(Handle, Spec)),
  [I || {_, I} <- Items].

-spec insert_at(handle(), integer(), integer(), term()) -> boolean().
insert_at(Handle, Row, Index, Item) ->
  Idx = idx(Handle, Row, Index),

  %%  SpecAbove = ets:fun2ms(fun({{R, X}, Y}) when X >= Idx
  %%                                             , R =:= RowIdx -> {{R, X}, Y} end),
  SpecAbove = [{{{'$1', '$2'}, '$3'},
                [{'>=', '$2', {const, Idx}}, {'=:=', '$1', {const, Row}}],
                [{{{{'$1', '$2'}}, '$3'}}]}],

  ItemsAbove = ets:select(Handle, SpecAbove),

  NewItemsAbove = lists:foldl(fun({{R, ObjIdx}, Obj}, Acc) ->
                                [{{R, ObjIdx + 1}, Obj} | Acc]
                              end, [], ItemsAbove),
  lists:foreach(fun(Obj) ->
                  ets:delete_object(Handle, Obj)
                end, ItemsAbove),
  ets:insert(Handle, NewItemsAbove),
  ets:insert(Handle, {{Row, Idx}, Item}).

-spec idx(handle(), integer(), integer()) -> integer().
idx(Handle, Row, Idx0) ->
  case Idx0 < 0 of
    true -> length(Handle, Row) + Idx0;
    _    -> Idx0
  end.

%% Only leave the items between From and From + Length in the stack
%% Update indices accordingly
-spec slice(handle(), integer(), integer(), integer()) -> [term()].
slice(Handle, Row, From0, Length) ->
  Count = length(Handle, Row),
  From = case From0 < 0 of
           true -> Count + From0;
           _    -> From0
         end,
  %%  Spec = ets:fun2ms(fun({{R, X}, Y}) when X >= From
  %%                                        , X =< To
  %%                                        , R =:= Row-> {{R, X}, Y} end),
  Spec = [{{{'$1', '$2'}, '$3'},
           [{'>=', '$2', {const, From}},
            {'=<', '$2', {const, From + Length}},
            {'=:=', '$1', {const, Row}}],
           [{{{{'$1', '$2'}}, '$3'}}]}],

  Items = ets:select(Handle, Spec),
  NewItems = lists:foldl(fun({{R, Idx}, Obj}, Acc) ->
                           [{{R, Idx - From}, Obj} | Acc]
                         end, [], Items),
  ets:delete_all_objects(Handle),
  ets:insert(Handle, NewItems),
  [I || {_, I} <- lists:sort(NewItems)].

