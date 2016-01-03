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

%% Data manipulation. Destructive
-export([ push/2
        , push/3
        , pop/1
        , pop/2
        , pop_n/2
        , pop_n/3
        , slice/3
        , slice/4
        , splice/3
        , splice/4
        , insert_at/3
        , insert_at/4
        , update_at/3
        , update_at/4
        ]).

%% Data manipulation. Non-destructive
-export([ range/3
        , range/4
        , at/2
        , at/3
        , nth/2
        , nth/3
        ]).

%%_* Types =====================================================================

-type handle() :: atom() | integer().

%%_* API =======================================================================

%_* Lifecycle ------------------------------------------------------------------
-spec new() -> integer().
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
  splice(Handle, Row, 0, N).

-spec range(handle(), integer(), integer()) -> [term()].
range(Handle, From, Count) ->
  range(Handle, 0, From, Count).

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

-spec insert_at(handle(), integer(), term()) -> boolean().
insert_at(Handle, Index, Item) ->
  insert_at(Handle, 0, Index, Item).

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

-spec update_at(handle(), integer(), term()) -> boolean().
update_at(Handle, Index, Item) ->
  update_at(Handle, 0, Index, Item).

-spec update_at(handle(), integer(), integer(), term()) -> boolean().
update_at(Handle, Row, Index, Item) ->
  ets:insert(Handle, {{Row, Index}, Item}).

-spec idx(handle(), integer(), integer()) -> integer().
idx(Handle, Row, Idx0) ->
  case Idx0 < 0 of
    true -> length(Handle, Row) + Idx0;
    _    -> Idx0
  end.

%% Only leave the items between From and From + Length in the array
-spec slice(handle(), integer(), integer()) -> [term()].
slice(Handle, From, Length) ->
  slice(Handle, 0, From, Length).

-spec slice(handle(), integer(), integer(), integer()) -> [term()].
slice(Handle, Row, From0, Count) ->
  From = case From0 < 0 of
           true ->
             ItemCount = length(Handle, Row),
             ItemCount + From0;
           _    ->
             From0
         end,
  %%  Spec = fun({{R, X}, Y}) when X >= From
  %%                             , X < To
  %%                             , R =:= Row-> {{R, X}, Y} end
  Spec = [{{{'$1', '$2'}, '$3'},
           [{'>=', '$2', {const, From}},
            {'<', '$2', {const, From + Count}},
            {'=:=', '$1', {const, Row}}],
           [{{{{'$1', '$2'}}, '$3'}}]}],

  Items = ets:select(Handle, Spec),
  NewItems = lists:foldl(fun({{R, Idx}, Obj}, Acc) ->
                           [{{R, Idx - From}, Obj} | Acc]
                         end, [], Items),

  %% OtherSpec = fun({{R, X}, Y}) when R /= Row -> {{R, X}, Y} end
  OtherSpec = [{{{'$1', '$2'}, '$3'},
                [{'/=', '$1', {const, Row}}],
                [{{{{'$1', '$2'}}, '$3'}}]}],

  OtherItems = ets:select(Handle, OtherSpec),
  ets:delete_all_objects(Handle),
  ets:insert(Handle, NewItems),
  ets:insert(Handle, OtherItems),
  [I || {_, I} <- lists:sort(NewItems)].

%% Remove and return items between From and From + Length in the array
-spec splice(handle(), integer(), integer()) -> [term()].
splice(Handle, From, Count) ->
  splice(Handle, 0, From, Count).

-spec splice(handle(), integer(), integer(), integer()) -> [term()].
splice(Handle, Row, From0, Count) ->
  From = idx(Handle, Row, From0),

  %%  Spec = fun({{R, X}, Y}) when X >= From
  %%                             , X =< To
  %%                             , R =:= Row-> {{R, X}, Y} end
  Spec = [{{{'$1', '$2'}, '$3'},
           [{'>=', '$2', {const, From}},
            {'<', '$2', {const, From + Count}},
            {'=:=', '$1', {const, Row}}],
           [{{{{'$1', '$2'}}, '$3'}}]}],

  Items = ets:select(Handle, Spec),

  %%  Spec = fun({{R, X}, Y}) when R =:= Row, (X < From orelse X >= To);
  %%                               R /= Row -> {{R, X}, Y} end
  OtherSpec = [{{{'$1', '$2'}, '$3'},
                [{'=:=', '$1', {const, Row}},
                 {'orelse', {'<', '$2', {const, From}}
                          , {'>=', '$2', {const, From + Count}}}],
                [{{{{'$1', '$2'}}, '$3'}}]},
               {{{'$1', '$2'}, '$3'},
                [{'/=', '$1', {const, Row}}],
                [{{{{'$1', '$2'}}, '$3'}}]}],
  OtherItems = ets:select(Handle, OtherSpec),

  NewItems = lists:foldl(fun({{R, X}, Y}, Acc) ->
    NewX = case R =:= Row andalso X >= From + Count of
             true -> X - Count;
             false -> X
           end,
    [{{R, NewX}, Y} | Acc]
  end, [], OtherItems),

  ets:delete_all_objects(Handle),
  ets:insert(Handle, NewItems),
  [I || {_, I} <- lists:sort(Items)].

-spec at(handle(), integer()) -> term().
at(Handle, N) ->
  nth(Handle, N).

-spec at(handle(), integer(), integer()) -> term().
at(Handle, Row, N) ->
  nth(Handle, Row, N).

-spec nth(handle(), integer()) -> term().
nth(Handle, N) ->
  nth(Handle, 0, N).

-spec nth(handle(), integer(), integer()) -> term().
nth(Handle, Row, N) ->
  %% Spec = fun({{R, X}, Y}) when R =:= Row, X == From -> Y end,
  Spec = [{{{'$1', '$2'}, '$3'},
           [{'=:=', '$1', {const, Row}}, {'==', '$2', {const, N}}],
           ['$3']}],
  [Item] = ets:select(Handle, Spec),
  Item.
