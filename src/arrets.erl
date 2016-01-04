%%%-----------------------------------------------------------------------------
%%% Copyright 2016 Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%-----------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%%% @doc A silly non-performant array implementation on top of ets.
%%%      See README for documentation
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
        , empty/1
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
new() ->
  Handle = ets:new('arrets$array', [set]),
  reset_row_idxs(Handle),
  Handle.

-spec new(atom()) -> atom().
new(Handle) ->
  try ets:new(Handle, [set, named_table])
  catch _:_ -> empty(Handle)
  end,
  reset_row_idxs(Handle),
  Handle.

-spec teardown(handle()) -> boolean().
teardown(Handle) ->
  remove_row_idxs(Handle),
  ets:delete(Handle).

%_* Info -----------------------------------------------------------------------
-spec length(handle()) -> integer().
length(Handle) ->
  length(Handle, 0).

-spec length(handle(), integer()) -> integer().
length(Handle, Row) ->
  row_idx(Handle, Row) + 1.

%_* Push/Pop -------------------------------------------------------------------
-spec push(handle(), term()) -> boolean().
push(Handle, Item) ->
  push(Handle, 0, Item).

-spec push(handle(), integer(), term()) -> boolean().
push(Handle, Row, Item) ->
  Idx = row_idx(Handle, Row),
  insert_at(Handle, Row, Idx + 1, Item).

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
  lists:reverse(splice(Handle, Row, -N, N)).

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
  ets:insert(Handle, {{Row, Idx}, Item}),
  update_row_idx(Handle, Row, +1).

-spec update_at(handle(), integer(), term()) -> boolean().
update_at(Handle, Index, Item) ->
  update_at(Handle, 0, Index, Item).

-spec update_at(handle(), integer(), integer(), term()) -> boolean().
update_at(Handle, Row, Index, Item) ->
  ets:insert(Handle, {{Row, Index}, Item}).

-spec empty(handle()) -> boolean().
empty(Handle) ->
  ets:delete_all_objects(Handle),
  reset_row_idxs(Handle).

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

  MaxIdx = length(Handle, Row) - 1,
  ActualCount = case From + Count > MaxIdx of
                  true -> MaxIdx - From;
                  false -> Count
                end,
  update_row_idx(Handle, Row, -ActualCount),
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

  %%  DeleteSpec = fun({{R, X}, Y}) when X >= From
  %%                                   , X =< To
  %%                                   , R =:= Row-> true end
  DeleteSpec = [{{{'$1', '$2'}, '$3'},
                 [{'>=', '$2', {const, From}},
                  {'<', '$2', {const, From + Count}},
                  {'=:=', '$1', {const, Row}}],
                 [true]}],
  ets:select_delete(Handle, DeleteSpec),

  %%  Spec = fun({{R, X}, Y}) when R =:= Row
  %%                             , X >= From + Count -> {{R, X}, Y} end
  ItemsAboveSpec = [{{{'$1', '$2'}, '$3'},
                 [{'=:=', '$1', Row}, {'>=', '$2', From + Count}],
                 [{{{{'$1', '$2'}}, '$3'}}]}],
  ItemsAbove = ets:select(Handle, ItemsAboveSpec),

  NewItems = [{{R, X - Count}, Y} || {{R, X}, Y} <- ItemsAbove],
  ets:insert(Handle, NewItems),

  MaxIdx = length(Handle, Row),
  ActualCount = case From + Count > MaxIdx of
                  true -> MaxIdx - From;
                  false -> Count
                end,

  update_row_idx(Handle, Row, -ActualCount),
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

%%_* Internal functions ========================================================

-spec reset_row_idxs(handle()) -> boolean().
reset_row_idxs(Handle) ->
  try ets:new('arrets$idx', [set, named_table])
  catch _:_ -> ok
  end,

  %% Spec = fun({{I, R}, _}) when R =:= Row -> {{I, R}, -1} end
  Spec = [{{{'$1', '$2'}, '_'},
           [{'=:=', '$1', {const, Handle}}],
           [{{{{'$1', '$2'}}, -1}}]}],

  Items = ets:select('arrets$idx', Spec),
  ets:insert('arrets$idx', Items).

-spec remove_row_idxs(handle()) -> boolean().
remove_row_idxs(Handle) ->
  %% Spec = fun({{I, R}, X}) when R =:= Row -> {{I, R}, X} end
  Spec = [{{{'$1', '$2'}, '$3'},
           [{'=:=', '$1', {const, Handle}}],
           [{{{{'$1', '$2'}}, -1}}]}],
  Items = ets:select('arrets$idx', Spec),
  lists:foreach(fun(I) -> ets:delete_object('arrets$idx', I) end, Items).


  -spec row_idx(handle(), integer()) -> integer().
row_idx(Handle, Row) ->
  %% IdxSpec = fun({{I, R}, X}) when I =:= 1, R =:= Row -> X end
  IdxSpec = [{{{'$1', '$2'}, '$3'},
              [{'=:=', '$1', Handle}, {'=:=', '$2', {const, Row}}],
              ['$3']}],

  case ets:select('arrets$idx', IdxSpec) of
    [] ->
      ets:insert('arrets$idx', {{Handle, Row}, -1}),
      -1;
    [Idx] ->
      Idx
  end.

-spec update_row_idx(handle(), integer(), integer()) -> integer().
update_row_idx(Handle, Row, ValueDiff) ->
  Idx = row_idx(Handle, Row),
  ets:insert('arrets$idx', {{Handle, Row}, Idx + ValueDiff}),
  Idx + ValueDiff.
