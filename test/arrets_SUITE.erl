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
%%% @doc Test SUITE for arrets
%%%-----------------------------------------------------------------------------
-module(arrets_SUITE).

%%_* Exports ===================================================================
%% CT callbacks
-export([ all/0
        , init_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Tests
%% Lifecycle
-export([ new/1
        , new_with_handle/1
        , teardown/1
        ]).

%% Info
-export([ length/1
        , length_rows/1
        ]).

%% Data manipulation. Destructive
-export([ push/1
        , push_rows/1
        , pop/1
        , pop_rows/1
        , pop_n/1
        , pop_n_rows/1
        , slice/1
        , slice_rows/1
        , slice_negative/1
        , splice/1
        , splice_rows/1
        , splice_negative/1
        , insert_at/1
        , insert_at_rows/1
        , update_at/1
        , update_at_rows/1
        , empty/1
        , empty_rows/1
        ]).

%% Data manipulation. Non-destructive
-export([ range/1
        , range_rows/1
        , range_negative/1
        , at_nth/1
        , at_nth_rows/1
        ]).


%%_* Includes ==================================================================
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

%%_* API =======================================================================
%%_* CT Callbacks ==============================================================
all() ->
  [   Name || {Name, _} <- ?MODULE:module_info(exports)
    , Name =/= all
    , Name =/= init_per_suite
    , Name =/= init_per_testcase
    , Name =/= end_per_testcase
    , Name =/= module_info
    , Name =/= test].

init_per_suite(Config) ->
  error_logger:tty(false),
  application:load(sasl),
  application:set_env(sasl, sasl_error_logger, false),
  application:set_env(sasl, errlog_type, error),
  {ok, _Apps} = application:ensure_all_started(sasl),
  Config.

init_per_testcase(Name, Config) ->
  doc(Name),
  do_init_per_testcase(Name, Config).

do_init_per_testcase(new, Config) ->
  Config;
do_init_per_testcase(new_with_handle, Config) ->
  Config;
do_init_per_testcase(_, Config) ->
  Arrets = arrets:new(),
  [{arrets, Arrets} | Config].

end_per_testcase(teardown, _) ->
  ok;
end_per_testcase(new, _) ->
  ok;
end_per_testcase(new_with_handle, _) ->
  ok;
end_per_testcase(_, Config) ->
  Handle = lkup(arrets, Config),
  arrets:teardown(Handle),
  ok.

%%_* Tests =====================================================================

%-------------------------------------------------------------------------------
new(doc) ->
  "Create an array with no handle. Make sure that corresponding ets table "
  "exists";
new(_) ->
  Handle = arrets:new(),
  ?assertNotEqual(undefined, ets:info(Handle)),
  ets:delete(Handle).

%-------------------------------------------------------------------------------
new_with_handle(doc) ->
  "Create an array with a handle. Make sure that corresponding ets table "
  "exists";
new_with_handle(_) ->
  Handle = arrets:new(handle),
  ?assertEqual(handle, ets:info(Handle)),
  ets:delete(Handle).

%-------------------------------------------------------------------------------
teardown(doc) ->
  "Create an array. Teardown an array. Make sure the corresponding ets table "
  "no longer exists";
teardown(Config) ->
  Handle = lkup(arrets, Config),
  arrets:teardown(Handle),
  ?assertEqual(undefined, ets:info(Handle)).

%-------------------------------------------------------------------------------
length(doc) ->
  "Length reports correct count of items";
length(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( Elements
                , elements()
                , begin
                    populate(Handle, Elements),
                    arrets:length(Handle) == erlang:length(Elements)
                  end
                ),
  ?assert(quickcheck(Prop)).

%-------------------------------------------------------------------------------
length_rows(doc) ->
  "Length reports correct count of items for each row";
length_rows(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( ListOfElements
                , list(elements())
                , begin
                    populate_rows(Handle, ListOfElements),
                    {_, Result} = lists:foldl(
                      fun(Elements, {Row, ResultAcc}) ->
                        Expected = erlang:length(Elements),
                        Actual = arrets:length(Handle, Row),
                        {Row + 1, (Expected == Actual) andalso ResultAcc}
                      end, {0, true}, ListOfElements),
                    Result
                  end
                ),
  ?assert(quickcheck(Prop)).
%-------------------------------------------------------------------------------
push(doc) ->
  "Pushing an element adds the element to the corresponding ets table at row 0 "
  "in the same order";
push(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( Elements
                , elements()
                , begin
                    populate(Handle, Elements),
                    Inserted = lists:sort(ets:tab2list(Handle)),
                    InsertedItems = [I || {{_, _}, I} <- scrub_idx(Inserted)],

                    erlang:length(Elements) == erlang:length(InsertedItems) andalso
                    Elements == InsertedItems
                  end
                ),
  ?assert(quickcheck(Prop)).

%-------------------------------------------------------------------------------
push_rows(doc) ->
  "Pushing an element adds the element to the corresponding ets table at row N "
  "in the same order";
push_rows(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( ListOfElements
                , list(elements())
                , begin
                    populate_rows(Handle, ListOfElements),
                    All = scrub_idx(ets:tab2list(Handle)),
                    {_, Result} = lists:foldl(
                       fun(Elements, {Row, ResultAcc}) ->
                         Actual = [I || {{R, _}, I} <- lists:sort(All), R == Row],
                         {Row + 1, (Elements == Actual) andalso ResultAcc}
                       end, {0, true}, ListOfElements),
                    Result
                  end
                ),
  ?assert(quickcheck(Prop)).

%-------------------------------------------------------------------------------
pop(doc) ->
  "Pop removes and returns the topmost item on the stack";
pop(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( Elements
                , elements()
                , begin
                    populate(Handle, Elements),
                    Result = lists:all(
                                 fun(Item) ->
                                   Item == arrets:pop(Handle)
                                 end
                               , lists:reverse(Elements)),
                    Result andalso ets:info(Handle, size) == 0
                  end
                ),
  ?assert(quickcheck(Prop)).

%-------------------------------------------------------------------------------
pop_rows(doc) ->
  "Pop removes and returns the topmost item on the stack in row N";
pop_rows(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( ListOfElements
                , list(elements())
                , begin
                    populate_rows(Handle, ListOfElements),
                    {_, Result} = lists:foldl(
                       fun(Elements, {Row, ResultAcc}) ->
                         LocalResult = lists:foldl(
                           fun(Expected, ResAcc) ->
                             Actual = arrets:pop(Handle, Row),
                             (Expected == Actual) andalso ResAcc
                           end, true, lists:reverse(Elements)
                         ),
                         {Row + 1, LocalResult andalso ResultAcc}
                       end, {0, true}, ListOfElements),
                    Result andalso ets:info(Handle, size) == 0
                  end
                ),
  ?assert(quickcheck(Prop)).

%-------------------------------------------------------------------------------
pop_n(doc) ->
  "Popping N items removes and returns top N items";
pop_n(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( {Elements, N}
                , ?LET( Els
                      , elements()
                      , {Els, ?SUCHTHAT( Count
                                       , nat()
                                       , Count =< erlang:length(Els)
                                       )
                        }
                      )
                , begin
                    populate(Handle, Elements),
                    Expected = case erlang:length(Elements) == 0 orelse N == 0 of
                                 true -> [];
                                 _ -> lists:sublist(lists:reverse(Elements), 1, N)
                               end,
                    Actual = arrets:pop_n(Handle, N),
                    Remaining = lists:sort(ets:tab2list(Handle)),
                    RemainingItems = [I || {_, I} <- Remaining],

                    {_, RemDiff} = lists:foldl(fun(E, {NN, Acc}) ->
                      case NN >= N of
                        true -> {NN + 1, [E | Acc]};
                        false -> {NN + 1, Acc}
                      end
                    end, {0, []}, lists:reverse(Elements)),

                    Expected == Actual andalso
                    RemDiff == RemainingItems
                  end
                ),
  ?assert(quickcheck(Prop)).

%-------------------------------------------------------------------------------
pop_n_rows(doc) ->
  "Popping N items removes and returns top N items from a row";
pop_n_rows(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( {ListOfElements, N}
                , {list(elements()), nat()}
                , begin
                    populate_rows(Handle, ListOfElements),
                    {_, Result} = lists:foldl(
                      fun(Elements, {Row, ResultAcc}) ->
                        Expected = case erlang:length(Elements) == 0 orelse N == 0 of
                                     true -> [];
                                     _ ->
                                       lists:sublist(lists:reverse(Elements), 1, N)
                                   end,
                        Actual = arrets:pop_n(Handle, Row, N),
                        Remaining = lists:sort(ets:tab2list(Handle)),
                        RemainingItems = [I || {{R, _}, I} <- Remaining, R == Row],
                        {_, RemDiff} = lists:foldl(fun(E, {NN, Acc}) ->
                          case NN >= N of
                            true -> {NN + 1, [E | Acc]};
                            false -> {NN + 1, Acc}
                          end
                        end, {0, []}, lists:reverse(Elements)),

                        Res = Actual == Expected
                              andalso RemDiff == RemainingItems,
                        {Row + 1, Res andalso ResultAcc}
                      end, {0, true}, ListOfElements),
                    Result
                  end
                ),
  ?assert(quickcheck(Prop)).

%-------------------------------------------------------------------------------
slice(doc) ->
  "Only leave the items between From and From + Count in the array";
slice(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( {Elements, From, Count}
                , ?LET( Els
                      , non_empty(elements())
                      , begin
                          Length = erlang:length(Els),
                          { Els
                          , random:uniform(Length) - 1
                          , random:uniform(Length)
                          }
                        end
                      )
                , begin
                    populate(Handle, Elements),
                    arrets:slice(Handle, From, Count),
                    Expected = lists:sublist( Elements
                                            , From + 1
                                            , Count),
                    Actual = [I || {_, I} <- lists:sort(ets:tab2list(Handle))],
                    Expected == Actual
                  end
                ),
  ?assert(quickcheck(Prop)).

%-------------------------------------------------------------------------------
slice_rows(doc) ->
  "Only leave the items between From and From + Count in the array "
  "in corresponding row";
slice_rows(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( ListOfElements
                , list(elements())
                , begin
                    populate_rows(Handle, ListOfElements),
                    {_, Result} = lists:foldl(fun(Elements, {Row, Result}) ->
                      Length = erlang:length(Elements),
                      {From, Count} = case Length of
                                        0 -> {0, random:uniform(10)};
                                        _ -> { random:uniform(Length) - 1
                                             , random:uniform(Length)}
                                      end,
                      arrets:slice(Handle, Row, From, Count),
                      Expected = lists:sublist( Elements
                                              , From + 1
                                              , Count),
                      Inserted = lists:sort(ets:tab2list(Handle)),
                      Actual = [I || {{R, _}, I} <- Inserted, R == Row],
                      {Row + 1, Expected == Actual andalso Result}
                    end, {0, true}, ListOfElements),
                    Result
                  end
                ),
  ?assert(quickcheck(Prop)).

%-------------------------------------------------------------------------------
slice_negative(doc) ->
  "If From < 0, only leave the items between (Length + From) and "
  "(Length + From) + Count in the array";
slice_negative(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( {Elements, From, NegativeFrom, Count}
                , ?LET( Els
                      , non_empty(elements())
                      , begin
                          Length = erlang:length(Els),
                          From = random:uniform(Length) - 1,
                          { Els
                          , From
                          , From - Length
                          , random:uniform(Length)
                          }
                        end
                      )
                , begin
                    populate(Handle, Elements),
                    arrets:slice(Handle, NegativeFrom, Count),
                    Expected = lists:sublist( Elements
                                            , From + 1
                                            , Count),
                    Actual = [I || {_, I} <- lists:sort(ets:tab2list(Handle))],
                    Expected == Actual
                  end
                ),
  ?assert(quickcheck(Prop)).

%-------------------------------------------------------------------------------
splice(doc) ->
  "Remove and return items between From and From + Length in the array";
splice(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( {Elements, From, Count}
                , ?LET( Els
                      , non_empty(elements())
                      , begin
                          Length = erlang:length(Els),
                          { Els
                          , random:uniform(Length) - 1
                          , random:uniform(Length)
                          }
                        end
                      )
                , begin
                    populate(Handle, Elements),
                    Expected = lists:sublist( Elements
                                            , From + 1
                                            , Count),
                    Actual = arrets:splice(Handle, From, Count),
                    Remaining = arrets:slice(Handle, 0, arrets:length(Handle)),
                    {_, ExpectedRemaining} = lists:foldl(fun(E, {Idx, Acc}) ->
                      case Idx < From orelse Idx >= From + Count of
                        true -> {Idx + 1, [E | Acc]};
                        false -> {Idx + 1, Acc}
                      end
                    end, {0, []}, Elements),
                    Expected == Actual andalso Remaining == lists:reverse(ExpectedRemaining)
                  end
                ),
  ?assert(quickcheck(Prop)).

%-------------------------------------------------------------------------------
splice_rows(doc) ->
  "Remove and return items between From and From + Length in a row";
splice_rows(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( ListOfElements
                , list(elements())
                , begin
                    populate_rows(Handle, ListOfElements),
                    {_, Result} = lists:foldl(fun(Elements, {Row, Result}) ->
                      Length = erlang:length(Elements),
                      {From, Count} = case Length of
                                        0 -> {0, random:uniform(10)};
                                        _ -> { random:uniform(Length) - 1
                                             , random:uniform(Length)}
                                      end,
                      Expected = lists:sublist( Elements
                                              , From + 1
                                              , Count),
                      Actual = arrets:splice(Handle, Row, From, Count),
                      Remaining = arrets:slice(Handle, Row, 0, arrets:length(Handle, Row)),
                      {_, ExpectedRemaining} = lists:foldl(fun(E, {Idx, Acc}) ->
                        case Idx < From orelse Idx >= From + Count of
                          true -> {Idx + 1, [E | Acc]};
                          false -> {Idx + 1, Acc}
                        end
                      end, {0, []}, Elements),
                      { Row + 1
                      , Expected == Actual andalso
                        Remaining == lists:reverse(ExpectedRemaining) andalso
                        Result
                      }
                    end, {0, true}, ListOfElements),
                    Result
                  end
                ),
  ?assert(quickcheck(Prop)).
%-------------------------------------------------------------------------------
splice_negative(doc) ->
  "If From < 0, only remove and return items between (Length + From) and "
  "(Length + From) + Count in the array";
splice_negative(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( {Elements, From, NegativeFrom, Count}
                , ?LET( Els
                      , non_empty(elements())
                      , begin
                          Length = erlang:length(Els),
                          From = random:uniform(Length) - 1,
                          { Els
                          , From
                          , From - Length
                          , random:uniform(Length)
                          }
                        end
                      )
                , begin
                    populate(Handle, Elements),
                    Expected = lists:sublist( Elements
                                            , From + 1
                                            , Count),
                    Actual = arrets:splice(Handle, NegativeFrom, Count),
                    Remaining = arrets:slice(Handle, 0, arrets:length(Handle)),
                    {_, ExpectedRemaining} = lists:foldl(fun(E, {Idx, Acc}) ->
                      case Idx < From orelse Idx >= From + Count of
                        true -> {Idx + 1, [E | Acc]};
                        false -> {Idx + 1, Acc}
                      end
                    end, {0, []}, Elements),
                    Expected == Actual andalso Remaining == lists:reverse(ExpectedRemaining)
                  end
                ),
  ?assert(quickcheck(Prop)).

%-------------------------------------------------------------------------------
insert_at(doc) ->
  "Insert a new item at position N";
insert_at(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( {Elements, N, Item}
                , ?LET( Els
                      , non_empty(elements())
                      , begin
                          Length = erlang:length(Els),
                          { Els
                          , random:uniform(Length) - 1
                          , int()
                          }
                        end
                      )
                , begin
                    populate(Handle, Elements),
                    arrets:insert_at(Handle, N, Item),
                    [Actual] = arrets:splice(Handle, N, 1),
                    Remaining = arrets:range(Handle, 0, arrets:length(Handle)),

                    Item == Actual andalso
                    Elements == Remaining
                  end
                ),
  ?assert(quickcheck(Prop)).

%-------------------------------------------------------------------------------
insert_at_rows(doc) ->
  "Insert a new item at position N in a row";
insert_at_rows(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( {ListOfElements, Item}
                , {list(non_empty(elements())), int()}
                , begin
                    populate_rows(Handle, ListOfElements),
                    {_, Result} = lists:foldl(fun(Elements, {Row, Result}) ->
                      N = random:uniform(erlang:length(Elements)) - 1,
                      arrets:insert_at(Handle, Row, N, Item),
                      [Actual] = arrets:splice(Handle, Row, N, 1),
                      Remaining = arrets:range(Handle, Row, 0, arrets:length(Handle, Row)),

                      { Row + 1
                      , Item == Actual andalso
                        Elements == Remaining andalso
                        Result
                      }
                    end, {0, true}, ListOfElements),
                    Result
                  end
                ),
  ?assert(quickcheck(Prop)).

%-------------------------------------------------------------------------------
update_at(doc) ->
  "Update an item at position N";
update_at(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( {Elements, N, Item}
                , ?LET( Els
                      , non_empty(elements())
                      , begin
                          Length = erlang:length(Els),
                          { Els
                          , random:uniform(Length) - 1
                          , int()
                          }
                        end
                      )
                , begin
                    populate(Handle, Elements),
                    arrets:update_at(Handle, N, Item),
                    {_, Result} = lists:foldl(fun(E, {Idx, Result}) ->
                      Actual = arrets:at(Handle, Idx),
                      Res = case Idx of
                        N -> Item == Actual;
                        _ -> E == Actual
                      end,
                      {Idx + 1, Res andalso Result}
                    end, {0, true}, Elements),
                    Result
                  end
                ),
  ?assert(quickcheck(Prop)).

%-------------------------------------------------------------------------------
update_at_rows(doc) ->
  "Update an item at position N in a row";
update_at_rows(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( {ListOfElements, Item}
                , {list(non_empty(elements())), int()}
                , begin
                    populate_rows(Handle, ListOfElements),
                    {_, Result} = lists:foldl(fun(Elements, {Row, ResultAcc}) ->
                      N = random:uniform(erlang:length(Elements)) - 1,
                      arrets:update_at(Handle, Row, N, Item),
                      {_, LocalResult} = lists:foldl(fun(E, {Idx, Result}) ->
                        Actual = arrets:at(Handle, Row, Idx),
                        Res = case Idx of
                          N -> Item == Actual;
                          _ -> E == Actual
                        end,
                        {Idx + 1, Res andalso Result}
                      end, {0, true}, Elements),

                      { Row + 1
                      , LocalResult andalso ResultAcc
                      }
                    end, {0, true}, ListOfElements),
                    Result
                  end
                ),
  ?assert(quickcheck(Prop)).

%-------------------------------------------------------------------------------
empty(doc) ->
  "Create and populate an array. Empty should empty the array and update "
  "indices";
empty(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( Elements
                , elements()
                , begin
                    populate(Handle, Elements),
                    arrets:empty(Handle),
                    BadIndices = [I || {{H, _}, I} <- ets:tab2list('arrets$idx')
                                     , H == Handle, I /= -1],
                    arrets:length(Handle) == 0 andalso BadIndices == []
                  end
                ),
  ?assert(quickcheck(Prop)).

%-------------------------------------------------------------------------------
empty_rows(doc) ->
  "Create and populate an array. Empty should empty the array and update "
  "indices for all rows";
empty_rows(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( ListOfElements
                , list(non_empty(elements()))
                , begin
                    populate_rows(Handle, ListOfElements),
                    arrets:empty(Handle),
                    lists:all(fun(Row) ->
                      BadIndices = [I || {{H, R}, I} <- ets:tab2list('arrets$idx')
                                       , H == Handle, R == Row, I /= -1],
                      arrets:length(Handle, Row) == 0 andalso
                      BadIndices == []
                    end, lists:seq(0, erlang:length(ListOfElements) - 1))
                  end
                ),
  ?assert(quickcheck(Prop)).

%-------------------------------------------------------------------------------
range(doc) ->
  "Return data in range";
range(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( {Elements, From, Count}
                , ?LET( Els
                      , elements()
                      , ?LET( From
                            , ?SUCHTHAT(X, nat(), X =< erlang:length(Els))
                            , {Els, From, From + erlang:length(Els)}
                            )
                      )
                , begin
                    populate(Handle, Elements),
                    InsertedCount = arrets:length(Handle),
                    Expected = lists:sublist( Elements
                                            , From + 1
                                            , Count),
                    Actual = arrets:range(Handle, From, Count),
                    RemainingCount = arrets:length(Handle),
                    Expected == Actual andalso
                    InsertedCount == RemainingCount
                  end
                ),
  ?assert(quickcheck(Prop)).

%-------------------------------------------------------------------------------
range_rows(doc) ->
  "Return data in range for a row";
range_rows(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( ListOfElements
                , list(elements())
                , begin
                    populate_rows(Handle, ListOfElements),

                    {_, Result} = lists:foldl(fun(Elements, {Row, Result}) ->
                      Length = erlang:length(Elements),
                      {From, Count} = case Length of
                                        0 -> {0, random:uniform(10)};
                                        _ -> { random:uniform(Length) - 1
                                             , random:uniform(Length)}
                                      end,
                      Actual = arrets:range(Handle, Row, From, Count),
                      Expected = lists:sublist( Elements
                                              , From + 1
                                              , Count),
                      InsertedLength = arrets:length(Handle, Row),

                      { Row + 1
                      , Expected == Actual andalso
                        InsertedLength == erlang:length(Elements) andalso
                        Result
                      }
                    end, {0, true}, ListOfElements),
                    Result
                  end
                ),
  ?assert(quickcheck(Prop)).

%-------------------------------------------------------------------------------
range_negative(doc) ->
  "If From < 0, Return data in range for with From = Count + From";
range_negative(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( {Elements, From, NegativeFrom, Count}
                , ?LET( Els
                      , non_empty(elements())
                      , begin
                          Length = erlang:length(Els),
                          From = random:uniform(Length) - 1,
                          { Els
                          , From
                          , From - Length
                          , random:uniform(Length)
                          }
                        end
                      )
                , begin
                    populate(Handle, Elements),
                    Actual = arrets:range(Handle, NegativeFrom, Count),
                    Expected = lists:sublist( Elements
                                            , From + 1
                                            , Count),
                    Expected == Actual
                  end
                ),
  ?assert(quickcheck(Prop)).

%-------------------------------------------------------------------------------
at_nth(doc) ->
  "Return item at position N. Array not affected";
at_nth(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( {Elements, N}
                , ?LET( Els
                      , non_empty(elements())
                      , { Els
                        , random:uniform(erlang:length(Els)) - 1
                        }
                      )
                , begin
                    populate(Handle, Elements),
                    ActualAt = arrets:at(Handle, N),
                    ActualNth = arrets:nth(Handle, N),
                    Expected = lists:nth(N + 1, Elements),
                    ActualAt == ActualNth andalso
                    Expected == ActualAt andalso
                    erlang:length(Elements) == arrets:length(Handle)
                  end
                ),
  ?assert(quickcheck(Prop)).

%-------------------------------------------------------------------------------
at_nth_rows(doc) ->
  "Return item at position N in a row. Array not affected";
at_nth_rows(Config) ->
  Handle = lkup(arrets, Config),
  Prop = ?FORALL( ListOfElements
                , list(non_empty(elements()))
                , begin
                    populate_rows(Handle, ListOfElements),

                    {_, Result} = lists:foldl                                                                                                                                                                        ( fun(Elements, {Row, Result}) ->
                      N = random:uniform(erlang:length(Elements)) - 1,
                      ActualAt = arrets:at(Handle, Row, N),
                      ActualNth = arrets:nth(Handle, Row, N),
                      Expected = lists:nth(N + 1, Elements),

                      { Row + 1
                      , ActualAt == ActualNth andalso
                        Expected == ActualAt andalso
                        erlang:length(Elements) == arrets:length(Handle, Row) andalso
                        Result
                      }
                                                                                                                                                                                                                  end, {0, true}, ListOfElements),
                    Result
                  end
                ),
  ?assert(quickcheck(Prop)).

%%_* EQC Generators ============================================================

elements() ->
  list(int()).

%%_* Helpers ===================================================================

lkup(What, Where) ->
  {_, Value} = lists:keyfind(What, 1, Where),
  Value.

doc(Name) ->
  Desc = try
      Doc = ?MODULE:Name(doc),
      iolist_to_binary(io_lib:format("~p: ~s", [Name, Doc]))
  catch
      _:_ -> iolist_to_binary(io_lib:format("~p", [Name]))
  end,
  ct:pal("~s", [Desc]).

populate(Handle, Elements) ->
  arrets:empty(Handle),
  lists:foreach( fun(Element) -> arrets:push(Handle, Element) end
               , Elements).

populate_rows(Handle, ListOfElements) ->
  arrets:empty(Handle),
  lists:foldl(
    fun(Elements, Row) ->
      lists:foreach(
        fun(Element) ->
          arrets:push(Handle, Row, Element)
        end, Elements
      ),
      Row + 1
    end, 0, ListOfElements
  ).

scrub_idx(L) ->
  [I || {{R, _}, _} = I <- L, R /= 'arrets$idx'].
