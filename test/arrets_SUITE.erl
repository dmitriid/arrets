%%%-----------------------------------------------------------------------------
%%% @doc Test SUITE for arrets
%%%
%%% @author Dmitrii Dimandt
%%% @copyright (C) 2013 Dmitrii Dimandt
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
        ]).

%% Data manipulation. Non-destructive
-export([ range/1
        , range_rows/1
        , range_negative/1
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
end_per_testcase(_, Config) ->
  Arrets = lkup(arrets, Config),
  arrets:teardown(Arrets),
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
  ?assertNotEqual(undefined, ets:info(Handle)),
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
                , ?WHENFAIL(
                    begin
                      ct:pal( "Failed. Elements: ~p~n Inserted: ~p"
                            , [Elements, ets:tab2list(Handle)])
                    end,
                    begin
                      populate(Handle, Elements),
                      Inserted = lists:reverse(lists:sort(ets:tab2list(Handle))),
                      InsertedItems = [I || {{_, _}, I} <- Inserted],

                      erlang:length(Elements) == erlang:length(Inserted) andalso
                      Elements == InsertedItems
                    end)
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
                    All = ets:tab2list(Handle),
                    {_, Result} = lists:foldl(
                       fun(Elements, {Row, ResultAcc}) ->
                         Actual = lists:reverse([I || {{R, _}, I} <- lists:sort(All), R == Row]),
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
                , ?WHENFAIL(
                    begin
                      ct:pal( "Failed. Elements: ~p~n"
                            , [Elements])
                    end,
                    begin
                      populate(Handle, Elements),
                      Result = lists:all(
                                 fun(Item) ->
                                   Popped = arrets:pop(Handle),
                                   Item == Popped
                                 end
                               , lists:reverse(Elements)),
                      Result andalso ets:info(Handle, size) == 0
                    end)
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
                    NFirst = lists:sublist(lists:reverse(Elements), N),
                    NPopped = arrets:pop_n(Handle, N),
                    Remaining = lists:sort(ets:tab2list(Handle)),
                    RemainingItems = [I || {_, I} <- Remaining],

                    Diff = NFirst -- NPopped,
                    RemDiff = lists:reverse(Elements -- NFirst),

                    erlang:length(NPopped) == N andalso
                    Diff == [] andalso
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
                        NFirst = lists:sublist(lists:reverse(Elements), N),
                        NPopped = arrets:pop_n(Handle, Row, N),
                        Remaining = lists:sort(ets:tab2list(Handle)),
                        RemainingItems = [I || {{R, _}, I} <- Remaining, R == Row],
                        Diff = NFirst -- NPopped,
                        {_, RemDiff0} = lists:foldl(fun(E, {NN, Acc}) ->
                          case NN >= N of
                            true -> {NN + 1, [E | Acc]};
                            false -> {NN + 1, Acc}
                          end
                        end, {0, []}, lists:reverse(Elements)),
                        RemDiff = lists:reverse(RemDiff0),

                        Res =         erlang:length(NPopped) == erlang:length(NFirst)
                              andalso Diff == []
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
                    Expected = lists:sublist( lists:reverse(Elements)
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
                      Expected = lists:sublist( lists:reverse(Elements)
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
                    Expected = lists:sublist( lists:reverse(Elements)
                                            , From + 1
                                            , Count),
                    Actual = [I || {_, I} <- lists:sort(ets:tab2list(Handle))],
                    Expected == Actual
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
                    Expected = lists:sublist( lists:reverse(Elements)
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
                      Expected = lists:sublist( lists:reverse(Elements)
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
                    Expected = lists:sublist( lists:reverse(Elements)
                                            , From + 1
                                            , Count),
                    Expected == Actual
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
  ets:delete_all_objects(Handle),
  lists:foreach( fun(Element) -> arrets:push(Handle, Element) end
               , Elements).

populate_rows(Handle, ListOfElements) ->
  ets:delete_all_objects(Handle),
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
