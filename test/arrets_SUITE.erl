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
-export([ new/1
        , new_with_handle/1
        , teardown/1
        , length/1
        , push/1
        , pop/1
        , pop_n/1
%%        , pop_and_push/1
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
                    ets:delete_all_objects(Handle),
                    lists:foreach( fun(Element) -> arrets:push(Handle, Element) end
                                 , Elements),
                    arrets:length(Handle) == erlang:length(Elements)
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
                      ets:delete_all_objects(Handle),
                      lists:foreach( fun(Element) -> arrets:push(Handle, Element) end
                                   , Elements),
                      Inserted = lists:reverse(lists:sort(ets:tab2list(Handle))),
                      InsertedItems = [I || {{_, _}, I} <- Inserted],


                      erlang:length(Elements) == erlang:length(Inserted) andalso
                      Elements == InsertedItems
                    end)
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
                      ets:delete_all_objects(Handle),
                      lists:foreach( fun(Element) -> arrets:push(Handle, Element) end
                                   , Elements),
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
                    ets:delete_all_objects(Handle),
                    lists:foreach( fun(Element) -> arrets:push(Handle, Element) end
                                 , Elements),
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