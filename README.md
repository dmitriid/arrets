# arrets
A silly non-performant array implementation on top of ets.

For "benchmarks" scroll all the way down.

## Why?

I "needed" an array/stack with Javascript-like semantics: ability
to push, pop, randomly access and update elements, slice and splice.

## What?

A 2-D dynamically growing array implemented on top of a `set` table in ets.

## Caveats?

Unless a `Row` parameter is specified, all functions by default operate on
_Row 0_. In other words: if you do not specify a `Row`, your array will behave
as an array with one row only.

If something crashes, your data is gone, as nothing is done to protect the ets
table (yet?).

If several processes access the dictionary, strange things will happen (once
again: ets).

## API

__Note__: all API calls that accept `From, Count` also accept a negative `From`.
If `From < 0`, starting element will be counted from the end of the array.

### Lifecycle
- `new()`: get a random handle to a new array
- `new(Handle::atom())`: get a new array with your own handle
- `teardown(Handle)`: delete the array

### Info
- `length(Handle)`: length
- `length(Handle, Row)`: length of row

### Data manipulation. Destructive
- `push(Handle, Item)`: push. Equivalent to `insert_at(Handle, 0, Item)`
- `push(Handle, Row, Item)`: push to row. Equivalent to `insert_at(Handle, Row, 0, Item)`
- `pop(Handle)`: pop. Equivalent to `at(Handle, 0)`
- `pop(Handle, Row)`: pop from row. Equivalent to `at(Handle, Row, 0)`
- `pop_n(Handle, N)`: pop N items. Equivalent to `splice(Handle, 0, N)`
- `pop_n(Handle, Row, N)`: pop N items from Row. Equivalent to `splice(Handle, Row, 0, N)`
- `slice(Handle, From, Count)`: Only leave the items between `From `and `From + Count` in the stack
- `slice(Handle, Row, From, Count)`: Same for a row
- `splice(Handle, From, Count)`: Remove and return items between `From `and `From + Count` in the stack
- `splice(Handle, Row, From, Count)`: Same for a row
- `insert_at(Handle, Index, Item)`: Insert `Item` at `Index`
- `insert_at(Handle, Row, Index, Item)`: Insert `Item` at `Index` in `Row`
- `update_at(Handle, Index, Item)`: Update `Item` at `Index`
- `update_at(Handle, Row, Index, Item)`: Update `Item` at `Index` in `Row`
- `empty(Handle)`: Empty the entire array (remove all items from all rows)

### Data manipulation. Non-destructive

- `range(Handle, From, Count)`: Return items between `From` and `From + Count`. Leave array intact
- `range(Handle, Row, From, Count)`: Same for a row
- `at(Handle, Index)`: Alias for `nth`
- `at(Handle, Row, Index)`: Alias for `nth`
- `nth(Handle, Index)`: Return `Item` at `Index`. Leave array intact.
- `nth(Handle, Row, Index)`: Return `Item` at `Index` at `Row`. Leave array intact.

## "Benchmarks"

Non-scientific ballpark figures using a Macbook Pro and `statistics/1`.

```erlang
> F = fun(Fun) ->
        statistics(runtime),
        statistics(wall_clock),
        Fun(),
        {_, Time1} = statistics(runtime),
        {_, Time2} = statistics(wall_clock),
        U1 = Time1,
        U2 = Time2,
        io:format("Code time=~p (~p) milliseconds~n", [U1,U2])
     end.
> L = lists:seq(0, 10000).

> %% Create new
> F(fun() -> arrets:new(test) end).
Code time=0 (0) milliseconds

> %% Push 10 000 items
> F(fun() -> lists:foreach(fun(E) -> arrets:push(test, E) end, L) end).
Code time=2530 (2564) milliseconds

> %% Pop those back. Why popping 10 000 items takes so long? I dunno.
> F(fun() -> lists:foreach(fun(_) -> arrets:pop(test) end, L) end).
Code time=27400 (28842) milliseconds

> %% Repopulate
> lists:foreach(fun(E) -> arrets:push(test, E) end, L).

> %% Get back a random range
> F(fun() -> arrets:range(test, 1337, 1337) end).
Code time=0 (3) milliseconds

> %% Remove a random range
> F(fun() -> arrets:splice(test, 1337, 1337) end).
Code time=20 (13) milliseconds

> %% Leave a random range
> F(fun() -> arrets:slice(test, 1337, 1337) end).
Code time=0 (5) milliseconds

> %% Empty and repopulate
> arrets:empty(test), lists:foreach(fun(E) -> arrets:push(test, E) end, L).

> %% Access a random element
> F(fun() -> arrets:at(test, 1337) end).
Code time=0 (1) milliseconds

> %% Pop n elements
> F(fun() -> arrets:pop_n(test, 1337) end).
Code time=10 (8) milliseconds

> %% Empty array
> F(fun() -> arrets:empty(test) end).
Code time=0 (1) milliseconds

> %% Destroy array
> F(fun() -> arrets:teardown(test) end).
Code time=0 (0) milliseconds
```
