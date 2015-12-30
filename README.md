# arrets
A silly non-performant array implementation on top of ets

## Why?

I "needed" an array with Javascript-like semantics: ability
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

- `new()`: get a random handle to a new array
- `new(Handle)`: get a new array with your own handle
- `teardown(Handle)`: delete te array
- `length(Handle)`: length
- `length(Handle, Row)`: length of row
- `push(Handle, Item)`: push. Equivalent to `insert_at(Handle, 0, Item)`
- `push(Handle, Row), Item`: push to row. Equivalent to `insert_at(Handle, Row, 0, Item)`
- `pop(Handle)`: pop. Equivalent to `at(Handle, 0)`
- `pop(handle, Row)`: pop from row. Equivalent to `at(Handle, Row, 0)`
- `pop_n(Handle, N)`: pop N items. Equivalent to `splice(Handle, 0, N)`
- `pop_n(Handle, Row, N)`: pop N items from Row. Equivalent to `splice(Handle, Row, 0, N)`

### Upcoming

- `range(Handle, From, Count)`: Return items between `From` and `From + Count`. Leave array intact
- `range(Handle, Row, From, Count)`: Same for a row
- `slice(Handle, From, Count)`: Only leave the items between `From `and `From + Count` in the stack
- `slice(Handle, Row, From, Count)`: Same for a row
- `splice(Handle, From, Count)`: Remove and return items between `From `and `From + Count` in the stack
- `splice(Handle, Row, From, Count)`: Same for a row
- `at(Handle, Index)`: Alias for `nth`
- `at(Handle, Row, Index)`: Alias for `nth`
- `nth(Handle, Index)`: Return `Item` at `Index`. Leave array intact.
- `nth(Handle, Row, Index)`: Return `Item` at `Index` at `Row`. Leave array intact.
- `insert_at(Handle, Index, Item)`: Insert `Item` at `Index`
- `insert_at(Handle, Row, Index, Item)`: Insert `Item` at `Index` in `Row`
- `update_at(Handle, Index, Item)`: Update `Item` at `Index`
- `update_at(Handle, Row, Index, Item)`: Update `Item` at `Index` in `Row`