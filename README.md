# TokenIterators

[![Build Status](https://github.com/joshday/TokenIterators.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/joshday/TokenIterators.jl/actions/workflows/CI.yml?query=branch%3Amain)


- TokenIterators.jl provides easy syntax for writing lexers/tokenizers, with a few built-ins.
- It's super fast and easy to use.

> [!IMPORTANT]
> - This package is not designed for validating syntax, but splitting text into tokens using simple rules.


## Usage

```julia
using TokenIterators

t = JSONTokens(b"""{ "key": "value", "key2": -1e-7}""")

collect(t)
# 13-element Vector{Token{Base.CodeUnits{UInt8, String}, Symbol}}:
#  1:1 (1 byte) curly_open {
#  2:2 (1 byte) whitespace
#  3:7 (5 bytes) string \"key\"
#  8:8 (1 byte) colon :
#  9:9 (1 byte) whitespace
#  10:16 (7 bytes) string \"value\"
#  17:17 (1 byte) comma ,
#  18:18 (1 byte) whitespace
#  19:24 (6 bytes) string \"key2\"
#  25:25 (1 byte) colon :
#  26:26 (1 byte) whitespace
#  27:31 (5 bytes) number -1e-7
#  32:32 (1 byte) curly_close }
```


## `TokenIterator` and `Token`

A `TokenIterator` (abstract type) iterates over `Token`s (smallest meaningful unit of text/data) from any input `T::AbstractVector{UInt8}`.

Both `TokenIterator{T,K}` and `Token{T,K}` are parameterized by:

1. The input data type `T <: AbstractVector{UInt8}`
2. The type used to label the kind of token `K`.

A `Token` acts like a `view(data, i:j)`.  It's defined as:

```julia
struct Token{T <: AbstractVector{UInt8}, K, S} <: AbstractVector{UInt8}
    data::T
    kind::K
    i::Int  # starting index
    j::Int  # closing index
    state::S
end
```

> [!TIP]
> [StringViews.jl](https://github.com/JuliaStrings/StringViews.jl) (used heavily in this package) can provide lightweight AbstractString views of the token via `StringView(t)`.

## Defining Iterators with Rules


The iteration interface is based on finding the next Token based on the current one in the following steps:

1. Join all the candidate bytes of the next token (everything after the current token).
2. Identify the kind of the next token via a *starting pattern*.
3. Determine the last index of the next token via `findnext` on an *ending pattern*.

![](https://github.com/user-attachments/assets/da24efee-c1d8-4bf0-b6fe-ba2009798db1)


## An Example: JSONTokens

See `src/TokenIterators.jl` for more `TokenIterator` implementations.

```julia
struct JSONTokens{T} <: TokenIterator{T, Symbol, Nothing}
    data::T
end

function next(o::JSONTokens, t::Token)
    '{' .. t && return t(:curly_open)
    '}' .. t && return t(:curly_close)
    '[' .. t && return t(:square_open)
    ']' .. t && return t(:square_close)
    ':' .. t && return t(:colon)
    ',' .. t && return t(:comma)
    't' .. t && return t(:True, 4)
    'f' .. t && return t(:False, 5)
    'n' .. t && return t(:null, 4)
    ∈(b"\t\n\r ") .. t && return t(:whitespace, Before(!∈(b"\t\n\r ")))
    '"' .. t && return t(:string, u('"'))
    ∈(b"-0123456789") .. t && return t(:number, Before(!∈(b"-+eE.012345678")))
    return t(:unknown)
end
```

> [!NOTE]
> - The `x .. tok` syntax is shorthand for `startswith(tok, x)`
> - `t(kind, end_pattern, start_idx=2)` returns another Token with the given `kind` and ending position defined via `findnext(end_pattern, tok, start_idx)`

## Mini-DSL

### Starting Patterns

| Type | Example | Match? |
|------|---------|-------------|
`UInt8` | `0x20` (space) | `x == token[1]`
`Char` | `' '` | `x == StringView(token)[1])`
`Function` | `∈(b" \t\r\n")` | `f(token[1])`
`UseStringView` | `s(isspace)` | `f(StringView(token)[1])`
`AbstractString` | `"abc"` | `startswith(StringView(token), x)`
`AbstractVector{UInt8}` | `b"<a"` | `x == token[1:length(x)]`


### Ending Patterns

| Type | Example | Find Last Index |
|------|---------|-----------------|
`UInt8` | `0x20` (space) | `findnext(==(x), token, 2)`
`Char` | `' '` | `findnext(==(x), StringView(token), 2)`
`Before` | `Before("<")` | `findnext(==('<'), token, 2) - 1`
`Last` | `Last("-->")` | `last(findnext("-->", StringView(token), 2))`

## Tokenizer State

Any type can be used to store state for a TokenIterator.  Changing the state is done via the operator:

```julia
token | function_of_state
```

We provide several types for common state functions:

```julia
# Example: Adding two states to a Set{Symbol} or Vector{Symbol}
token | Push!(:state_to_add) | Push!(:another_state_to_add)

# Example: Removing a state
token | Pop!()

# Example: deleting a state from a Set{Symbol}
token | Delete!(:state_to_remove)
```

## Performance

TokenIterators is very fast with minimal allocations:

```julia
using TokenIterators, BenchmarkTools

versioninfo()
# Julia Version 1.11.6
# Commit 9615af0f269 (2025-07-09 12:58 UTC)
# Build Info:
#   Official https://julialang.org/ release
# Platform Info:
#   OS: macOS (arm64-apple-darwin24.0.0)
#   CPU: 10 × Apple M1 Pro
#   WORD_SIZE: 64
#   LLVM: libLLVM-16.0.6 (ORCJIT, apple-m1)
# Threads: 8 default, 0 interactive, 4 GC (on 8 virtual cores)
# Environment:
#   JULIA_NUM_THREADS = auto

data = read(download("https://github.com/plotly/plotly.js/raw/v3.0.1/dist/plot-schema.json"));

t = JSONTokens(data)
# JSONTokens (3.648 MiB)

@benchmark sum(t.kind == :string for t in $t)
# BenchmarkTools.Trial: 301 samples with 1 evaluation per sample.
#  Range (min … max):  16.554 ms … 16.931 ms  ┊ GC (min … max): 0.00% … 0.00%
#  Time  (median):     16.638 ms              ┊ GC (median):    0.00%
#  Time  (mean ± σ):   16.657 ms ± 72.185 μs  ┊ GC (mean ± σ):  0.00% ± 0.00%

#       ▁▄  ▂▅▃▇▂ ▄▄█ ▅
#   ▃▄▃▆██▄▇██████████████▇▅▃▃▁▃▁▄▄▁▃▄▆▃█▃▁▃▄▃▅▃▁▄▄▄▃▆▃▁▃▃▁▁▃▁▃ ▄
#   16.6 ms         Histogram: frequency by time        16.9 ms <

#  Memory estimate: 0 bytes, allocs estimate: 0.
```
