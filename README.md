# TokenIterators

[![Build Status](https://github.com/joshday/TokenIterators.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/joshday/TokenIterators.jl/actions/workflows/CI.yml?query=branch%3Amain)


TokenIterators.jl provides tools for building TokenIterators, with a few built-ins.  It's super fast and easy to use.

> [!IMPORTANT]
> This package is not designed for validating syntax.  Iterators can have odd behavior with ill-formmed data.


## Usage

```julia
using TokenIterators

t = JSONTokens(b"""{ "key": "value", "key2": -1e-7}""")

collect(t)
```

<img src="https://github.com/user-attachments/assets/2f225a34-35ea-4c2b-8389-53c00ae4de5d" alt="TokenIterators.jl example" height="300px">


## `TokenIterator` and `Token`

A `TokenIterator` (abstract type) iterates over `Token`s (smallest meaningful unit of text/data) from any input `T::AbstractVector{UInt8}`.

Both `TokenIterator{T,K,S}` and `Token{T,K,S}` are parameterized by the input data `T`, type of token kind `K`, and type of state `S`.

```julia
struct Token{T <: AbstractVector{UInt8}, K, S} <: AbstractVector{UInt8}
    data::T     # Input Data
    kind::K     # Kind of Token (e.g. Symbol or an Enum type)
    i::Int      # first index of token
    j::Int      # last index of token
    state::S    # Any additional state we wish to store
end
```

> [!TIP]
> [StringViews.jl](https://github.com/JuliaStrings/StringViews.jl) can be used to provide lightweight AbstractString views of the token.

## Rules

A `Token` can be created by a `Rule`, which is created with the syntax:

```julia
starting_pattern --> ending_pattern
```

where:

1. `starting_pattern` is used to identify the beginnign of a token.
2. `ending_pattern` is used to identify the end of a token.

> [!NOTE]
> A `Token` with indexes `i` and `j` will be created if `starting_pattern == data[i]` and `j == findnext(ending_pattern, data, i + 1)`.
> We use `TokenIterators.isfirst(starting_pattern, dataview)` and `TokenIterators._findnext(ending_pattern, dataview, i + 1)` (to avoid piracy with `Base.findnext`) to determine `i` and `j` of a `Token` where `dataview` is a view of the data *after* the previous token, e.g. `@view data[prev_token.j + 1:end]`.

## An Example: JSONTokens

- Here is the full implementation of `TokenIterators.JSONTokens`:

```julia
struct JSONTokens{T <: Data} <: TokenIterator{T, Symbol, Nothing}
    data::T
end

next(o::JSONTokens, n::Token) = @tryrules begin
    curly_open      = '{' --> 1
    curly_close     = '}' --> 1
    square_open     = '[' --> 1
    square_close    = ']' --> 1
    comma           = ',' --> 1
    colon           = ':' --> 1
    var"true"       = 't' --> 4
    var"false"      = 'f' --> 5
    var"null"       = 'n' --> 4
    string          = STRING            # '"'                -->  →((¬('\\'), '"'))
    number          = NUMBER            # ∈(b"-0123456789")  -->  ≺(∉(b"-+eE.0123456789"))
    whitespace      = ASCII_WHITESPACE  #  ∈(b" \t\r\n")     -->  ≺(∉(b" \t\r\n"))
end
```

- `next` is the core part of `Base.iterate(::TokenIterator)`.
- Here `n::Token` is a view into the data after the previous token, e.g. `Token(prev.data, prev.kind, prev.j + 1, length(prev.data), prev.state)`.
- The `@tryrules` macro will attempt to match the rules in order.  If a rule matches, the token is created and the function returns.

## Mini-DSL

Patterns in the DSL determine how the token is identified in `data::AbstractVector{UInt8}`.

### Starting Patterns

| Type | Example |Symbol | Tab-Completion | Description |
|------|---------|-------|----------------|-------------|
`UInt8` | `0x7b` |   |  | `x == data[i]`
`Char` | `'{'` |   |  | `UInt8(x) == data[i]`
`Function` | `∈(b" \t\r\n") ` |  |  | `f(data[1]) == true`
`Tuple` | `(a,b)` | | | `data` starts with sequence of patterns
`AbstractVector{UInt8}` | `b"null"` | | | `x == data[1:length(x)]`
`UseStringView` | `𝑠('😄')` | `𝑠` | `\its` | Use pattern with `data::StringView`

### Ending Patterns

| Type | Example |Symbol | Tab-Completion | Description |
|------|---------|-------|----------------|-------------|
`UInt8` | `0x7b` |   |  | `findnext(==(x), data, i + 1)`
`Char` | `'{'` |   |  | `findnext(==(UInt8(x)), data, i + 1)`
`Function` | `∈(b" \t\r\n") ` |  |  | `findnext(f, data, i + 1)`
`Int` |  `1` |   |  | `return x` (fixed length)
`Tuple` | `(a,b)` | | | return range in which indexes of `data` match sequence of patterns
`Last` | `Last((a,b))` | `→` | `\rightarrow` | return last index of match indexes
`First` | `First((a,b))` | `←` | `\leftarrow` | return first index of match indexes
`Before` | `Before(x -> x == a)` | `≺` | `\prec` | return index before the match index
`UseStringView` | `𝑠(isascii)` | `𝑠` | `\its` | Use pattern with `data::StringView`
`Not` | `¬('\\')` | `¬` | `\neg` | Matching anything but the given pattern

### Mini-DSL Examples

The `STRING`, `NUMBER`, and `ASCII_WHITESPACE` patterns from the JSONTokens example are interpreted as:

```julia
# STRING: Token begins with UInt8('"') and ends at the last index of a sequence that matches any character other than '\\' followed by '"'
'"' --> →((¬('\\'), '"'))

# NUMBER: Token begins with any byte in b"-0123456789" and ends at the index before the first byte that is not in b"-+eE.0123456789"
∈(b"-0123456789")  -->  ≺(∉(b"-+eE.0123456789"))

# ASCII_WHITESPACE: Token begins with any byte in b" \t\r\n" and ends at the index before the first byte that is not in b" \t\r\n"
∈(b" \t\r\n")  -->  ≺(∉(b" \t\r\n"))
```

## Performance

TokenIterators is very fast with minimal allocations:

```julia
using TokenIterators, BenchmarkTools

versioninfo()
# Julia Version 1.11.3
# Commit d63adeda50d (2025-01-21 19:42 UTC)
# Build Info:
#   Official https://julialang.org/ release
# Platform Info:
#   OS: macOS (arm64-apple-darwin24.0.0)
#   CPU: 10 × Apple M1 Pro
#   WORD_SIZE: 64
#   LLVM: libLLVM-16.0.6 (ORCJIT, apple-m1)
# Threads: 8 default, 0 interactive, 4 GC (on 8 virtual cores)

data = read(download("https://github.com/plotly/plotly.js/raw/v3.0.1/dist/plot-schema.json"));

Base.format_bytes(length(data))
# "3.648 MiB"

t = JSONTokens(data)
# JSONTokens (3824728-element Vector{UInt8})

@benchmark sum(t.kind == :string for t in $t)
# BenchmarkTools.Trial: 497 samples with 1 evaluation per sample.
#  Range (min … max):   9.834 ms …  31.878 ms  ┊ GC (min … max): 0.00% … 0.00%
#  Time  (median):     10.001 ms               ┊ GC (median):    0.00%
#  Time  (mean ± σ):   10.059 ms ± 993.690 μs  ┊ GC (mean ± σ):  0.00% ± 0.00%

#     ▁▁▄▄▂▂▄▁    ▃▄ ▂▄▅█▁▄▃ ▄▃▃▆▂▁▂▂
#   ▆▃████████▆▆▅▆██████████▆█████████▆▅▆▆▆▆▅▆▃▃▄▃▃▅▄▃▅▁▃▃▃▃▃▁▃▃ ▅
#   9.83 ms         Histogram: frequency by time         10.3 ms <

#  Memory estimate: 0 bytes, allocs estimate: 0.
```
