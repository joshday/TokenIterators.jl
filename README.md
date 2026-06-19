[![CI](https://github.com/joshday/TokenIterators.jl/actions/workflows/CI.yml/badge.svg)](https://github.com/joshday/TokenIterators.jl/actions/workflows/CI.yml)
[![Docs Build](https://github.com/joshday/TokenIterators.jl/actions/workflows/Docs.yml/badge.svg)](https://github.com/joshday/TokenIterators.jl/actions/workflows/Docs.yml)
[![Stable Docs](https://img.shields.io/badge/docs-stable-blue)](https://joshday.github.io/TokenIterators.jl/stable/)
[![Dev Docs](https://img.shields.io/badge/docs-dev-blue)](https://joshday.github.io/TokenIterators.jl/dev/)

# TokenIterators



- TokenIterators.jl provides easy syntax for writing lexers/tokenizers, with a few built-ins.
- It's super fast and easy to use.

> [!IMPORTANT]
> - This package is not designed for validating syntax, but splitting text into tokens using simple rules.


## Usage

```julia
using TokenIterators
using StringViews: StringView

t = JSONTokens.Tokenizer(codeunits("""{"key": "value", "n": -1e7}"""))

collect(t)
# 12-element Vector{Token{...}}:
#  1→1  (1 byte)   curly_open  {
#  2→6  (5 bytes)  key         "key"
#  7→7  (1 byte)   colon       :
#  8→8  (1 byte)   whitespace
#  9→15 (7 bytes)  string      "value"
#  ...
```


## `Token`

A `Token` is a tagged view into a byte buffer:

```julia
struct Token{T <: AbstractVector{UInt8}, K} <: AbstractVector{UInt8}
    data::T   # underlying byte buffer (shared, never copied)
    kind::K   # user-defined label (e.g. an enum value)
    i::Int    # first byte index (inclusive)
    j::Int    # last byte index (inclusive); j < i means empty
end
```

> [!TIP]
> [StringViews.jl](https://github.com/JuliaStrings/StringViews.jl) can provide zero-copy `AbstractString` views via `StringView(t)`.


## `@tokenizer` Macro

The `@tokenizer` macro generates a complete tokenizer module from a set of rules:

```julia
@tokenizer MyTokens begin
    kind = <rule>
    ...
end
```

This creates a module `MyTokens` containing:
- `MyTokens.Kinds` — an `@enum` with one variant per rule
- `MyTokens.Tokenizer{T}` — an iterable tokenizer struct
- `MyTokens.RULES` — a `Dict{Kinds, Any}` for introspection

```julia
t = MyTokens.Tokenizer(codeunits(input))
for tok in t
    println(tok.kind, ": ", StringView(tok))
end
```


## DSL

Each rule maps a token kind to a pattern expression:

```
kind = [state_guard -->] start_pat [.. stop_pat [>> anchor]] [=> new_state]
```

Operator precedence (high → low): `>>` > `..` > `-->` > `=>`

### Operators

| Operator | Meaning |
|----------|---------|
| `start .. stop` | Match tokens that start with `start`, scan forward to find `stop` |
| `(start .. stop) >> Anchor` | Adjust where `stop` lands: `Before`, `After`, `First`, or `Last` |
| `:guard --> rule` | Only apply this rule when `state == :guard` |
| `rule => :new_state` | After matching, transition to `:new_state` |

### Rule forms

```julia
# 1. Start-only: emit exactly width(start) bytes
whitespace = isspace

# 2. Start + stop: scan from start to stop (default anchor: Last)
string = '"' .. Unescaped('"')

# 3. Start + stop + anchor: adjust the stop position
whitespace = isspace .. !isspace >> Before    # stop before the non-space byte
number     = isdigit .. !isdigit >> Before

# 4. State transition: emit token, then change state
curly_open = '{' => :expect_key

# 5. State guard: only fire when in a specific state
key    = :expect_key   --> '"' .. Unescaped('"') => :expect_colon
string = :expect_value --> '"' .. Unescaped('"') => :in_object
```

### Start patterns

| Type | Example | Matches when… |
|------|---------|---------------|
| `UInt8` | `0x22` | `t[1] == x` |
| `Char` | `'"'` | `t[1] == x` (ASCII) or `StringView(t)[1] == x` (UTF-8) |
| `AbstractString` | `"<!--"` | token starts with this string |
| `AbstractVector{UInt8}` | `b"<!--"` | token starts with these bytes |
| `Function` | `isspace`, `∈(b"\t\n")` | `f(t[1])` returns `true` |

### Stop patterns

| Type | Example | Finds… |
|------|---------|--------|
| `Integer` | `4` | fixed width (emit exactly N bytes) |
| `UInt8` / `Char` | `'\n'` | next occurrence of that byte/char |
| `AbstractString` | `"-->"` | end of the next match of that string |
| `Function` | `!isspace` | next byte/char where `f` is true |
| `Unescaped(c)` | `Unescaped('"')` | next unescaped occurrence of `c` |

### Anchors

Anchors adjust which position within a stop-pattern match is used as the token end:

| Anchor | Token ends at… |
|--------|----------------|
| `Last` (default) | last byte of the match |
| `First` | first byte of the match |
| `Before` | byte just before the match |
| `After` | byte just after the match |

### State machine

When any rule uses `-->` (guard) or `=>` (transition), the macro automatically generates a **stateful** tokenizer (`state::Symbol`, initial state `:default`).

- Rules **without** a guard fire in any state.
- Rules **with** `guard --> rule` only fire when `state == guard`.
- Use `=>` to transition the state after a match.
- Rules are checked **in order** — put more specific (guarded) rules before general fallbacks.

```julia
@tokenizer JSONTokens begin
    curly_open   = '{'                                                    => :expect_key
    curly_close  = '}'                                                    => :default
    colon        = :expect_colon --> ':'                                  => :expect_value
    comma        = :in_object    --> ','                                  => :expect_key
    whitespace   = ∈(b"\t\n\r ") .. !∈(b"\t\n\r ") >> Before
    key          = :expect_key   --> '"' .. Unescaped('"')                => :expect_colon
    string       = :expect_value --> '"' .. Unescaped('"')                => :in_object
    number       = ∈(b"-0123456789") .. !∈(b"-+eE.0123456789") >> Before => :in_object
    unknown      = Returns(true)
end
```

## API Reference

### `Token` constructors

```julia
Token(data::AbstractVector{UInt8}, kind=nothing)  # i=1, j=0 (sentinel/empty)
Token(s::AbstractString, kind=nothing)
```

---

### `after(t::Token) -> Token`

Returns a new `Token` spanning all bytes in `t.data` that come **after** `t`
ends (`t.j + 1 : length(t.data)`).  The `kind` field is inherited from `t`.
Used internally by `AbstractTokenizer` to advance past the most-recently
emitted token.

---

### `emit(t, kind, start_pattern_width, stop_pattern, anchor=Last) -> Token`

Builds the next token from `t` (the remaining unprocessed input) in three steps:

1. Skip `start_pattern_width` bytes (those matched by the start pattern).
2. Call `findnext(stop_pattern, t, start_pattern_width + 1, anchor)` to find the end position.
3. Return `Token(t.data, kind, t.i, t.i + len - 1)`.

---

### `width(pattern) -> Int`

Returns the number of codeunits consumed by `pattern` when it matches at the
start of a token.  Used by `emit` to skip past the start pattern before
searching for the stop pattern.

| Pattern | Width |
|---------|-------|
| `UInt8` | `1` |
| `Char` | `ncodeunits(x)` |
| `AbstractVector{UInt8}` | `length(x)` |
| `AbstractString` | `ncodeunits(x)` |
| `Function` | `1` |
| `Tuple` | sum of element widths |

---

### `startswith(t::Token, pattern) -> Bool`

Returns `true` when the bytes at the beginning of `t` match `pattern`.

| Pattern | Matches when… |
|---------|---------------|
| `UInt8` | `t[1] == x` |
| `Char` | `t[1] == x` (ASCII fast path), or `StringView(t)[1] == x` |
| `AbstractVector{UInt8}` | `t[1:length(x)] == x` |
| `AbstractString` | same as `AbstractVector{UInt8}` via `codeunits` |
| `Function` | `f(t[1])` returns `true` |
| `Tuple` | each element matches in sequence (empty tuple always matches) |

---

### `findnext(pattern, t::Token, i) -> Union{Int, UnitRange{Int}, Nothing}`
### `findnext(pattern, t::Token, i, anchor::Anchor) -> Int`

Locates `pattern` inside `t` starting at index `i` (1-based, relative to `t`).

- The 3-argument form returns the raw result — an `Int`, a `UnitRange{Int}`, or `nothing`.
- The 4-argument form applies the `Anchor` to reduce the result to a single `Int`.

| Pattern | Behavior |
|---------|----------|
| `Function` | `findnext(f, view(t), i)` |
| `Integer` | returns `x` directly (fixed-width stop) |
| `UInt8` | next byte equal to `x` |
| `Char` | byte equality (ASCII) or `StringView` search (UTF-8) |
| `AbstractString` | searches `StringView(t)`, adjusted by `anchor` |
| `Unescaped(c)` | next occurrence of `c` not preceded by an escape byte |

`Anchor` controls which position within a range result is used:

| Anchor | Position |
|--------|----------|
| `First` | `rng[1]` |
| `Last` | `rng[end]` |
| `Before` | `rng[1] - 1` |
| `After` | `rng[end] + 1` |

---

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
