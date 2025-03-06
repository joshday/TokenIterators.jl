# Tokenizers

[![Build Status](https://github.com/joshday/Tokenizers.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/joshday/Tokenizers.jl/actions/workflows/CI.yml?query=branch%3Amain)


Tokenizers.jl provides tools for building tokenizers based on [finite-state machines](https://en.wikipedia.org/wiki/Finite-state_machine).  It implements a mini [DSL](https://en.wikipedia.org/wiki/Domain-specific_language) for identifying/emitting tokens.

## Usage

Using Tokenizers.jl is best seen through example.  Here is a full implementation of a JSON Tokenizer:

```julia
struct JSONTokens{T <: AbstractVector{UInt8}} <: Tokenizer{T, Symbol, Nothing}
    data::T
end

function next(o::JSONTokens, n::Token)
    '{' ≪ n && return n ≛ :curly_open
    '}' ≪ n && return n ≛ :curly_close
    '[' ≪ n && return n ≛ :square_open
    ']' ≪ n && return n ≛ :square_close
    ',' ≪ n && return n ≛ :comma
    ':' ≪ n && return n ≛ :colon
    't' ≪ n && return n ⋆ Symbol("true") ⋆ 4
    'f' ≪ n && return n ⋆ Symbol("false") ⋆ 5
    'n' ≪ n && return n ⋆ :null ⋆ 4
    '"' ≪ n && return n ⋆ :string ⋆ →((¬('\\'), '"'))
    ∈(b"-0123456789") ≪ n && return n ⋆ :number ⋆ ≺(∉(b"-+eE.0123456789"))
    ∈(b" \t\n\r") ≪ n && return n ⋆ :whitespace ⋆ ≺(∉(b" \t\n\r"))
    return n ≛ :unknown
end
```

Let's analyze the pieces of this code top-to-bottom:

1. `struct JSONTokens{T <: AbstractVector{UInt8}} <: Tokenizer{T, Symbol, Nothing}`

A `Tokenizer` is parameterized by the type of underlying data (`<: AbstractVector{UInt8}`), the kind of the token (`Symbol` in this case), and the kind of any additional state we may want the tokens to store (`Nothing` in this case).

2. `next(o::JSONTokens, n::Token)::Token`

This is the main step used within `Base.iterate`.  The `n::Token` argument is actually a view into the remaining data after the previous token, e.g. `data == @view previous_token.data[j+1:end]` (`n` is shorthand for "next", as in the data used to create the next token).

Similar to `Tokenizer`, a `Token` is parameterized by the underlying data `T`, type of token kind `K`, and type of state `S`.

```julia
struct Token{T <: AbstractVector{UInt8}, K, S} <: AbstractVector{UInt8}
    data::T
    kind::K
    i::Int
    j::Int
    state::S
end
```

3. `pattern ≪ n`

Does the data start with `pattern`?

4. `n ≛ :curly_open`

Set the token kind to `:curly_open` and its length to 1.

5. `n ⋆ :null ⋆ 4`

Set the token kind to `:null` and its length to 4.

6. `→((¬('\\'), '"'))`

Set the token length to the last (`→`) index of a sequence that matches any character other than `'\\'` followed by `'"'`.  This could also be written as `Last((Not('\\'), '"'))`

7. `∈(b"-0123456789") ≪ n`

Is `first_byte_of_n ∈ b"-0123456789"`?

8. `≺(∉(b"-+eE.0123456789"))`

The length of the token is the index *before* where the function `∉(b"-+eE.0123456789")(byte)` returns true.  This could also be written as `Before(x -> x ∉ b"-+eE.0123456789")`.

## The Mini-DSL

| Operator | Tab-Completion | Description |
|----------|----------------|-------------|
| `pattern ≪ tok` | `\ll` | Does `tok.data` begin with `pattern`?
| `tok ⋆ x::K` | `\star` | Set the token kind
| `tok ⋆ pattern` | `\star` | Set the token length according to `pattern`.
| `tok ≛ x` | `\starequal` | Set the token kind and set its length to `1`.

## Special Pattern Types

Any `pattern` used in the mini-DSL must satisfy:

> If `pattern ≪ tok` then `first(_findfirst(pattern, data, i))`

where `Tokenizers._findfirst` is used to avoid piracy with `Base.findfirst`.  Tokenizers.jl offers several types with shorthand notation for specifying patterns.

| Type | Symbol | Tab-Completion | Description |
|------|--------|----------------|-------------|
`Before` | `≺` | `\prec` | The byte before the pattern
`First` | `←` | `\leftarrow` | The first byte of the pattern match
`Last` | `→` | `\rightarrow` | The last byte of the pattern match
`Not` | `¬` | `\neg` | Match anything but a given pattern
