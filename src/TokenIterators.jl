module TokenIterators

using StyledStrings, StringViews

import Base: startswith, findfirst, findnext, !, tail, |, +, -

export Token, State, Rule,
    JSONTokens, HTMLTokens, XMLTokens, DelimFileTokens, CharFunTokens,
    Before, Not, First, Last, ≺, ¬, ←, →, ⋆, ≛, ≪, 𝑠, -->, ..,
    next, is, isfirst, _findnext, init, transition

#-----------------------------------------------------------------------------# utils
const Data = AbstractVector{UInt8}
const SData = AbstractString

#-----------------------------------------------------------------------------# State
struct State{T} x::T end
State(; kw...) = State(NamedTuple(kw))

State(s::State) = s
Base.show(io::IO, o::State) = print(io, '(', state_repr(o.x), ')')

state_repr(x) = x
state_repr(x::NamedTuple) = all(x -> x isa Bool, x) ? join(keys(filter(identity, x)), ", ") : x

@inline isfirst(s::State, token) = s.x == token.state
@inline isfirst(s::State{<:NamedTuple}, token) = all(s.x[k] == token.state[k] for k in keys(s.x))
@inline isfirst(s::State{<:Function}, token) = s.x(token.state)

#-----------------------------------------------------------------------------# Token
struct Token{T <: Data, K, S} <: Data
    data::T
    kind::K
    i::Int
    j::Int
    state::S
end
Token(data, kind, state=nothing) = Token(data, kind, 1, 0, state)
Base.view(t::Token) = view(t.data, t.i:t.j)
StringViews.StringView(t::Token) = StringView(view(t))
Base.length(t::Token) = t.j - t.i + 1
Base.size(t::Token) = (length(t),)
Base.getindex(t::Token, i::Integer) = getindex(view(t), i)
function Base.show(io::IO, ::MIME"text/plain", t::Token)
    format(x::Int) = replace(string(x), r"(\d)(?=(\d{3})+(?!\d))" => s"\1_")
    rng = styled"{bright_black:$(format(t.i)):$(format(t.j)) ($(Base.format_bytes(length(t))))}"
    s = styled"$rng {bright_cyan:$(t.kind)} "
    n = displaysize(io)[2] - length(s) - 1
    _s2 = styled"{inverse:{bright_cyan:$(escape_string(StringView(t)))}}"
    s2 = _s2[1:min(n, end)] * (length(_s2) > n ? styled"{bright_cyan:…}" : "")
    print(io, s, s2)
end
Base.show(io::IO, t::Token) = show(io, MIME("text/plain"), t)

after(t::Token) = Token(t.data, t.kind, t.j + 1, length(t.data), t.state)

|(t::Token{T,K,S}, k::K) where {T, K, S} = Token(t.data, k, t.i, t.j, t.state)
# |(t::Token{T,K,S}, s::State) where {T, K, S} = Token(t.data, t.kind, t.i, t.j, transition(s, t))
|(t::Token{T,K,S}, x) where {T, K, S} = Token(t.data, t.kind, t.i, t.i - 1 + findnextind(x, t, 2), t.state)


#-----------------------------------------------------------------------------# tizer
abstract type Tokenizer{T, K, S} end
Base.show(io::IO, o::Tokenizer) = print(io, typeof(o).name.name, " ($(summary(o.data)))")

Base.IteratorSize(::Type{T}) where {T <: Tokenizer} = Base.SizeUnknown()
Base.eltype(::Type{Tok}) where {T, K, S, Tok <: Tokenizer{T, K, S}} = Token{T, K, S}
Base.isdone(o::Tokenizer, prev::Token) = prev.j == length(prev.data)

function Base.iterate(o::Tokenizer, prev = init(o))
    Base.isdone(o, prev) && return nothing
    n = next(o, after(prev))
    return n, n
end

init(t::Tokenizer{T,K,S}) where {T,K,S} = Token(t.data, init_kind(t), 1, 0, init_state(t))

init_kind(t::Tokenizer{T,K,S}) where {T,K,S} = init(K)
init_state(t::Tokenizer{T,K,S}) where {T,K,S} = init(S)

init(::Type{Symbol}) = :init
init(::Type{Nothing}) = nothing
init(::Type{Vector{T}}) where {T} = T[]
init(::Type{Set{T}}) where {T} = Set{T}()
init(::Type{NT}) where {NT <: NamedTuple} = NT(init.(NT.types))
init(::Type{T}) where {T} = typemin(T)
init(::Type{T}) where {T <: Number} = one(T)



token_error(n::Token) = error(styled"""
    {bright_red:No Token Identified in $(summary(n.data)) at position $(n.i)

    i:j (size) state kind data:
    $n
    }""")

#-----------------------------------------------------------------------------# Selectors
for (Sel, sym) in (:Before => :≺, :First => :←, :Last => :→, :Not => :¬, :UseStringView => :𝑠)
    @eval (struct $Sel{T} x::T end; $sym(x) = $Sel(x))
end


findnextind(x, t::Token, i) = (j = _findnext(x, t, i); isnothing(j) ? length(t.data) : j)


|(a, t::Token) = isfirst(a, t)

@inline is(a) = Base.Fix1(is, a)
@inline is(f::Function) = f

@inline is(a::T, b::T) where {T} = a == b
@inline is(a::Char, b::UInt8) = UInt8(a) == b
@inline is(a::UInt8, b::Char) = Char(a) == b
@inline is(f::Function, b) = f(b)
@inline is(n::Not, b) = !is(n.x, b)
@inline is(x::Bool, b) = x

# Fallback methods
@inline isfirst(x, d) = is(x, first(d))
@inline isfirst(o::UseStringView, d) = isfirst(o.x, StringView(d))
@inline _findnext(arg, data, i) = findnext(is(arg), data, i)
@inline _findnext(o::UseStringView, data, i) = findnext(is(o.x), StringView(data), nextind(StringView(data), i - 1))

# AbstractString/AbstractVector/Tuple
@inline isfirst(x::AbstractString, d) = isfirst(codeunits(x), d)
@inline _findnext(x::AbstractString, d, i) = _findnext(codeunits(x), d, i)
@inline isfirst(x::AbstractVector, d) = all(is(x)(d) for (x,d) in zip(x,d))
@inline _findnext(x::AbstractVector, d, i) = findnext(x, d, i)
@inline isfirst(t::Tuple, d) = isempty(t) ? true : is(t[1], d[1]) && isfirst(tail(t), @view(d[2:end]))
@inline _findnext(t::Tuple, d, i) = (j = _findnext(t[1], d,i); isfirst(t, @view(d[j:end])) ? j + length(t) - 1 : _findnext(t, d, j + 1))

# Selectors that only work with _findnext
@inline _findnext(j::Int, d, i) = j
@inline _findnext(f::First, d, i) = (rng = _findnext(f.x, d, i); isnothing(rng) ? nothing : first(rng))
@inline _findnext(f::Last, d, i) =  (rng = _findnext(f.x, d, i); isnothing(rng) ? nothing : last(rng))
@inline _findnext(b::Before, d, i) = (j = _findnext(b.x, d, i); isnothing(j) ? length(d) : j - 1)


#-----------------------------------------------------------------------------# Rule
struct Rule{F, T}
    from::F
    to::T
end
Base.show(io::IO, r::Rule) = print(io, styled"{highlight:$(r.from)} --> {highlight:$(r.to)}")
-->(a, b) = Rule(a, b)

isfirst(r::Rule, t) = isfirst(r.from, t)
_findnext(r::Rule, data, i) = _findnext(r.to, data, i)

|(t::Token{T,K,S}, r::Rule) where {T, K, S} = t | r.to

const STRING = '"' --> →((¬('\\'), '"'))
const NUMBER = ∈(b"-0123456789") --> ≺(∉(b"-+eE.0123456789"))
const ASCII_WHITESPACE = ∈(b" \t\r\n") --> ≺(∉(b" \t\r\n"))
const ASCII_WORD = ∈(UInt8('A'):UInt8('z')) --> ≺(∉(UInt8('A'):UInt8('z')))

const WORD = 𝑠(isletter) --> ≺(𝑠(!isletter))
const WHITESPACE = 𝑠(isspace) --> ≺(𝑠(!isspace))

#-----------------------------------------------------------------------------# @tryrule(s)
function tryrule_ex(kind, rule, token=:(n))
    :($rule | $token && return $token | $kind | $rule)
end

macro tryrule(kind, rule, token=:(n))
    esc(tryrule_ex(kind, rule, token))
end
macro tryrules(ex, token=:(n))
    Base.remove_linenums!(ex)
    exprs = map(ex.args) do x
        tryrule_ex(QuoteNode(x.args[1]), x.args[2], token)
    end
    e = Expr(:block, exprs..., :(token_error(token)))
    return esc(e)
end

#-----------------------------------------------------------------------------# JSONTokens
struct JSONTokens{T <: Data} <: Tokenizer{T, Symbol, Nothing}
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
    string          = STRING
    number          = NUMBER
    whitespace      = ASCII_WHITESPACE
end

#-----------------------------------------------------------------------------# HTMLTokens
struct HTMLTokens{T <: Data} <: Tokenizer{T, Symbol, Nothing}
    data::T
end
next(t::HTMLTokens, n::Token) = @tryrules begin
    whitespace = WHITESPACE
    comment     = "<!--"    --> →("-->")
    doctype     = "<!"      --> '>'
    close_tag   = "</"      --> '>'
    open_tag    = "<"       --> '>'
    text        = true      --> ≺(←("</"))
end

#-----------------------------------------------------------------------------# XMLTokens
struct XMLTokens{T <: Data} <: Tokenizer{T, Symbol, Bool}
    data::T
end
next(o::XMLTokens, n::Token) = @tryrules begin
    whitespace = WHITESPACE
    processing_instruction  = "<?" --> '>'
    comment                 = "<!--" --> →("-->")
    cdata                   = "<![" --> →("]]>")
    close_tag               = "</" --> '>'
    open_tag                = "<" --> '>'
    text                    = true --> ≺('<')
end

#-----------------------------------------------------------------------------# DelimFileTokens
struct DelimFileTokens{T <: Data} <: Tokenizer{T, Symbol, Int}
    data::T
    delim::Char
end
init(::DelimFileTokens, state::Int) = 1
next(o::DelimFileTokens, n::Token) = @tryrules begin
    whitespace  = WHITESPACE
    delim       = o.delim --> 1
    colon       = ':' --> 1
    string      = STRING
    word        = WORD
    number      = NUMBER
    unknown     = true --> 1
end

#-----------------------------------------------------------------------------# CharFunTokens
struct CharFunTokens{T, F, K} <: Tokenizer{T, K, Nothing}
    data::T
    f::F
end
function CharFunTokens(data::T, f::F) where {T, F}
    K = typeof(f(first(StringView(data))))
    CharFunTokens{T, F, K}(data, f)
end
function next(o::CharFunTokens, n::Token)
    k = o.f(first(StringView(n)))
    return n | k | ≺(𝑠(x -> o.f(x) != k))
end

end  # module
