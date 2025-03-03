module Tokenizers

using StyledStrings, StringViews

import Base: startswith, findfirst, findnext, /, tail

export Token, Next, JSONTokens, HTMLTokens, XMLTokens, Rule,
    Byte, ByteSet, FixedPosition, Before, First, Last, Pattern, Not, init, nextstate,
    selector, next, next_data, rules, findj, isfirst, isbyte, nbytes, ≪, ←, →, ¬, ∿, ≺, ⋆, ≛,
    encode, decode, Flags

#-----------------------------------------------------------------------------# utils
const Data = AbstractVector{UInt8}
const SData = AbstractString

#-----------------------------------------------------------------------------# Flags
# encode(x::T, pos::Integer, val::Bool) where {T} = val ? x | (T(1) << pos) : x & ~(T(1) << pos)
# decode(x::T, pos::Integer) where {T} = (x >> pos) & T(1) == 1

# struct Flags{T, names} x::T end
# function Flags{T}(x) where {T}
#     out = zero(T)
#     for (i, (k, v)) in enumerate(pairs(x))
#         out = encode(out, i, v)
#     end
#     Flags{T, keys(x)}(out)
# end
# Base.getindex(o::Flags, i::Integer) = decode(getfield(o, :x), i)
# Base.propertynames(o::Flags{T, names}) where {T, names} = names


# struct Flags{T, names}
#     x::T
# end
# function Flags(x)
#     length(x) ≤ 8 && return Flags{UInt8, keys(x)}
# end

#-----------------------------------------------------------------------------# AbstractState
struct State{T} x::T end
State(; kw...) = State(NamedTuple(kw))
(o::State{T})(state) where {T} = o.x
(o::State{<:Function})(state) = o.x(state)
(o::State{<:NamedTuple})(state) = merge(state, o.x)


#-----------------------------------------------------------------------------# Token
struct Token{T <: Data, K, S} <: Data
    data::T
    kind::K
    i::Int
    j::Int
    state::S
end
Token(data, kind, state=nothing) = Token(data, kind, 1, 0, state)
⋆(t::Token{T, K}, x::S) where {T, K, S} = Token(t.data, t.kind, t.i, t.i - 1 + _findnext(x, t, 2), t.state)
⋆(t::Token{T, K}, k::K) where {T, K} = Token(t.data, k, t.i, t.j, t.state)
⋆(t::Token{T, K}, s::State) where {T, K} = Token(t.data, t.kind, t.i, t.j, s(t.state))
≛(t::Token, k) = Token(t.data, k, t.i, t.i, t.state)


Base.view(t::Token) = view(t.data, t.i:t.j)
StringViews.StringView(t::Token) = StringView(view(t))
Base.length(t::Token) = t.j - t.i + 1
Base.size(t::Token) = (length(t),)
Base.getindex(t::Token, i::Integer) = getindex(view(t), i)
function Base.show(io::IO, ::MIME"text/plain", t::Token)
    format(x::Int) = replace(string(x), r"(\d)(?=(\d{3})+(?!\d))" => s"\1_")
    rng = styled"{bright_black:$(format(t.i)):$(format(t.j)) len=$(format(length(t)))}"
    s = styled"$rng {bright_cyan:$(t.kind)} "
    n = displaysize(io)[2] - length(s) - 1
    _s2 = styled"{inverse:{bright_cyan:$(escape_string(StringView(t)))}}"
    s2 = _s2[1:min(n, end)] * (length(_s2) > n ? styled"{bright_cyan:…}" : "")
    print(io, s, s2)
end
Base.show(io::IO, t::Token) = show(io, MIME("text/plain"), t)

after(t::Token) = Token(t.data, t.kind, t.j + 1, length(t.data), t.state)


#-----------------------------------------------------------------------------# Selectors
for (Sel, sym) in (:Before => :≺, :First => :←, :Last => :→, :Not => :¬, :UseStringView => :∿)
    @eval (struct $Sel{T} x::T end; $sym(x) = $Sel(x))
end

≪(a, b) = isfirst(a, b)
isfirst(x::Union{Char, UInt8}, d::Data) = UInt8(x) == d[1]
isfirst(x::Char, s::SData) = x == s[1]
isfirst(f::Function, d) = f(d[1])
isfirst(x::Data, d::Data) = all(x -> x[1] == x[2], zip(d, x))
isfirst(t::Tuple, d) = isempty(t) ? true : isfirst(t[1], d) && isfirst(tail(t), @view(d[2:end]))
isfirst(n::Not, d) = !isfirst(n.x, d)
isfirst(o::UseStringView, d) = isfirst(o.x, StringView(d))

_findnext(x, d, i) = findnext(x, d, i)
_findnext(x::Union{Char, UInt8}, d::Data, i) = findnext(==(UInt8(x)), d, i)
_findnext(n::Not{Char}, d::Data, i) = findnext(!=(UInt8(n.x)), d, i)
_findnext(n::Not{Char}, s::SData, i) = findnext(!=(n.x), s, i)
_findnext(n::Not{<:Function}, d, i) = findnext(!n.x, d, i)
_findnext(j::Integer, d, i) = j ≥ i ? j : nothing
_findnext(f::First, d, i) = (rng = _findnext(f.x, d, i); isnothing(rng) ? nothing : first(rng))
_findnext(f::Last, d, i) = (rng = _findnext(f.x, d, i); isnothing(rng) ? nothing : last(rng))
_findnext(b::Before, d, i) = (j = _findnext(b.x, d, i); isnothing(j) ? length(d) : j - 1)
_findnext(n::Not{<:Function}, d, i) = _findnext(!n.x, d, i)
_findnext(o::UseStringView, d, i) = _findnext(o.x, StringView(d), i)

function _findnext(t::Tuple, d, i)
    j = _findnext(t[1], d, i)
    isfirst(t, @view(d[j:end])) ? (j + length(t) - 1) : _findnext(t, d, j + 1)
end
_findnext(o::UseStringView, d, i) = _findnext(o.x, StringView(d), i)


#-----------------------------------------------------------------------------# Tokenizer
abstract type Tokenizer{T, K, S} end
Base.IteratorSize(::Type{T}) where {T <: Tokenizer} = Base.SizeUnknown()
Base.eltype(::Type{Tok}) where {T, K, S, Tok <: Tokenizer{T, K, S}} = Token{T, K}
Base.isdone(o::Tokenizer, n::Token) = isempty(n)
Base.show(io::IO, o::Tokenizer) = print(io, typeof(o).name.name, " ($(summary(o.data)))")

init(::Type{Symbol}) = :init
init(::Type{Bool}) = false
init(::Type{Nothing}) = nothing
init(::Type{Vector{T}}) where {T} = T[]
init(::Type{Set{T}}) where {T} = Set{T}()
init(::Type{NT}) where {NT <: NamedTuple} = NT(init.(NT.types))

init(t::Tokenizer{T,K,S}) where {T,K,S} = Token(t.data, init(K), 1, length(t.data), init(S))
# nextstate(o, n, n2, state) = state

function Base.iterate(o::Tokenizer, n = init(o))
    Base.isdone(o, n) && return nothing
    n = next(o, n)
    return n, after(n)
end

#-----------------------------------------------------------------------------# JSONTokens
struct JSONTokens{T <: Data} <: Tokenizer{T, Symbol, Nothing}
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
    ∈(b"-0123456789") ≪ n && return n ⋆ :number ⋆ Before(∉(b"-+eE.0123456789"))
    ∈(b" \t\n\r") ≪ n && return n ⋆ :whitespace ⋆ Before(∉(b" \t\n\r"))
    return n ≛ :unknown
end

#-----------------------------------------------------------------------------# HTMLTokens
struct HTMLTokens{T <: Data} <: Tokenizer{T, Symbol, @NamedTuple{style::Bool, script::Bool, tag::Bool}}
    data::T
end
function next(o::HTMLTokens, n)
    ∿(isspace) ≪ n && return n ⋆ :whitespace ⋆ ∿(≺(!isspace))
    # state tag=true
    b"<script" ≪ n && return n ⋆ :open_tag_start ⋆ ≺(∈(b" >")) ⋆ State(script=true, tag=true)
    b"<style" ≪ n && return n ⋆ :open_tag_start ⋆ ≺(∈(b" >")) ⋆ State(script=true, tag=true)
    if n.state.tag
        ∿(isletter) ≪ n && return n ⋆ :tag_name ⋆ ∿(≺(!isletter))
        '=' ≪ n && return n ≛ :equals
        '"' ≪ n && return n ⋆ :attr_val ⋆ '"'
    end
    n.state.script && return n ⋆ :text ⋆ ≺(←(b"</script>")) ⋆ State(script=false)
    n.state.style && return n ⋆ :text ⋆ ≺(←(b"</style>")) ⋆ State(style=false)
    b"<!--" ≪ n && return n ⋆ :comment ⋆ →(b"-->")
    b"<!" ≪ n && return n ⋆ :doctype_start ⋆ 9 ⋆ State(tag=true)
    b"</" ≪ n && return n ⋆ :close_tag_start ⋆ '>'
    '>' ≪ n && return (n ≛ :open_tag_end) ⋆ State(tag=false)
    '<' ≪ n && return n ⋆ :open_tag_start ⋆ ≺(∈(b" >")) ⋆ State(tag=true)
    return n ⋆ :text ⋆ ≺('<')
end

#-----------------------------------------------------------------------------# XMLTokens
struct XMLTokens{T <: Data} <: Tokenizer{T, Symbol, Bool}
    data::T
end
function next(o::XMLTokens, n::Token)
    ∿(isspace) ≪ n && return n ⋆ :whitespace ⋆ ∿(≺(!isspace))
    if n.state
        ∿(isletter) ≪ n && return n ⋆ :tag_name ⋆ ∿(≺(x -> !isletter(x) && x != ':'))
        '=' ≪ n && return n ≛ :equals
        '"' ≪ n && return n ⋆ :attr_val ⋆ '"'
    end
    b"<?" ≪ n && return n ⋆ :pi_start ⋆ ≺(∈(b" ")) ⋆ State(true)
    b"?>" ≪ n && return n ⋆ :pi_end ⋆ 2 ⋆ State(false)
    b"<!--" ≪ n && return n ⋆ :comment ⋆ →(b"-->")
    b"<![" ≪ n && return n ⋆ :cdata ⋆ →(b"]]>")
    b"</" ≪ n && return n ⋆ :close_tag ⋆ '>'
    '>' ≪ n && return (n ≛ :open_tag_end) ⋆ State(false)
    b"/>" ≪ n && return n ⋆ :self_close_tag_end ⋆ 2
    '<' ≪ n && return n ⋆ :open_tag_start ⋆ ≺(∈(b" >")) ⋆ State(true)
    return n ⋆ :text ⋆ ≺('<')
end

end  # module
