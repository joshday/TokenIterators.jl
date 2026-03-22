module TokenIterators

using StringViews, StyledStrings
import Base: startswith, findnext

#------------------------------------------------------------------------------# Anchor
# "Anchor: First/Last/Before/After anchors a UnitRange returned from findnext into an Int"
@enum Anchor First Last Before After
function pick(t::Token, rng::UnitRange{Int}, anchor::Anchor)
    anchor == First ? rng[1] :
    anchor == Last ? rng[end] :
    anchor == Before ? rng[1] - 1 :
    rng[end] + 1
end
pick(t::Token, ::Nothing, anchor::Anchor) = anchor == Before ? length(t) : error("$anchor can't be applied to `nothing`")


#------------------------------------------------------------------------------# Token
struct Token{T<:AbstractVector{UInt8},K} <: AbstractVector{UInt8}
    data::T
    kind::K
    i::Int
    j::Int
end
Token(data::AbstractVector{UInt8}, kind=nothing) = Token(data, kind, 1, 0)
Token(s::AbstractString, kind=nothing) = Token(codeunits(s), kind, 1, 0)

Base.view(t::Token) = view(t.data, t.i:t.j)
StringViews.StringView(t::Token) = StringView(view(t))
Base.length(t::Token) = t.j - t.i + 1
Base.size(t::Token) = (length(t),)
Base.getindex(t::Token, i::Integer) = view(t)[i]

function Base.show(io::IO, ::MIME"text/plain", t::Token)
    s = styled"$(format(t.i)) → $(format(t.j)) {bright_black:($(Base.format_bytes(length(t))))} " *
        styled"{bright_cyan:$(t.kind)} "
    n = displaysize(io)[2] - length(s) - 1
    _s2 = styled"{inverse:{bright_cyan:$(escape_string(StringView(t)))}}"
    s2 = _s2[1:min(n, end)] * (length(_s2) > n ? styled"{bright_cyan:…}" : "")
    print(io, s, s2)
end
Base.show(io::IO, t::Token) = show(io, MIME("text/plain"), t)

after(t::Token) = Token(t.data, t.kind, t.j + 1, length(t.data))

function emit(t::Token, kind, start_pattern_width::Int, stop_pattern, anchor::Anchor=Last)
    len = findnext(stop_pattern, t, start_pattern_width + 1, anchor)
    Token(t.data, kind, t.i, t.i + len - 1)
end


#-----------------------------------------------------------------------------# utils
format(x::Int) = replace(string(x), r"(\d)(?=(\d{3})+(?!\d))" => s"\1_")

function token_error(data::AbstractVector{UInt8}, i::Int, nchars=50)
    sv = StringView(data)
    pre = escape_string(sv[max(1, i - nchars):i-1])
    c = escape_string(string(sv[i]))
    post = escape_string(sv[i+1:min(i + nchars, end)])
    error(styled"""
    {bright_red:No Token Identified in $(summary(data)) beginning at position $i}

    {bright_black:$pre}{inverse:{bright_red:$c}}{bright_black:$post}
    """)
end

#------------------------------------------------------------------------------# interface
# startswith(::Token, ::Pattern) --> Bool
# findnext(::Pattern, ::Token, ::Int) -> Union{Int, UnitRange{Int}, Nothing}
#
# Pipeline:
# 1) startswith(token, start_pattern) --> true
# 2) findnext(stop_pattern, token, width(start_pattern), anchor?)
# 3) If findnext returns range, adjust with `Anchor`


#------------------------------------------------------------------------------# startswith rules
width(::Token, x) = width(x)

startswith(t::Token, x::UInt8) = t[1] == x
width(::UInt8) = 1

startswith(t::Token, x::Char) = isascii(x) ? t[1] == UInt8(x) : StringView(t)[1] == x
width(x::Char) = ncodeunits(x)

function startswith(t::Token, x::AbstractVector{UInt8})
    length(x) > length(t) && return false
    all(a == b for (a, b) in zip(t, x))
end
width(x::AbstractVector{UInt8}) = length(x)

startswith(t::Token, x::AbstractString) = startswith(t, codeunits(x))
width(x::AbstractString) = ncodeunits(x)

startswith(t::Token, rule::Function) = rule(t[1])
width(::Function) = 1

drop(t::Token, n::Integer) = Token(t.data, t.kind, t.i + n, t.j)
startswith(t::Token, rule::Tuple) = isempty(rule) ? true : startswith(t, rule[1]) && startswith(drop(t, width(rule[1])), Base.tail(rule))
width(t::Token, rule::Tuple) = sum(width(t, r) for r in rule)

#------------------------------------------------------------------------------# findnext rules
# Fallback ignores `anchor`
findnext(pattern, t::Token, i::Integer, anchor::Anchor) = findnext(pattern, t, i)

findnext(f::Function, t::Token, i::Integer) = findnext(f, view(t), i)
findnext(x::Integer, t::Token, i::Integer) = x
findnext(x::UInt8, t::Token, i::Integer) = findnext(==(x), view(t), i)
findnext(x::Char, t::Token, i::Integer) = isascii(x) ? findnext(==(UInt8(x)), view(t), i) : findnext(==(x), StringView(t), i)
findnext(x::AbstractString, t::Token, i::Integer, anchor::Anchor) = pick(t, findnext(x, StringView(t), i), anchor)

#------------------------------------------------------------------------------# Char functions
for f in [isletter, isdigit, isnumeric, isspace, isuppercase, islowercase, isxdigit, ispunct, isprint, isascii, iscntrl]
    @eval begin
        startswith(t::Token, ::typeof($f)) = f(StringView(t)[1])
        findnext(::typeof($f), t::Token, i::Int) = findnext(f, StringView(t), i)
    end
end

#-----------------------------------------------------------------------------# Unescaped
struct Unescaped{T<:Union{UInt8,Char}}
    char::T
    esc::T
    Unescaped(c::T, esc::T=T('\\')) where {T<:Union{UInt8,Char}} = new{T}(c, esc)
end

function findnext(rule::Unescaped{UInt8}, t::Token, i::Integer)
    escaped = false
    @inbounds for j in (i+1):length(t)
        byte = t.data[t.i+j-1]
        byte == rule.char && !escaped && return j
        escaped = byte == rule.esc ? !escaped : false
    end
    return nothing
end

function findnext(rule::Unescaped{Char}, t::Token, i::Integer)
    escaped = false
    sv = StringView(t)
    j = nextind(sv, i)
    while j <= ncodeunits(sv)
        c = sv[j]
        c == rule.char && !escaped && return j
        escaped = c == rule.esc ? !escaped : false
        j = nextind(sv, j)
    end
    return nothing
end


#-----------------------------------------------------------------------------# AbstractTokenizer
abstract type AbstractTokenizer{T,K} end

Base.show(io::IO, o::AbstractTokenizer) = print(io, typeof(o).name.name, " ($(Base.format_bytes(length(o.data))))")

Base.IteratorSize(::Type{<:AbstractTokenizer}) = Base.SizeUnknown()
Base.eltype(::Type{Tok}) where {T,K,Tok<:AbstractTokenizer{T,K}} = Token{T,K}
Base.isdone(o::AbstractTokenizer, prev::Token) = prev.j == length(prev.data)

init(o::AbstractTokenizer{T,K}) where {T,K} = Token(o.data, init(K), 1, 0)
init(K) = error("No init method for kind::$K")

next(o::AbstractTokenizer{T,K}, tok::Token{T,K}) where {T,K} = error("$o has not implemented the required next(...) method.")

function Base.iterate(o::AbstractTokenizer, prev=init(o))
    Base.isdone(o, prev) && return nothing
    t = next(o, after(prev))
    return t, t
end

function debug(o::AbstractTokenizer)
    tok = nothing
    try
        for (i, ti) in enumerate(o)
            tok = ti
            i > length(o.data) && error("Found more tokens than bytes available")
        end
    catch
        @warn "Probably an infinite loop.  Stopping after $(length(o.data)) tokens."
        token_error(tok.data, tok.j + 1)
    end
    @info "success!"
end


end  # module
