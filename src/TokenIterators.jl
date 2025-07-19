module TokenIterators

using StyledStrings, StringViews

import Base: startswith, findfirst, findnext, !, tail, |, +, -

export Token

export Token, JSONTokens, HTMLTokens, XMLTokens, DelimFileTokens

#-----------------------------------------------------------------------------# utils
const Data = AbstractVector{UInt8}
const SData = AbstractString

# Add _ separator for large Ints
format(x::Int) = replace(string(x), r"(\d)(?=(\d{3})+(?!\d))" => s"\1_")

#-----------------------------------------------------------------------------# Token
struct Token{T <: Data, K} <: Data
    data::T
    kind::K
    i::Int
    j::Int
end
Token(data::Data, kind=:init) = Token(data, kind, 1, 0)

Base.view(t::Token) = view(t.data, t.i:t.j)
StringViews.StringView(t::Token) = StringView(view(t))
Base.length(t::Token) = t.j - t.i + 1
Base.size(t::Token) = (length(t),)
Base.getindex(t::Token, i::Integer) = getindex(view(t), i)

function Base.show(io::IO, ::MIME"text/plain", t::Token)
    rng = styled"{bright_black:$(format(t.i)):$(format(t.j)) ($(Base.format_bytes(length(t))))}"
    s = styled"$rng {bright_cyan:$(t.kind)} "
    n = displaysize(io)[2] - length(s) - 1
    _s2 = styled"{inverse:{bright_cyan:$(escape_string(StringView(t)))}}"
    s2 = _s2[1:min(n, end)] * (length(_s2) > n ? styled"{bright_cyan:…}" : "")
    print(io, s, s2)
end
Base.show(io::IO, t::Token) = show(io, MIME("text/plain"), t)

after(t::Token) = Token(t.data, t.kind, t.j + 1, length(t.data))

(t::Token)(kind, len::Integer = 1) = Token(t.data, kind, t.i, t.i + len - 1)


#-----------------------------------------------------------------------------# TokenIterator
# required field: `data::Data`
abstract type TokenIterator{T, K} end
Base.show(io::IO, o::TokenIterator) = print(io, typeof(o).name.name, " ($(Base.format_bytes(length(o.data))))")

Base.IteratorSize(::Type{T}) where {T <: TokenIterator} = Base.SizeUnknown()
Base.eltype(::Type{Tok}) where {T, K, Tok <: TokenIterator{T, K}} = Token{T, K}
Base.isdone(o::TokenIterator, prev::Token) = prev.j == length(prev.data)

function Base.iterate(o::TokenIterator, prev = init(o))
    Base.isdone(o, prev) && return nothing
    n = next(o, after(prev))
    return n, n
end

init(o::TokenIterator) = Token(o.data)

#-----------------------------------------------------------------------------# debugging
# Failure to identify token from `data` at position `i`.  Error prints `nchars` on both sides of `data[i]`.
function token_error(data::Data, i::Int, nchars=50)
    sv = StringView(data)
    pre = escape_string(sv[max(1, i - nchars):i - 1])
    c = escape_string(string(sv[i]))
    post = escape_string(sv[i + 1:min(i + nchars, end)])
    error(styled"""
    {bright_red:No Token Identified in $(summary(data)) beginning at position $i}

    {bright_black:$pre}{{inverse:{bright_red:$c}}{bright_black:$post}
    """)
end

function debug(t::TokenIterator)
    tok = nothing
    try
        for ti in t
            tok = ti
        end
    catch
        token_error(tok.data, tok.j + 1)
    end
    @info "success!"
end

#-----------------------------------------------------------------------------# @trytok
macro trytok(n, kind, rule)
    esc(quote
        if startswith($n, $rule)
            return Token($n.data, $kind, $n.i, $n.i + findlen($n, $rule) - 1)
        end
    end)
end

#-----------------------------------------------------------------------------# pattern interface

# Shorthand:
<<(x, t::Token) = startswith(t, x)

startswith(::Token, b::Bool) = b
findlen(::Token, b::Bool) = 1

startswith(t::Token, rule::Pair) = startswith(t, rule[1])
findlen(t::Token, rule::Pair) = findlen(t, rule[1], rule[2])

startswith(t::Token, rule::Char) = StringView(t)[1] == rule
findlen(::Token, ::Char) = 1
findlen(t::Token, ::Any, c::Char) = findnext(==(c), StringView(t), 2)

startswith(t::Token, rule::AbstractString) = startswith(StringView(t), rule)
findlen(t::Token, rule::AbstractString) = length(rule)
findlen(t::Token, ::Any, rule::AbstractString) = last(findnext(rule, StringView(t), 2))

startswith(t::Token, rule::Regex) = startswith(StringView(t), rule)
findlen(t::Token, rule::Regex) = 1

startswith(t::Token, rule::Function) = rule(t[1])
function findlen(t::Token, rule::Function)
    i = findnext(!rule, t, 2)
    isnothing(i) ? length(t) : i - 1
end
findlen(t::Token, a::Any, b::Function) = findlen(t, b)

startswith(t::Token, rule::Data) = all(ti == bi for (ti, bi) in zip(t, rule))
findlen(t::Token, rule::Data) = length(rule)

struct Unescaped
    char::Char
end
u(x) = Unescaped(x)
function findlen(t::Token, ::Any, rule::Unescaped)
    skip = false
    for (j, byte) in enumerate(t)
        j == 1 && continue
        c = Char(byte)
        c == rule.char && !skip && return j
        skip = c == '\\' ? !skip : false
    end
end

struct UseStringView{T} <: Function
    x::T
end
s(x) = UseStringView(x)
(f::UseStringView)(x) = f.x(x)
startswith(t::Token, o::UseStringView{F}) where {F <: Function} = o.x(StringView(t)[1])
function findlen(t::Token, rule::UseStringView{F}) where {F <: Function}
    i = findnext(!rule, StringView(t), 2)
    isnothing(i) ? length(t) : i - 1
end
Base.:(!)(o::UseStringView) = UseStringView(!o.x)

struct Unknown end
startswith(::Token, ::Unknown) = true
findlen(::Token, ::Unknown) = 1

struct Before{T}
    x::T
end
findlen(t::Token, ::Any, b::Before{<:AbstractString}) = (i = findnext(b.x, UseStringView(t), 2); isnothing(i) ? length(t) : first(i) - 1)
findlen(t::Token, ::Any, b::Before{<:Data}) = (i = findnext(b.x, view(t), 2); isnothing(i) ? length(t) : first(i) - 1)

#-----------------------------------------------------------------------------# Common rules
const STRING = '"' => u('"')
const NUMBER = (∈(b"-0123456789")) => (∈(b"-+eE.0123456789"))
const ASCII_WHITESPACE = (∈(b" \t\r\n"))
const ASCII_LETTERS = ∈(UInt8('A'):UInt8('z'))
const LETTERS = s(isletter)
const WHITESPACE = s(isspace)
const DIGITS = s(isdigit)

#-----------------------------------------------------------------------------# JSONTokens
struct JSONTokens{T} <: TokenIterator{T, Symbol}
    data::T
end
function next(o::JSONTokens, t::Token)
    '{' << t && return t(:curly_open)
    '}' << t && return t(:curly_close)
    '[' << t && return t(:square_open)
    ']' << t && return t(:square_close)
    ':' << t && return t(:colon)
    ',' << t && return t(:comma)
    't' << t && return t(:True, 4)
    'f' << t && return t(:False, 5)
    'n' << t && return t(:null, 4)
    @trytok t :whitespace (∈(b"\t\n\r "))
    @trytok t :string STRING
    @trytok t :number (∈(b"-0123456789")) => (∈(b"-+eE.0123456789"))
    return t(:unknown)
end


#-----------------------------------------------------------------------------# HTMLTokens
struct HTMLTokens{T <: Data} <: TokenIterator{T, Symbol}
    data::T
end
function next(o::HTMLTokens, t::Token)
    @trytok t :whitespace WHITESPACE
    @trytok t :comment "<!--" => "-->"
    @trytok t :doctype "<!" => '>'
    @trytok t :close_tag "</" => '>'
    @trytok t :open_tag '<' => '>'
    @trytok t :text true => Before(b"</")
    return t(:unknown)
end

#-----------------------------------------------------------------------------# XMLTokens
struct XMLTokens{T <: Data} <: TokenIterator{T, Symbol}
    data::T
end
function next(o::XMLTokens, t::Token)
    @trytok t :whitespace WHITESPACE
    @trytok t :comment "<!--" => "-->"
    @trytok t :processing_instruction "<?" => "?>"
    @trytok t :cdata "<![" => "]]>"
    @trytok t :doctype "<!" => '>'
    @trytok t :close_tag "</" => '>'
    @trytok t :open_tag '<' => '>'
    @trytok t :text true => Before(b"</")
    t(:unknown)
end

#-----------------------------------------------------------------------------# DelimFileTokens
struct DelimFileTokens{T <: Data} <: TokenIterator{T, Symbol}
    data::T
    delim::Char
end
DelimFileTokens(data) = DelimFileTokens(data, ',')
function next(o::DelimFileTokens, t::Token)
    @trytok t :whitespace WHITESPACE
    @trytok t :delim o.delim
    @trytok t :string STRING
    @trytok t :word LETTERS
    @trytok t :number NUMBER
    return t(:unknown)
end


end  # module
