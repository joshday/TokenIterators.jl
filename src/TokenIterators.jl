module TokenIterators

using StyledStrings, StringViews

import Base: startswith, findnext

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

function (t::Token)(kind, x=1, skip=2)
    _j = findnext(x, t, skip)
    Token(t.data, kind, t.i, t.i + _j - 1)
end


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
        for (i, ti) in enumerate(t)
            tok = ti
            i > length(t.data) && error("Found more tokens than bytes available")
        end
    catch
        @warn "Probably an infinite loop.  Stopping after $(length(t.data)) tokens."
        token_error(tok.data, tok.j + 1)
    end
    @info "success!"
end

#-----------------------------------------------------------------------------# pattern interface
Base.:(<<)(x, t::Token) = startswith(t, x)

startswith(t::Token, rule::Char) = StringView(t)[1] == rule
startswith(t::Token, rule::Regex) = startswith(StringView(t), rule)
startswith(t::Token, rule::AbstractString) = startswith(StringView(t), rule)
startswith(t::Token, rule::Function) = rule(t[1])
startswith(t::Token, rule::Data) = all(ti == bi for (ti, bi) in zip(t, rule))

findnext(f::Function, t::Token, i::Integer) = findnext(f, view(t), i)
findnext(x::Integer, t::Token, i::Integer) = x
findnext(x::UInt8, t::Token, i::Integer) = findnext(==(x), t, i)
findnext(x::Char, t::Token, i::Integer) = findnext(==(x), StringView(t), i)
findnext(x::AbstractString, t::Token, i::Integer) = findnext(x, StringView(t), i)


struct UseStringView{T}
    x::T
end
s(x) = UseStringView(x)
startswith(t::Token, o::UseStringView{F}) where {F <: Function} = o.x(StringView(t)[1])
findnext(o::UseStringView, t::Token, i::Integer) = findnext(o.x, StringView(t), i)

# ASCII-only
struct Unescaped
    char::Char
end
u(x) = Unescaped(x)
function findnext(rule::Unescaped, t::Token, i::Integer)
    skip = false
    for (j, byte) in enumerate(t)
        j ≤ i && continue
        c = Char(byte)
        c == rule.char && !skip && return j
        skip = c == '\\' ? !skip : false
    end
    return length(t)
end

struct Last{T}
    x::T
end
findnext(o::Last, t::Token, i::Integer) = last(findnext(o.x, t, i))

struct Before{T}
    x::T
end
findnext(o::Before, t::Token, i::Integer) = (i=findnext(o.x, t, i); isnothing(i) ? length(t) : first(i) - 1)

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
    ∈(b"\t\n\r ") << t && return t(:whitespace, Before(!∈(b"\t\n\r ")))
    '"' << t && return t(:string, u('"'))
    ∈(b"-0123456789") << t && return t(:number, Before(!∈(b"-+eE.012345678")))
    return t(:unknown)
end


#-----------------------------------------------------------------------------# HTMLTokens
struct HTMLTokens{T <: Data} <: TokenIterator{T, Symbol}
    data::T
end
function next(o::HTMLTokens, t::Token)
    s(isspace) << t && return t(:whitespace, Before(s(!isspace)))
    "<!--" << t && return t(:comment, Last("-->"), 5)
    "<!" << t && return t(:doctype, '>', 10)
    "</" << t && return t(:close_tag, '>', 3)
    '<' << t && return t(:open_tag, '>', 3)
    return t(:text, Before(b"</"))
end

#-----------------------------------------------------------------------------# XMLTokens
struct XMLTokens{T <: Data} <: TokenIterator{T, Symbol}
    data::T
end
function next(o::XMLTokens, t::Token)
    s(isspace) << t && return t(:whitespace, Before(s(!isspace)))
    "<!--" << t && return t(:comment, Last("-->"), 5)
    "<?" << t && return t(:processing_instruction, Last("?>"), 3)
    "<![" << t && return t(:cdata, Last("]]>"), 10)
    "<!" << t && return t(:doctype, '>', 10)
    "</" << t && return t(:close_tag, '>', 4)
    '<' << t && return t(:open_tag, '>', 3)
    return t(:text, Before(b"</"))
end

#-----------------------------------------------------------------------------# DelimFileTokens
struct DelimFileTokens{T <: Data} <: TokenIterator{T, Symbol}
    data::T
    delim::Char
end
DelimFileTokens(data) = DelimFileTokens(data, ',')
function next(o::DelimFileTokens, t::Token)
    s(isspace) << t && return t(:whitespace, Before(s(!isspace)))
    delim << t && return t(:delim)
    '"' << t && return t(:string, u('"'))
    s(isletter) << t && return t(:word, Before(s(!isletter)))
    s(isnumeric) << t && return t(:numeric, Before(s(!isnumeric)))
    t(:unknown)
end


end  # module
