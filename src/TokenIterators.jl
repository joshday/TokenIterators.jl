module TokenIterators

using StyledStrings, StringViews

import Base: startswith, findfirst, findnext, !, tail, |, +, -

export Token, State, Rule, FSM,
    JSONTokens, HTMLTokens, XMLTokens, DelimFileTokens, CharFunTokens,
    Before, Not, First, Last, ≺, ¬, ←, →, ⋆, ≛, ≪, 𝑠, -->, ..,
    next, is, isfirst, _findnext, init, transition

#-----------------------------------------------------------------------------# utils
const Data = AbstractVector{UInt8}
const SData = AbstractString

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
    format(x::Int) = replace(string(x), r"(\d)(?=(\d{3})+(?!\d))" => s"\1_")
    rng = styled"{bright_black:$(format(t.i)):$(format(t.j)) ($(Base.format_bytes(length(t))))}"
    s = styled"$rng {bright_cyan:$(t.kind)} "
    n = displaysize(io)[2] - length(s) - 1
    _s2 = styled"{inverse:{bright_cyan:$(escape_string(StringView(t)))}}"
    s2 = _s2[1:min(n, end)] * (length(_s2) > n ? styled"{bright_cyan:…}" : "")
    print(io, s, s2)
end
Base.show(io::IO, t::Token) = show(io, MIME("text/plain"), t)

after(t::Token) = Token(t.data, t.kind, t.j + 1, length(t.data))

token_error(n::Token) = error(styled"""
    {bright_red:No Token Identified in $(summary(n.data)) at position $(n.i)
    }""")

(t::Token)(kind, b=1, a=1) = Token(t.data, kind, t.i, t.i + findlen(t, a, b) - 1)


#-----------------------------------------------------------------------------# TokenIterator
# required field: `data::Data`
abstract type TokenIterator{T, K} end
Base.show(io::IO, o::TokenIterator) = print(io, typeof(o).name.name, " ($(summary(o.data)))")

Base.IteratorSize(::Type{T}) where {T <: TokenIterator} = Base.SizeUnknown()
Base.eltype(::Type{Tok}) where {T, K, Tok <: TokenIterator{T, K}} = Token{T, K}
Base.isdone(o::TokenIterator, prev::Token) = prev.j == length(prev.data)

function Base.iterate(o::TokenIterator, prev = init(o))
    Base.isdone(o, prev) && return nothing
    n = next(o, after(prev))
    return n, n
end

init(o::TokenIterator) = Token(o.data)



#-----------------------------------------------------------------------------# pattern interface
struct UseStringView{T}
    f::T
end
s(x) = UseStringView(x)

struct Unescaped{T}
    x::T
end
u(x) = Unescaped(x)


# Does candidate token match pattern?
..(x::UInt8, t::Token) = t[1] == x
..(x::Char, t::Token) = UInt8(t[1]) == x
..(f::Function, t::Token) = f(t[1])
..(v::UseStringView{T}, t::Token) where {T <: Function} = v.f(StringView(t[1]))
..(v::UseStringView{Char}, t::Token) = v.f == StringView(t)[1]
..(x::Data, t::Token) = all(x == t for (x, t) in zip(x, t))
..(x::SData, t::Token) = all(x == s for (x, s) in zip(x, StringView(t)))


# How many bytes does the pattern occupy?
width(t::Token, x) = width(x)
width(x) = 1
width(x::Data) = length(x)
width(x::SData) = length(x)


_findnext(x::UInt8, t::Token, i=2) = findnext(==(x), t, i)
_findnext(x::Char, t::Token, i=2) = findnext(==(UInt8(x)), t, i)
_findnext(f::Function, t::Token, i=2) = (i=findnext(!f, t, i); isnothing(i) ? length(t) : i)
_findnext(s::UseStringView{<:Function}, t::Token, i=2) = findnext(s.f, StringView(t), i)
_findnext(s::UseStringView{Char}, t::Token, i=2) = findnext(s.x, StringView(t), i)
_findnext(x::Data, t::Token, i=2) = last(findnext(x, t, i))
_findnext(x::SData, t::Token, i=2) = last(findnext(x, t, i))
_findnext(x::Int, t::Token, i=2) = x

@inline function _findnext(u::Unescaped, d, i)
    skip = false
    for j in i:length(d)
        c = Char(d[j])
        c == u.x && !skip && return j
        skip = c == '\\' ? !skip : false
    end
end

function _findnext(x::Unescaped, t::Token, i=2)
end

function findlen(t::Token, a, b)
    i = _findnext(b, t, width(a))
    isnothing(i) ? length(t) : i
end



#-----------------------------------------------------------------------------# JSONTokens
struct JSONTokens{T} <: TokenIterator{T, Symbol}
    data::T
end


function next(o::JSONTokens, n::Token)
    '{'             .. n && return n(:curly_open)
    '}'             .. n && return n(:curly_close)
    '['             .. n && return n(:square_open)
    ']'             .. n && return n(:square_close)
    ':'             .. n && return n(:colon)
    ','             .. n && return n(:comma)
    "true"          .. n && return n(4)
    "false"         .. n && return n(5)
    "null"          .. n && return n(4)
    ∈(b"\t\n\r ")   .. n && return ∈(b"\t\n\r ")
    '"'             .. n && return Unescaped('"')
    return n(:unknown)
end

# next(o::JSONTokens, n::Token) = @rules n begin
#     curly_open = '{'  # --> Rule('{', 1) --> is(n, '{') && return n(:curly_open, 1)
#     curly_close = '}'
#     square_open = '['
#     square_close = ']'
#     comma = ','
#     colon = ':'
#     true = b"true"
#     false = "false"
#     # string = '"' --> Unescaped('"')
#     # number = ∈(b"-0123456789") --> Before(∉(b"-+eE.0123456789"))
#     # whitespace = ∈(b"\t\n\r ") --> Before(∉(b"\t\n\r "))
# end

# #-----------------------------------------------------------------------------# (t::Token)(x)
# (t::Token{T,K,S})(kind::K) where {T,K,S} = Token(t.data, t.kind, t.i, t.j, t.state)

# (t::Token{T,K,S})(::PopState) where {T,K,S} = (pop!(t.state); return t)
# (t::Token{T,K,S})(p::PushState) where {T,K,S} = (push!(t.state, p.x); return t)

# function (t::Token{T,K,S})(match) where {T,K,S}
#     _j = token_last_index(match, t, 2)
#     Token(t.data, t.kind, t.i, t.i + _j - 1)
# end

# #-----------------------------------------------------------------------------# Lexer
# abstract type Lexer{T, K, S} end
# Base.show(io::IO, o::Lexer) = print(io, typeof(o).name.name, " ($(summary(o.data)))")

# Base.IteratorSize(::Type{T}) where {T <: Lexer} = Base.SizeUnknown()
# Base.eltype(::Type{Tok}) where {T, K, S, Tok <: Lexer{T, K, S}} = Token{T, K, S}
# Base.isdone(o::Lexer, prev::Token) = prev.j == length(prev.data)

# function Base.iterate(o::Lexer, prev = init(o))
#     Base.isdone(o, prev) && return nothing
#     n = next(o, after(prev))
#     return n, n
# end

# init(o::Lexer{T,K,S}) where {T,K,S} = Token(o.data, init(K), 1, 0, S[])

# init(::Type{Symbol}) = :init

# # init(t::TokenIterator{T,K,S}) where {T,K,S} = Token(t.data, init_kind(t), 1, 0, init_state(t))

# # init_kind(t::TokenIterator{T,K,S}) where {T,K,S} = init(K)
# # init_state(t::TokenIterator{T,K,S}) where {T,K,S} = init(S)

# # init(::Type{Symbol}) = :init
# # init(::Type{Nothing}) = nothing
# # init(::Type{Vector{T}}) where {T} = T[]
# # init(::Type{Set{T}}) where {T} = Set{T}()
# # init(::Type{NT}) where {NT <: NamedTuple} = NT(init.(NT.types))
# # init(::Type{T}) where {T} = typemin(T)
# # init(::Type{T}) where {T <: Number} = one(T)

# # Base.read(file::AbstractString, ::Type{T}) where {T <: TokenIterator} = T(read(file))



# #-----------------------------------------------------------------------------# Selectors
# # for (Sel, sym) in (:Before => :≺, :First => :←, :Last => :→, :Not => :¬, :UseStringView => :𝑠, :Unescaped => :u)
# #     @eval (struct $Sel{T} x::T end; $sym(x) = $Sel(x))
# # end


# findnextind(x, t::Token, i) = (j = _findnext(x, t, i); isnothing(j) ? length(t.data) : j)


# # |(a, t::Token) = isfirst(a, t)

# @inline is(a) = Base.Fix1(is, a)
# @inline is(f::Function) = f

# # @inline is(a::T, b::T) where {T} = a == b
# # @inline is(a::Char, b::UInt8) = UInt8(a) == b
# # @inline is(a::UInt8, b::Char) = Char(a) == b
# # @inline is(f::Function, b) = f(b)
# # @inline is(n::Not, b) = !is(n.x, b)
# # @inline is(x::Bool, b) = x

# # # Fallback methods
# # @inline isfirst(x, d) = is(x, first(d))
# # @inline isfirst(o::UseStringView, d) = isfirst(o.x, StringView(d))
# # @inline _findnext(arg, data, i) = findnext(is(arg), data, i)
# # @inline _findnext(o::UseStringView, data, i) = findnext(is(o.x), StringView(data), nextind(StringView(data), i - 1))

# # # AbstractString/AbstractVector/Tuple
# # @inline isfirst(x::AbstractString, d) = isfirst(codeunits(x), d)
# # @inline _findnext(x::AbstractString, d, i) = _findnext(codeunits(x), d, i)
# # @inline isfirst(x::AbstractVector, d) = all(is(x)(d) for (x,d) in zip(x,d))
# # @inline _findnext(x::AbstractVector, d, i) = findnext(x, d, i)
# # @inline isfirst(t::Tuple, d) = isempty(t) ? true : is(t[1], d[1]) && isfirst(tail(t), @view(d[2:end]))
# # @inline _findnext(t::Tuple, d, i) = (j = _findnext(t[1], d,i); isfirst(t, @view(d[j:end])) ? j + length(t) - 1 : _findnext(t, d, j + 1))

# # # Selectors that only work with _findnext
# # @inline _findnext(j::Int, d, i) = j
# # @inline _findnext(f::First, d, i) = (rng = _findnext(f.x, d, i); isnothing(rng) ? nothing : first(rng))
# # @inline _findnext(f::Last, d, i) =  (rng = _findnext(f.x, d, i); isnothing(rng) ? nothing : last(rng))
# # @inline _findnext(b::Before, d, i) = (j = _findnext(b.x, d, i); isnothing(j) ? length(d) : j - 1)
# # @inline function _findnext(u::Unescaped, d, i)
# #     skip = false
# #     for j in i:length(d)
# #         c = Char(d[j])
# #         c == u.x && !skip && return j
# #         skip = c == '\\' ? !skip : false
# #     end
# # end


# # #-----------------------------------------------------------------------------# Rule
# # struct Rule{F, T}
# #     from::F
# #     to::T
# # end
# # Base.show(io::IO, r::Rule) = print(io, styled"{highlight:$(r.from)} --> {highlight:$(r.to)}")
# # -->(a, b) = Rule(a, b)

# # isfirst(r::Rule, t) = isfirst(r.from, t)
# # _findnext(r::Rule, data, i) = _findnext(r.to, data, i)

# # |(t::Token{T,K,S}, r::Rule) where {T, K, S} = t | r.to

# # const STRING = '"' --> u('"')
# # const NUMBER = ∈(b"-0123456789") --> ≺(∉(b"-+eE.0123456789"))
# # const ASCII_WHITESPACE = ∈(b" \t\r\n") --> ≺(∉(b" \t\r\n"))
# # const ASCII_LETTERS = ∈(UInt8('A'):UInt8('z')) --> ≺(∉(UInt8('A'):UInt8('z')))

# # const LETTERS = 𝑠(isletter) --> ≺(𝑠(!isletter))
# # const WHITESPACE = 𝑠(isspace) --> ≺(𝑠(!isspace))

# # #-----------------------------------------------------------------------------# @tryrule(s)
# # function tryrule_ex(kind, rule, token=:(n))
# #     :($rule | $token && return $token | $kind | $rule)
# # end

# # macro tryrule(kind, rule, token=:(n))
# #     esc(tryrule_ex(kind, rule, token))
# # end
# # macro tryrules(ex, token=:(n))
# #     Base.remove_linenums!(ex)
# #     exprs = map(ex.args) do x
# #         tryrule_ex(QuoteNode(x.args[1]), x.args[2], token)
# #     end
# #     e = Expr(:block, exprs..., :(token_error($token)))
# #     return esc(e)
# # end

# # #-----------------------------------------------------------------------------# JSONTokens
# # struct JSONTokens{T <: Data} <: TokenIterator{T, Symbol, Nothing}
# #     data::T
# # end

# # next(o::JSONTokens, n::Token) = @tryrules begin
# #     curly_open      = '{' --> 1
# #     curly_close     = '}' --> 1
# #     square_open     = '[' --> 1
# #     square_close    = ']' --> 1
# #     comma           = ',' --> 1
# #     colon           = ':' --> 1
# #     var"true"       = 't' --> 4
# #     var"false"      = 'f' --> 5
# #     var"null"       = 'n' --> 4
# #     string          = STRING
# #     number          = NUMBER
# #     whitespace      = ASCII_WHITESPACE
# # end

# # #-----------------------------------------------------------------------------# HTMLTokens
# # struct HTMLTokens{T <: Data} <: TokenIterator{T, Symbol, Nothing}
# #     data::T
# # end
# # next(t::HTMLTokens, n::Token) = @tryrules begin
# #     whitespace = WHITESPACE
# #     comment     = "<!--"    --> →("-->")
# #     doctype     = "<!"      --> '>'
# #     close_tag   = "</"      --> '>'
# #     open_tag    = "<"       --> '>'
# #     text        = true      --> ≺(←("</"))
# # end

# # #-----------------------------------------------------------------------------# XMLTokens
# # struct XMLTokens{T <: Data} <: TokenIterator{T, Symbol, Bool}
# #     data::T
# # end
# # next(o::XMLTokens, n::Token) = @tryrules begin
# #     whitespace = WHITESPACE
# #     processing_instruction  = "<?" --> '>'
# #     comment                 = "<!--" --> →("-->")
# #     cdata                   = "<![" --> →("]]>")
# #     close_tag               = "</" --> '>'
# #     open_tag                = "<" --> '>'
# #     text                    = true --> ≺('<')
# # end

# # #-----------------------------------------------------------------------------# DelimFileTokens
# # struct DelimFileTokens{T <: Data} <: TokenIterator{T, Symbol, Int}
# #     data::T
# #     delim::Char
# # end
# # DelimFileTokens(data) = DelimFileTokens(data, ',')
# # init(::DelimFileTokens, state::Int) = 1
# # next(o::DelimFileTokens, n::Token) = @tryrules begin
# #     whitespace  = WHITESPACE
# #     delim       = o.delim --> 1
# #     colon       = ':' --> 1
# #     string      = STRING
# #     word        = LETTERS
# #     number      = NUMBER
# #     unknown     = true --> 1
# # end

# # #-----------------------------------------------------------------------------# CharFunTokens
# # struct CharFunTokens{T, F, K} <: TokenIterator{T, K, Nothing}
# #     data::T
# #     f::F
# # end
# # function CharFunTokens(data::T, f::F) where {T, F}
# #     K = typeof(f(first(StringView(data))))
# #     CharFunTokens{T, F, K}(data, f)
# # end
# # function next(o::CharFunTokens, n::Token)
# #     k = o.f(first(StringView(n)))
# #     return n | k | ≺(𝑠(x -> o.f(x) != k))
# # end



end  # module
