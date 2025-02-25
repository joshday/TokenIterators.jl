module Tokenizers

using StyledStrings, StringViews, Parsers

import Base: findfirst, findnext, !

export Token, Next, JSONTokens, HTMLTokens, XMLTokens, debug

const Data = AbstractVector{UInt8}

function debug(itr)
    out = eltype(typeof(itr))[]
    try
        for x in itr
            push!(out, x)
        end
    catch ex
        return out
    end
    return out
end

#-----------------------------------------------------------------------------# TokenLike
abstract type TokenLike end
StringViews.StringView(t::TokenLike) = StringView(view(t))
Base.first(o::TokenLike) = o[1]
Base.getindex(o::TokenLike, x::Int) = view(o)[x]
Base.getindex(o::TokenLike, x) = @view view(o)[x]
Base.length(o::TokenLike) = length(view(o))

#-----------------------------------------------------------------------------# Token
struct Token{T <: Data, K} <: TokenLike
    data::T
    kind::K
    i::Int
    j::Int
end
(t::Token)(kind) = Token(t.data, kind, t.j + 1, t.j + 1)
(t::Token)(kind, j::Integer) = Token(t.data, kind, t.j + 1, j)
Base.view(t::Token) = view(t.data, t.i:t.j)


#-----------------------------------------------------------------------------# Next
struct Next{T} <: TokenLike
    token::T
end
Base.view(n::Next) = @view(n.token.data[n.token.j+1:end])
(n::Next)(kind) = n.token(kind)
(n::Next)(kind, x) = n.token(kind, n.token.j + findnext(x, n, 2))
(n::Next)(kind, w::Integer, x) = n.token(kind, n.token.j + findnext(x, n, w + 1))

# `a ≺ b` means "a begins with b"
≺(x::UInt8, n::Next) = view(n)[1] == x
≺(x::Data, n::Next) = all(x -> x[1] == x[2], zip(view(n), x))
≺(x::Char, n::Next) = ≺(UInt8(x), n::Next)
≺(f::Function, n::Next) = f(first(view(n)))
≺(x::Set{UInt8}, n::Next) = view(n)[1] in x
for f in (:isspace, :isdigit, :islowercase, :isuppercase)
    @eval ≺(::typeof($f), n::Next) = $f(first(StringView(n)))
    @eval findnext(::typeof($f), n::Next, i) = findnext($f, StringView(n), i)
    @eval findnext(::typeof(!$f), n::Next, i) = findnext(!$f, StringView(n), i)
end

struct First{T} x::T end
struct Last{T} x::T end
struct Before{T} x::T end
struct After{T} x::T end

# Returns index in terms of view(::Next).  `i` input will always be > 1
findnext(x::UInt8, n::Next, i) = findnext(==(x), view(n), i)
findnext(x::Int, n::Next, i) = x
findnext(x::Char, n::Next, i) = findnext(UInt8(x), n, i)
findnext(x::First{<:Data}, n::Next, i) = first(findnext(x.x, view(n), i))
findnext(x::Last{<:Data}, n::Next, i) = last(findnext(x.x, view(n), i))
findnext(x::Last{Regex}, n::Next, i) = last(findnext(x.x, StringView(n), i))
findnext(x::Last{Set{UInt8}}, n::Next, i) = last(findnext(b -> b in x.x, view(n), i))
findnext(x::Before{UInt8}, n::Next, i) = (j = findnext(==(x.x), x, i); isnothing(j) ? length(n) : j - 1)
findnext(x::Before{<:Data}, n::Next, i) = (j = findnext(x.x, view(n), i); isnothing(j) ? length(n) : first(j) - 1)
findnext(x::Before{<:Function}, n::Next, i) = (j = findnext(x.x, n, i); isnothing(j) ? length(n) : j - 1)
findnext(x::Regex, n::Next, i) = only(findnext(x, StringView(n), i))
findnext(f::Function, n::Next, i) = findnext(f, view(n), i)
findnext(s::Set{UInt8}, n::Next, i) = (j = findnext(x -> x ∉ s, view(n), i); isnothing(j) ? length(n) : j - 1)
findnext(x::After, n::Next, i) = findnext(x.x, n, i) + 1

struct Unescaped; c::Char; end
function findnext(o::Unescaped, n::Next, i)
    skip = false
    for (j, x) in enumerate(view(n))
        j == 1 && continue
        Char(x) == o.c && !skip && return j
        skip = x == UInt8('\\')
    end
end


#-----------------------------------------------------------------------------# Token show
format(x) = styled"{bright_green:$(repr(x))}"
format(x::Integer) = (s=replace(string(x), r"(\d)(?=(\d{3})+(?!\d))" => s"\1_"); styled"{bright_black:$s}")
format(x::Bool) = x ? styled"{bright_green:true}" : styled"{bright_black:false}"
format(x::Symbol) = styled"{bright_blue:$(repr(x))}"

function Base.show(io::IO, t::Token)
    s = styled"$(format(t.kind)) $(format(t.i)):$(format(t.j)) "
    n = displaysize(io)[2] - length(s)
    _s2 = styled"{bright_cyan:$(escape_string(StringView(t.data[t.i:t.j])))}"
    s2 = length(_s2) > n ? styled"{inverse:$(_s2[1:n-1])}…" : styled"{inverse:$_s2}"
    print(io, s, s2)
end

#-----------------------------------------------------------------------------# Tokenizer
abstract type Tokenizer{D <: AbstractVector{UInt8}, K} end
init(::Type{Symbol}) = :init
init(::Type{T}) where {T} = typemin(T)

Base.IteratorSize(::Type{T}) where {T <: Tokenizer} = Base.SizeUnknown()
Base.eltype(::Type{T}) where {D, K, T <: Tokenizer{D, K}} = Token{D, K}

function Base.iterate(o::T, t::Token = Token(o.data, init(K), 1, 0)) where {D, K, T <: Tokenizer{D, K}}
    t.j == length(t.data) && return nothing
    n = next(o, Next(t))
    return n, n
end

function Base.show(io::IO, o::T) where {T <: Tokenizer}
    print(io, T.name.name, ':')
    n = min(displaysize(io)[1] - 5, 10)
    toks = first(o, n)
    print(io, map(x -> styled"\n  $x", toks)...)
    toks[end].j == length(toks[end].data) || print(io, styled"\n   {bright_green:⋮}")
end

#-----------------------------------------------------------------------------# JSONTokens
struct JSONTokens{T} <: Tokenizer{T, Symbol}
    data::T
end
function next(::JSONTokens, n::Next)
    isspace ≺ n && return n(:ws, Before(!isspace))
    '{' ≺ n && return n(:bracket_open)
    '}' ≺ n && return n(:bracket_close)
    '[' ≺ n && return n(:square_open)
    ']' ≺ n && return n(:square_close)
    ',' ≺ n && return n(:comma)
    ':' ≺ n && return n(:colon)
    't' ≺ n && return n(:var"true", 4)
    'f' ≺ n && return n(:var"false", 5)
    'n' ≺ n && return n(:null, 4)
    '"' ≺ n && return n(:string, Unescaped('"'))
    Set(b"-0123456789") ≺ n && return n(:number, Set(b"-+eE.0123456789"))
    n(:unknown)
end

#-----------------------------------------------------------------------------# HTMLTokens
struct HTMLTokens{T} <: Tokenizer{T, Symbol}
    data::T
end
function next(::HTMLTokens, n::Next)
    b"<!-" ≺ n && return n(:comment, Last(b"-->"))
    b"<!" ≺ n && return n(:doctype, '>')
    b"</" ≺ n && return n(:tagclose, '>')
    b"<script" ≺ n && return n(:scriptopen, '>')
    b"<" ≺ n && return n(:tagopen, '>')
    n.token.kind == :scriptopen && return n(:scripttext, Before(b"</script>"))
    n.token.kind == :tagopen && return n(:text, Before(b"<"))
    isspace ≺ n && return n(:ws, Before(!isspace))
    n(:unknown)
end

#-----------------------------------------------------------------------------# XMLTokens
struct XMLTokens{T} <: Tokenizer{T, Symbol}
    data::T
end
function next(::XMLTokens, n::Next)
    isspace ≺ n && return n(:ws, Before(!isspace))
    b"<?" ≺ n && return n(:xmldecl, Last(b"?>"))
    b"<!--" ≺ n && return n(:comment, Last(b"-->"))
    b"<![" ≺ n && return n(:cdata, Last(b"]]>"))
    b"<!" ≺ n && return n(:doctype, '>')
    b"</" ≺ n && return n(:tagclose, '>')
    b"<" ≺ n && return n(:tagopen, '>')
    n.token.kind == :tagopen && return n(:text, Before(b"<"))
    n(:unknown)
end

# function next(o::HTMLTokens, t::Token)
#     if o.state == :standard
#         if nextbyte(t) == '<'
#         end
#     elseif o.state == :in_tag
#     elseif o.state == :in_script
#     elseif o.state == :in_pre
#     end

#     if isnext(b"<", t)
#         isnext(b"<!-", t) && return t(:comment, b"<!--", Last(b"-->"))
#         isnext(b"<!", t) && return t(:doctype, b"<!", Last(b">"))
#         isnext(b"</", t) && return t(:tagclose, b"</", Last(b">"))
#         isnext(b"<script", t) && (o.state=:in_script; return t(:scriptopen, b"<script", Last(b">")))
#         o.state = :in_tag
#         return t(:tagopen, findnext(x -> !isletter(Char(x)), t.data, t.j+2) - 1)
#     elseif isnext(b">", t)
#         o.state = :standard
#         return t(:tagclose)
#     end
#     # isnext(b"<scrip", t) && return t(:scriptopen, b"<script", Last(b">"))
#     # t.kind == :scriptopen && return t(:scripttext, 0x0, Last(b"</script>"))
#     # (t → b"<!") && return t(:doctype, findnext(==(UInt8('>')), t.data, t.j + 2))
#     # (t → b"<script") && return t(:scriptopen, findnext(==(UInt8('>')), t.data, t.j + 7))
#     # t.kind == :scriptopen && return t(:scripttext, findnext(Before(b"</script>"), t.data, t.j + 1))
#     # (t → b"</") && return t(:tagclose, findnext(==(UInt8('>')), t.data, t.j + 8))
#     # (t → b"<") && return t(:tagopen, findnext(==(UInt8('>')), t.data, t.j + 2))
#     # return t(:text, findnext(==(UInt8('<')), t.data, t.j + 2) - 1)
#     return t(:unknown)
# end

# →(t::Token, x::AbstractVector{UInt8}) = nextbytes(t, length(x)) == x


# #-----------------------------------------------------------------------------# Rule
# struct Rule{T, S}
#     token_start::T
#     token_end::S
# end
# Rule(x::AbstractVector{UInt8}) = Rule(x, length(x))
# Rule(x::AbstractString) = Rule(codeunits(x))
# Rule(x::Char) = isascii(x) ? Rule(UInt8(x)) : Rule(codeunits("$x"))
# Rule(x::UInt8) = Rule(x, 1)
# Rule(r::Rule) = r
# →(a, b) = Rule(a, b)
# Base.show(io::IO, r::Rule) = print(io, styled"{blue:Rule($(repr(r.token_start)), $(repr(r.token_end)))}")
# Base.print(io::IO, r::Rule) = show(io, r)

# # Domain interface
# init_kind(r::Rule) = false
# rules(r::Rule) = (true => r, false => !r)
# !(r::Rule) = Rule(Not(r.token_start), Before(r.token_start))

# #-----------------------------------------------------------------------------# TokenDomain
# abstract type TokenDomain end
# Base.show(io::IO, o::TokenDomain) =print(io, styled"{bright_green:$(name(o))}")

# name(o::T) where {T} = replace(string(T),  "Tokenizers." => "")
# init_kind(x) = :init
# init_state(x) = nothing
# rules(x) = rules(typeof(x))
# rules(x::Type) = map(Rule, _rules(x))

# #-----------------------------------------------------------------------------# Token
# struct Token{T, D <: AbstractVector{UInt8}, V, K, S}
#     domain::T
#     data::D
#     postview::V
#     kind::K
#     i::Int
#     j::Int
#     state::S
# end
# Token(T::Type, data) = Token(T(), data)
# Token(x, data) = Token(x, data, @view(data[1:end]), init_kind(x), 1, 0, init_state(x))

# (t::Token)(kind, j) = Token(t.domain, t.data, @view(t.data[j+1:end]), kind, t.j + 1, j, t.state)
# (t::Token)(kind, j, state) = Token(t.domain, t.data, @view(t.data[j+1:end]), kind, t.j + 1, j, state)

# Base.sizeof(t::Token) = t.j - t.i + 1
# Base.view(t::Token) = view(t.data, t.i:t.j)
# StringViews.StringView(t::Token) = StringView(view(t))

# format(x) = x
# format(x::Integer) = (s=replace(string(x), r"(\d)(?=(\d{3})+(?!\d))" => s"\1_"); styled"{bright_yellow:$s}")
# format(x::Bool) = x ? styled"{bright_green:true}" : styled"{bright_black:false}"
# format(x::Symbol) = styled"{bright_blue:$(repr(x))}"

# function Base.show(io::IO, t::Token)
#     s = styled"$(t.domain) $(format(t.i))-$(format(t.j)) " *
#     styled"{magenta:($(Base.format_bytes(sizeof(t))))} $(format(t.kind)) "
#     n = displaysize(io)[2] - length(s)
#     _s2 = escape_string(StringView(t))
#     s2 = length(_s2) > n ? styled"{inverse:$(_s2[1:n-1])}…" : styled"{inverse:$_s2}"
#     print(io, s, s2)
# end

# Base.IteratorSize(::Type{T}) where {T <: Token} = Base.SizeUnknown()
# Base.eltype(::Type{T}) where {T <: Token} = T
# Base.isdone(::Token, t::Token) = t.j == length(t.data)

# function Base.iterate(itr::Token, t=itr)
#     Base.isdone(itr, t) && return nothing
#     n = next(t)
#     return n, n
# end
# next(t::Token{R}) where {R <: Rule} = isnext(t.domain, t) ? t(true, findnext(t.domain, t)) : t(false, findnext(!t.domain, t))

# rules(o::Type{Token{T,D,V,K,S}}) where {T,D,V,K,S} = rules(T)

# @generated function next(t::Token)
#     r = rules(t)
#     exprs = [:(isnext(r[$i], t) && return t($(QuoteNode(k)), findnext(r[$i], t), t.state)) for (i,k) in enumerate(keys(r))]
#     quote
#         r = rules(t)
#         $(exprs...)
#     end
# end

# # next(t::Token) = next(t, rules(t.domain))
# # function next(t::Token, rules)
# #     length(rules) == 0 && return nothing
# #     x = first(rules)
# #     if isnext(r, t)
# #         return t(kind, findnext(r, t), t.state)
# #     else
# #         return next(t, @inbounds rules[2:end])
# #     end
# # end

# #-----------------------------------------------------------------------------# isnext
# isnext(x::Rule, t::Token) = isnext(x.token_start, t)

# isnext(x::UInt8, t::Token) = t.data[t.j+1] == x
# isnext(x::AbstractVector{UInt8}, t::Token) = all(x -> x[1] == x[2], zip(t.postview, x))
# isnext(x::AbstractString, t::Token) = isnext(codeunits(x), t)
# isnext(x::Char, t::Token) = isascii(x) ? isnext(UInt8(x), t) : isnext(codeunits("$x"), t)
# isnext(x::Set{UInt8}, t::Token) = t.data[t.j+1] in x

# #-----------------------------------------------------------------------------# findnext
# findnext(x::Rule, t::Token) = findnext(x.token_end, t, t.j + 2)

# findnext(x::UInt8, t::Token, i) = findnext(==(x), t.data, i)
# findnext(x::AbstractVector{UInt8}, t::Token, i) = findnext(x, t.data, i)
# findnext(x::Int, t::Token, i) = t.j + x
# findnext(x::Char, t::Token, i) = findnext(==(x), StringView(t.data), i)


# #-----------------------------------------------------------------------------# State
# struct State{S,T}
#     state::S
#     x::T
# end
# isnext(s::State, t::Token) = t.state == s.state && isnext(s.x, t)
# findnext(s::State, t::Token, i) = findnext(s.x, t, i)

# #-----------------------------------------------------------------------------# Not
# struct Not{T} x::T end
# isnext(n::Not, t::Token) = !isnext(n.x, t)
# findnext(n::Not, t::Token, i) = findnext(!=(n.x), t, i)

# #-----------------------------------------------------------------------------# CharFun
# struct FirstChar{F} f::F end
# isnext(fc::FirstChar, t::Token) = fc.f(first(StringView(t.postview)))
# findnext(fc::FirstChar, t::Token, i) = findnext(fc.f, StringView(t.data), i)

# #-----------------------------------------------------------------------------# Unescaped
# struct Unescaped{X} end
# Unescaped(x) = Unescaped{UInt8(x)}()
# Base.show(io::IO, o::Unescaped{X}) where {X} = print(io, "Unescaped('$(Char(X))')")
# function findnext(o::Unescaped{X}, t::Token, i) where {X}
#     skip = false
#     for j in i:length(t.data)
#         x = t.data[j]
#         x == X && !skip && return j
#         skip = x == UInt8('\\')
#     end
# end

# STRING = Rule('"', Unescaped('"'))
# WHITESPACE = Rule(FirstChar(isspace), Before(FirstChar(!isspace)))

# #-----------------------------------------------------------------------------# JSONDomain
# struct JSONDomain <: TokenDomain end

# _rules(::Type{JSONDomain}) = (
#     bracket_open = '{',
#     bracket_close = '}',
#     square_open = '[',
#     square_close = ']',
#     comma = ',',
#     colon = ':',
#     string = STRING,
#     number = Set(b"-0123456789") → Set(b"-+eE.0123456789"),
#     var"true" = 't' → 4,
#     var"false" = 'f' → 5,
#     null = 'n' → 4,
#     ws = WHITESPACE
# )

# # #-----------------------------------------------------------------------------# HTMLDomain
# # struct HTMLDomain <: TokenDomain end
# # init_state(::HTMLDomain) = :init

# # rules(::Type{HTMLDomain}) = (
# #     comment = "<!-" → Last("-->"),
# #     doctype = "<!" → '>',
# #     scriptopen = "<script" → State(:in_script, '>'),
# #     scripttext = State(:in_script, true) → State(:init, Before('>')),
# #     scriptclose = "</script" → '>',
# #     tagclose = "</" → '>',
# #     tagopen = '<' → '>',
# #     ws = WHITESPACE,
# #     other = CharFun(!isspace) → Before(CharFun(x -> isspace(x) || x == '<'))
# # )

end  # module
