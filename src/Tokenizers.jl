module Tokenizers

using StyledStrings, StringViews, Parsers

import Base: startswith, findfirst, findnext, !, /, //

export Token, Next, JSONTokens, HTMLTokens, XMLTokens, debug, next, Rule, DataRule, SDataRule, BSplit, Until, Unescaped,
    ≻, ⪼, ≺, ⪻

#-----------------------------------------------------------------------------# utils
const Data = AbstractVector{UInt8}
const SData = AbstractString

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


#-----------------------------------------------------------------------------# Token
struct Token{T <: Data, K, S} <: Data
    data::T
    kind::K
    i::Int
    j::Int
    state::S
end

# Get Token for remainder of data
next(t::Token{T,K,S}) where {T,K,S} = Token(t.data, t.kind, t.j + 1, length(t.data), t.state)

# Replace (kind, j, state)
# (t::Token{T,K,S})(k::K, j::Int = 1, s::S=t.state) where {T,K,S} = Token(t.data, k, t.i, t.i + j - 1, s)

# AbstractArray interface
Base.length(t::Token) = t.j - t.i + 1
Base.size(t::Token) = (length(t), )
Base.view(t::Token) = @view t.data[t.i:t.j]
Base.getindex(t::Token, i::Integer) = view(t)[i]

# show
format(x) = styled"{bright_green:$(repr(x))}"
format(x::Integer) = (s=replace(string(x), r"(\d)(?=(\d{3})+(?!\d))" => s"\1_"); styled"{bright_black:$s}")
format(x::Bool) = x ? styled"{bright_green:true}" : styled"{bright_black:false}"
format(x::Symbol) = styled"{bright_blue:$(repr(x))}"
function Base.show(io::IO, ::MIME"text/plain", t::Token)
    s = styled"$(format(t.kind)) $(format(t.i)):$(format(t.j)) "
    n = displaysize(io)[2] - length(s) - 1
    _s2 = styled"{inverse:{bright_cyan:$(escape_string(StringView(t.data[t.i:t.j])))}}"
    s2 = _s2[1:min(n, end)] * (length(_s2) > n ? styled"{bright_cyan:…}" : "")
    print(io, s, s2)
end
Base.show(io::IO, t::Token) = show(io, MIME("text/plain"), t)

# Token mini-DSL
initkind(::Type{Symbol}) = :init
initstate(::Type{Symbol}) = :init
initkind(::Type{Bool}) = false
initstate(::Type{Bool}) = false
initstate(::Type{Nothing}) = nothing


isfirst(x::UInt8, data::Data) = data[1] == x
isfirst(x::AbstractVector{UInt8}, data::Data) = all(x -> x[1] == x[2], zip(data, x))
isfirst(x::Char, data::Data) = isfirst(UInt8(x), data)
isfirst(x::Set{UInt8}, data::Data) = data[1] in x
isfirst(x::Function, data::Data) = x(data[1])

isfirst(x::Char, data::SData) = data[1] == x
isfirst(x::Union{Regex, SData}, data::SData) = startswith(x, data)
isfirst(f::Function, data::SData) = f(data[1])

≺(x, t::Token) = isfirst(x, t)
⪻(x, t::Token) = isfirst(x, StringView(t))

findj(x::UInt8, data::Data, i) = findnext(==(x), data, i)
findj(x::Data, data::Data, i) = last(findnext(x, data, i))
findj(x::Int, data::Data, i) = x
findj(x::Set, data::Data, i) = (j = findnext(b -> b ∉ x, data, i); isnothing(j) ? length(data) : j - 1)

findj(x::Char, data::SData, i) = findnext(x, data, i)
findj(x::SData, data::SData, i) = last(findnext(x, data, i))
findj(x::Regex, data::SData, i) = last(findnext(x, data, i))

struct Until{T}
    x::T
end
Base.show(io::IO, u::Until) = print(io, "Until($(u.x))")
findj(u::Until, data, i) = (j = findnext(u.x, data, i); isnothing(j) ? length(data) : j - 1)

struct Unescaped{T}
    x::T
end
function findj(u::Unescaped{T}, data, i) where {T}
    skip = false
    for j in i:length(data)
        x = data[j]
        x == u.x && !skip && return j
        skip = x == T('\\')
    end
    error("No unescaped character $(repr(u.x)) found in data")
end


/(t::Token{T,K,S}, kind::K) where {T,K,S} = Token(t.data, kind, t.i, t.j, t.state)
/(t::Token{T,K,S}, x::Int) where {T,K,S} = Token(t.data, t.kind, t.i, t.i + x - 1, t.state)
/(t::Token{T,K,S}, x) where {T,K,S} = Token(t.data, t.kind, t.i, t.i + findj(x, t, 2) - 1, t.state)
//(t::Token, x) = Token(t.data, t.kind, t.i, t.i + findj(x, StringView(t), 2) - 1, t.state)

# /(t::Token, x::Int) = t(t.kind, x, t.state)
# /(t::Token, x) = t(t.kind, findj(x, t, 2), t.state)
# //(t::Token, x) = t(t.kind, findj(x, StringView(t), 2), t.state)


# ≻(t::Token, x::Tuple) = t(x[1], findj(get(x, 2, 1), t, 2), get(x, 3, t.state))
# ⪼(t::Token, x::Tuple) = t(x[1], findj(get(x, 2, 1), StringView(t), 2), get(x, 3, t.state))

# ⪼(t::Token, x::Tuple) = t(x[1], findnext(x[2], t, 2))

# ≻(t::Token{T,K,S}, kind::K) where {T, K, S} = t(kind)
# ≻(t::Token{T, K, S}, x::Pair{K, Int}) where {T, K, S} = t(x.first, x.second)
# ≻(t::Token{T,K,S}, x::Pair{K, <:Function}) where {T, K, S} = t(x.first, findnext(x.second, t, t.i + 1))

# ⪼(t::Token{T,K,S}, x::Pair{K, <:Function}) where {T,K,S} = t(x.first, findnext(x.second, StringView(t), t.i + 1))

#-----------------------------------------------------------------------------# Tokenizer
abstract type Tokenizer{T <: Data, K, S} end
Base.IteratorSize(::Type{T}) where {T <: Tokenizer} = Base.SizeUnknown()
Base.eltype(::Type{Tok}) where {T, K, S, Tok <: Tokenizer{T, K, S}} = Token{T, K, S}
init(t::Tokenizer{T, K, S}) where {T, K, S} = Token(t.data, initkind(K), 1, 0, initstate(S))


function Base.iterate(o::Tokenizer, t = init(o))
    t.j == length(t.data) && return nothing
    n = next(o, next(t))
    return n, n
end


# #-----------------------------------------------------------------------------# Rule
# struct Rule{A_OP <: Union{typeof(≺), typeof(⪻)}, A, B_OP <: Union{typeof(≻), typeof(⪼)}, B}
#     a::Base.Fix1{A_OP, A}
#     b::Base.Fix2{B_OP, B}
# end
# function Base.show(io::IO, r::Rule{A_OP, A, B_OP, B}) where {A_OP, A, B_OP, B}
#     print(io, styled"Rule: {bright_green:$(r.a.x) $(r.a.f) n && return n $(r.b.f) $(r.b.x))}")
# end

# ≺(x) = Base.Fix1(≺, x)
# ⪻(x) = Base.Fix1(⪻, x)
# ≻(x) = Base.Fix2(≻, x)
# ⪼(x) = Base.Fix2(⪼, x)

# #-----------------------------------------------------------------------------# BSplit
# struct BSplit{T, R} <: Tokenizer{T, Bool, Nothing}
#     data::T
#     rule::R
# end
# function next((; rule)::BSplit, n::Token)
#     if rule.a(n)
#         rule.b.op(n, (true, findj))
#     else
#     end
# end

# struct Split{T, A, B} <: Tokenizer{T, Bool, Nothing}
#     data::T
#     a::A
#     b::B
# end
# function next(o::Split, n::Token)
#     o.a(n) && return o.b(n)
# end

#-----------------------------------------------------------------------------# JSONTokens
struct JSONTokens{T} <: Tokenizer{T, Symbol, Symbol}
    data::T
end
function next(o::JSONTokens, n::Token)
    isspace ⪻ n && return n // Until(!isspace) / :ws
    '{' ≺ n && return n / :bracket_open / 1
    '}' ≺ n && return n / :bracket_close / 1
    '[' ≺ n && return n / :square_open / 1
    ']' ≺ n && return n / :square_close / 1
    ',' ≺ n && return n / :comma / 1
    ':' ≺ n && return n / :colon / 1
    't' ≺ n && return n / :var"true" / 4
    'f' ≺ n && return n / :var"false" / 5
    'n' ≺ n && return n / :null / 4
    '"' ≺ n && return n / :string / Unescaped(UInt8('"'))
    # Set(b"-0123456789") ≺ n && return n / :number / Set(b"-+eE.0123456789")
    n / :unknown / 1
end




# StringViews.StringView(t::Token) = StringView(view(t))
# Base.getindex(t::Token, i::Int) = view(t)[i]
# Base.getindex(t::Token, x) = @view view(t)[x]
# Base.iterate(t::Token, state...) = iterate(view(t), state...)
# Base.length(t::Token) = length(view(t))
# Base.eltype(::Type{<:Token}) = UInt8
# Base.IteratorSize(::Type{<:Token}) = Base.HasLength()
# Base.keys(t::Token) = keys(view(t))
# Base.nextind(t::Token, i) = i + 1


# startswith(r::Rule, t::Token) = startswith(r.token_start, t)
# startswith(x::UInt8, t::Token) = x == t[1]
# startswith(x::AbstractVector{UInt8}, t::Token) = x == t[1:length(x)]

# findnext(r::Rule, t::Token, i) = findnext(r.token_end, t, i)
# findnext(x::UInt8, t::Token, i) = findnext(==(x), t, i)

# struct Last{T} x::T end; findnext(x::Last, t::Token, i) = last(findnext(x.x, t, i))

# startswith(x::UInt8, t::Token) = x == t[1]

# isnext(r::Rule, t::Token) = isnext(r.token_start, t)


# next(t::Token, ::Type{UInt8}, i::Int=1) = t.data[t.j + i]
# next(t::Token, ::Type{Char}, i::Int=1) = StringView(viewnext(t))

# ≺(t::Token, r::Rule) = ≺(t, r.token_start)
# ≺(t, x::UInt8) = next(t, UInt8) == x
# ≺(t, x::AbstractVector{UInt8}) = all(x -> x[1] == x[2], zip(viewnext(t), x))


# next(t::Token, r::Rule) = Token(t.data, r.kind, t.j + 1, findnext(r.token_end, t, t.j + 1 + nbytes(r.token_start)))

# next(t::Token{T, K, S}, kind::K, j::Integer, state::S=t.state) = Token(t.data, kind, t.j + 1, j, state)




# #-----------------------------------------------------------------------------# Next
# struct Next{T} <: TokenLike
#     token::T
# end
# Base.view(n::Next) = @view(n.token.data[n.token.j+1:end])
# (n::Next)(kind) = n.token(kind)
# (n::Next)(kind, x) = n.token(kind, n.token.j + findnext(x, n, 2))
# (n::Next)(kind, w::Integer, x) = n.token(kind, n.token.j + findnext(x, n, w + 1))

# # `a ≺ b` means "a begins with b"
# ≺(x::UInt8, n::Next) = view(n)[1] == x
# ≺(x::Data, n::Next) = all(x -> x[1] == x[2], zip(view(n), x))
# ≺(x::Char, n::Next) = ≺(UInt8(x), n::Next)
# ≺(f::Function, n::Next) = f(first(view(n)))
# ≺(x::Set{UInt8}, n::Next) = view(n)[1] in x
# for f in (:isspace, :isdigit, :islowercase, :isuppercase)
#     @eval ≺(::typeof($f), n::Next) = $f(first(StringView(n)))
#     @eval findnext(::typeof($f), n::Next, i) = findnext($f, StringView(n), i)
#     @eval findnext(::typeof(!$f), n::Next, i) = findnext(!$f, StringView(n), i)
# end

# struct First{T} x::T end
# struct Last{T} x::T end
# struct Before{T} x::T end
# struct After{T} x::T end

# # Returns index in terms of view(::Next).  `i` input will always be > 1
# findnext(x::UInt8, n::Next, i) = findnext(==(x), view(n), i)
# findnext(x::Int, n::Next, i) = x
# findnext(x::Char, n::Next, i) = findnext(UInt8(x), n, i)
# findnext(x::First{<:Data}, n::Next, i) = first(findnext(x.x, view(n), i))
# findnext(x::Last{<:Data}, n::Next, i) = last(findnext(x.x, view(n), i))
# findnext(x::Last{Regex}, n::Next, i) = last(findnext(x.x, StringView(n), i))
# findnext(x::Last{Set{UInt8}}, n::Next, i) = last(findnext(b -> b in x.x, view(n), i))
# findnext(x::Before{UInt8}, n::Next, i) = (j = findnext(==(x.x), x, i); isnothing(j) ? length(n) : j - 1)
# findnext(x::Before{<:Data}, n::Next, i) = (j = findnext(x.x, view(n), i); isnothing(j) ? length(n) : first(j) - 1)
# findnext(x::Before{<:Function}, n::Next, i) = (j = findnext(x.x, n, i); isnothing(j) ? length(n) : j - 1)
# findnext(x::Regex, n::Next, i) = only(findnext(x, StringView(n), i))
# findnext(f::Function, n::Next, i) = findnext(f, view(n), i)
# findnext(s::Set{UInt8}, n::Next, i) = (j = findnext(x -> x ∉ s, view(n), i); isnothing(j) ? length(n) : j - 1)
# findnext(x::After, n::Next, i) = findnext(x.x, n, i) + 1




# #-----------------------------------------------------------------------------# Tokenizer
# abstract type Tokenizer{D <: AbstractVector{UInt8}, K} end
# init(::Type{Symbol}) = :init
# init(::Type{T}) where {T} = typemin(T)

# Base.IteratorSize(::Type{T}) where {T <: Tokenizer} = Base.SizeUnknown()
# Base.eltype(::Type{T}) where {D, K, T <: Tokenizer{D, K}} = Token{D, K}

# function Base.iterate(o::T, t::Token = Token(o.data, init(K), 1, 0)) where {D, K, T <: Tokenizer{D, K}}
#     t.j == length(t.data) && return nothing
#     n = next(o, Next(t))
#     return n, n
# end

# function Base.show(io::IO, o::T) where {T <: Tokenizer}
#     print(io, T.name.name, ':')
#     n = min(displaysize(io)[1] - 5, 10)
#     toks = first(o, n)
#     print(io, map(x -> styled"\n  $x", toks)...)
#     toks[end].j == length(toks[end].data) || print(io, styled"\n   {bright_green:⋮}")
# end

# #-----------------------------------------------------------------------------# JSONTokens
# struct JSONTokens{T} <: Tokenizer{T, Symbol}
#     data::T
# end
# function next(::JSONTokens, n::Next)
#     isspace ≺ n && return n(:ws, Before(!isspace))
#     '{' ≺ n && return n(:bracket_open)
#     '}' ≺ n && return n(:bracket_close)
#     '[' ≺ n && return n(:square_open)
#     ']' ≺ n && return n(:square_close)
#     ',' ≺ n && return n(:comma)
#     ':' ≺ n && return n(:colon)
#     't' ≺ n && return n(:var"true", 4)
#     'f' ≺ n && return n(:var"false", 5)
#     'n' ≺ n && return n(:null, 4)
#     '"' ≺ n && return n(:string, Unescaped('"'))
#     Set(b"-0123456789") ≺ n && return n(:number, Set(b"-+eE.0123456789"))
#     n(:unknown)
# end

# #-----------------------------------------------------------------------------# HTMLTokens
# struct HTMLTokens{T} <: Tokenizer{T, Symbol}
#     data::T
# end
# function next(::HTMLTokens, n::Next)
#     b"<!-" ≺ n && return n(:comment, Last(b"-->"))
#     b"<!" ≺ n && return n(:doctype, '>')
#     b"</" ≺ n && return n(:tagclose, '>')
#     b"<script" ≺ n && return n(:scriptopen, '>')
#     b"<" ≺ n && return n(:tagopen, '>')
#     n.token.kind == :scriptopen && return n(:scripttext, Before(b"</script>"))
#     n.token.kind == :tagopen && return n(:text, Before(b"<"))
#     isspace ≺ n && return n(:ws, Before(!isspace))
#     n(:unknown)
# end

# #-----------------------------------------------------------------------------# XMLTokens
# struct XMLTokens{T} <: Tokenizer{T, Symbol}
#     data::T
# end
# function next(::XMLTokens, n::Next)
#     isspace ≺ n && return n(:ws, Before(!isspace))
#     b"<?" ≺ n && return n(:processinginstruction, Last(b"?>"))
#     b"<!--" ≺ n && return n(:comment, Last(b"-->"))
#     b"<![" ≺ n && return n(:cdata, Last(b"]]>"))
#     b"<!" ≺ n && return n(:doctype, '>')
#     b"</" ≺ n && return n(:tagclose, '>')
#     b"<" ≺ n && return n(:tagopen, '>')
#     n.token.kind == :tagopen && return n(:text, Before(b"<"))
#     n(:unknown)
# end

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
