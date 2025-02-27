module Tokenizers

using StyledStrings, StringViews

import Base: startswith, findfirst, findnext, ∈

export Token, Next, JSONTokens, HTMLTokens,
    next, next_data, rules, findj, isfirst, isbyte, nbytes, →, ¬

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
struct Token{D, T <: Data, K, S}
    domain::D
    data::T
    kind::K
    i::Int
    j::Int
    state::S
end
Token(D::Type, data) = Token(D(), data)
Token(domain, data) = Token(domain, data, initkind(domain), 1, 0, initstate(domain))
next_data(t::Token) = Token(t.domain, t.data, t.kind, t.j + 1, length(t.data), t.state)
rules(t::Token) = rules(t.domain)

function Base.show(io::IO, ::MIME"text/plain", t::Token)
    format(x::Int) = replace(string(x), r"(\d)(?=(\d{3})+(?!\d))" => s"\1_")
    rng = styled"{bright_black:$(format(t.i)):$(format(t.j)) len=$(length(t))} $(t.state)"
    s = styled"{bright_magenta:$(t.domain)} $rng {bright_cyan:$(t.kind)} "
    n = displaysize(io)[2] - length(s) - 1
    _s2 = styled"{inverse:{bright_cyan:$(escape_string(StringView(t.data[t.i:t.j])))}}"
    s2 = _s2[1:min(n, end)] * (length(_s2) > n ? styled"{bright_cyan:…}" : "")
    print(io, s, s2)
end
Base.show(io::IO, t::Token) = show(io, MIME("text/plain"), t)


initkind(x) = :init
initstate(x) = nothing

# AbstractArray interface
Base.length(t::Token) = t.j - t.i + 1
Base.size(t::Token) = (length(t), )
Base.view(t::Token) = @view t.data[t.i:t.j]
Base.getindex(t::Token, i::Integer) = view(t)[i]
Base.lastindex(t::Token) = length(t)

# Iteration
Base.IteratorSize(::Type{T}) where {T <: Token } = Base.SizeUnknown()
Base.eltype(::Type{T}) where {T <: Token} = T
function Base.iterate(t::Token{D,T,K,S}, state::Token{D,T,K,S}=t) where {D,T,K,S}
    state.j == length(t.data) && return nothing
    n = next_data(state)
    n = next(n)
    return n, n
end
next(n::Token) = next(n, keys(rules(n)), values(rules(n)))
function next(n::Token, ks, vs)
    length(ks) == 0 && return nothing
    k = first(ks)
    v = →(first(vs))
    out = v ∈ n ? n(k, v) : nothing
    isnothing(out) ? next(n, Base.tail(ks), Base.tail(vs)) : out
end

#-----------------------------------------------------------------------------# Rule
struct Rule{F, T, S}
    from::F
    to::T
    statechange::S
end
Base.show(io::IO, r::Rule) = print(io, "($(r.from) → $(r.to))")
→(a, b, c) = Rule(a, b, c)
→(a, b) = Rule(a, b, identity)
→(a) = Rule(a, nbytes(a), identity)
→(r::Rule) = r
→(r::Rule, s::State) = Rule(r.from, r.to, s)
→(r::Rule, s) = Rule(r.from, r.to, State(s))
∈(r::Rule, t::Token) = isfirst(r, t)

struct State{T} x::T end
(s::State)(::Any) = s.x

function (t::Token)(kind, rule)
    Token(t.domain, t.data, kind, t.i, t.i - 1 + findj(rule, t, 1 + nbytes(rule)), rule.statechange(t.state))
end

# Using Rule as the Token domain
macro tryrule(t, k, r)
    esc(:(→($r) ∈ $t && return $t($k, →($r))))
end
macro dorule(t, k, r)
    esc(:(return →($r) ∈ $t ? $t($k, →($r)) : error(string("Rule Failed", $r))))
end

initkind(r::Rule) = false
function next(n::Token{R}) where {R <: Rule}
    @tryrule n true n.domain
    @tryrule n false Unknown() → ≺(n.domain.from)
end

#-----------------------------------------------------------------------------# operators
struct UseStringView{T}  # \sinewave
    x::T
end
∿(x) = UseStringView(x)

struct Not{T}  # \neg
    x::T
end
¬(x) = Not(x)

struct Before{T}  # \prec
    x::T
end
≺(x) = Before(x)


# Selector interface
# nbytes(x) --> Number of bytes the starting match consumes
# isfirst(x, data) --> Is x the first match in data?
# findj(x, data, i) --> Find the next match of x in data starting at i
# isbyte(x, b::UInt8) --> Does x "match" b?

nbytes(x) = 1
nbytes(x::AbstractVector{UInt8}) = length(x)

isfirst(x, t::Token) = isfirst(x, view(t))
findj(x, t::Token, i) = findj(x, view(t), i)

# Something to use as a fallback
struct Unknown end
isbyte(::Unknown, ::UInt8) = true


isbyte(x::UInt8, b::UInt8) = x == b
isbyte(x::Char, b::UInt8) = UInt8(x) == b
isbyte(x::Function, b::UInt8) = x(b)
isbyte(x::Not{UInt8}, b::UInt8) = x.x != b
isbyte(x::Not{Char}, b::UInt8) = UInt8(x.x) != b

# isfirst for Data
isfirst(r::Rule, d::Data) = isfirst(r.from, d)
isfirst(x, d::Data) = isbyte(x, d[1])
isfirst(x::Data, d::Data) = all(x -> x[1] == x[2], zip(d, x))
isfirst(t::Tuple, d::Data) = isempty(t) ? true : isfirst(t[1], d) && isfirst(Base.tail(t), @view(d[2:end]))

# isfirst for SData
isfirst(x::UseStringView, d::Data) = isfirst(x.x, StringView(d))
isfirst(x::Char, s::SData) = s[1] == x
isfirst(x::Union{Regex, SData}, s::SData) = startswith(x, s)
isfirst(f::Function, s::SData) = f(s[1])

# findj for Data
findj(r::Rule, d::Data, i) = findj(r.to, d, i)
findj(x::UInt8, d::Data, i) = findnext(==(x), d, i)
findj(x::Char, d::Data, i) = findj(UInt8(x), d, i)
findj(x::Integer, ::Data, i) = x
findj(x::Not{UInt8}, d::Data, i) = findnext(!=(x.x), d, i)
findj(x::Not{Char}, d::Data, i) = findj(Not(UInt8(x.x)), d, i)
function findj(tup::Tuple, data::Data, i)
    j = findj(tup[end], data, i)
    v = @view(data[j -  length(tup) + 1:end])
    isfirst(tup, v) ? j : findj(tup, data, j + 1)
end
findj(b::Before, data::Data, i) = (j = findj(b.x, data, i); isnothing(j) ? length(data) : j - 1)
function findj(b::Before{<:Data}, data::Data, i)
    rng = findnext(b.x, data, i)
    isnothing(rng) ? length(data) : first(rng) - 1
end

# findj for SData
findj(x::UseStringView, d::Data, i) = findj(x.x, StringView(d), i)
findj(x::Char, s::SData, i) = findnext(x, s, i)
findj(x::SData, s::SData, i) = last(findnext(x, s, i))

findj(f::Function, d::Union{Data, SData}, i) = findnext(f, d, i)


#-----------------------------------------------------------------------------# JSONTokens
struct JSONTokens end

function next(n::Token{JSONTokens})
    @tryrule n :curly_open '{'
    @tryrule n :curly_close '}'
    @tryrule n :square_open '['
    @tryrule n :square_close ']'
    @tryrule n :comma ','
    @tryrule n :colon ':'
    @tryrule n :var"true" 't' → 4
    @tryrule n :var"false" 'f' → 5
    @tryrule n :var"null" 'n' → 4
    @tryrule n :string '"' → (¬('\\'), '"')
    @tryrule n :number (∈(b"-0123456789")) → ≺(∉(b"-+eE.0123456789"))
    @tryrule n :whitespace (∈(b"\t\n\r ")) → ≺(∉(b"\t\n\r "))
    @dorule n :unkown Unknown()
end

#-----------------------------------------------------------------------------# HTMLTokens
struct HTMLTokens end
initstate(::HTMLTokens) = :init

function next(n::Token{HTMLTokens})
    @tryrule n :close_tag_start b"</"
    n.kind == :close_tag_start && @dorule n :close_tag_name ∿(isletter) → ≺('>')
    n.kind == :close_tag_name && @dorule n :close_tag_end '>'
    if n.state in (:in_tag, :in_script_tag)
        @tryrule n :attr_name ∿(isletter) → ≺(∈(b" ="))
        @tryrule n :attr_value '"' → (¬('\\'), '"')
        @tryrule n :open_tag_end Rule('>', 1, State(n.state == :in_script_tag ? :in_script : :init))
    elseif n.state == :in_script
        @dorule n :script_text Rule(Unknown(), ≺(b"</script>"), State(:init))
    end
    @tryrule n :comment b"<!--" → b"-->"
    @tryrule n :doctype b"<!" → '>'
    @tryrule n :open_tag_start '<'
    if n.kind == :open_tag_start
        @tryrule n :open_tag_name Rule(b"script", ≺(∈(b" >")), State(:in_script_tag))
        @dorule n :open_tag_name Rule(∿(isletter), ≺(∈(b" >")), State(:in_tag))
    end
    @tryrule n :whitespace ∿(isspace) → ≺(∿(!isspace))
    @tryrule n :equals '='
    @tryrule n :text Unknown() → ≺('<')
    @dorule n :unknown Unknown()
end


# Token{T, K, S}(data) where {T, K, S} = Token(data, initkind(K), 1, 0, initstate(S))
# Token(data, kind) = Token(data, kind, 1, 0)

# # Get Token for remainder of data
# next(t::Token{T,K,S}) where {T,K,S} = Token(t.data, t.kind, t.j + 1, length(t.data), t.state)

# # Replace (kind, j, state)
# (t::Token{T,K,S})(k::K) where {T,K,S} = Token(t.data, k, t.i, t.i, t.state)
# (t::Token{T,K,S})(k::K, j::Int) where {T,K,S} = Token(t.data, k, t.i, t.i + j - 1, t.state)



# show

# # Token mini-DSL
# initkind(::Type{Symbol}) = :init
# initkind(::Type{Bool}) = false
# initstate(::Type{Symbol}) = :init
# initstate(::Type{Vector{T}}) where {T} = T[]
# initstate(::Type{Nothing}) = nothing


# isfirst(x::UInt8, data::Data) = data[1] == x
# isfirst(x::AbstractVector{UInt8}, data::Data) = all(x -> x[1] == x[2], zip(data, x))
# isfirst(x::Char, data::Data) = isfirst(UInt8(x), data)
# isfirst(x::Set{UInt8}, data::Data) = data[1] in x
# isfirst(x::Function, data::Data) = x(data[1])

# isfirst(x::Char, data::SData) = data[1] == x
# isfirst(x::Union{Regex, SData}, data::SData) = startswith(x, data)
# isfirst(f::Function, data::SData) = f(data[1])


# findj(x::UInt8, data::Data, i) = findnext(==(x), data, i)
# findj(x::Data, data::Data, i) = last(findnext(x, data, i))
# findj(x::Int, data::Data, i) = x
# findj(x::Set, data::Data, i) = (j = findnext(b -> b ∉ x, data, i); isnothing(j) ? length(data) : j - 1)

# findj(x::Char, data::SData, i) = findnext(x, data, i)
# findj(x::SData, data::SData, i) = last(findnext(x, data, i))
# findj(x::Regex, data::SData, i) = last(findnext(x, data, i))

# struct Until{T}
#     x::T
# end
# Base.show(io::IO, u::Until) = print(io, "Until($(u.x))")
# findj(u::Until, data, i) = (j = findnext(u.x, data, i); isnothing(j) ? length(data) : j - 1)
# findj(u::Until{<:Data}, data, i) = (j = first(findnext(u.x, data, i)); isnothing(j) ? length(data) : j - 1)

# struct CharSet{T}
#     x::T
# end
# isfirst(o::CharSet, data) = any(x -> isfirst(x, data), o.x)
# findj(o::CharSet, data, i) = (j = findnext(x -> x ∉ o.x, data, i); isnothing(j) ? length(data) : j - 1)

# struct Unescaped{T}
#     x::T
# end
# function findj(u::Unescaped{T}, data, i) where {T}
#     skip = false
#     for j in i:length(data)
#         x = data[j]
#         x == u.x && !skip && return j
#         skip = x == T('\\')
#     end
#     error("No unescaped character $(repr(u.x)) found in data")
# end

# #-----------------------------------------------------------------------------# Token mini-DSL
# ∘(x, t::Token) = isfirst(x, t)
# ∘(s::State, t::Token) = s.state == t.state
# ⊚(x, t::Token) = isfirst(x, StringView(t))

# ∘(t::Token{T,K,S}, k::K) where {T,K,S} = Token(t.data, k, t.i, t.j, t.state)
# ∘(t::Token{T,K,S}, x) where {T,K,S} = Token(t.data, t.kind, t.i, t.i + findj(x, t, 2) - 1, t.state)
# ∘(t::Token{T,K,S}, s::AbstractState) where {T,K,S} = Token(t.data, t.kind, t.i, t.j, s(t.state))
# ⊚(t::Token{T,K,S}, x) where {T,K,S} = Token(t.data, t.kind, t.i, t.i + findj(x, StringView(t), 2) - 1, t.state)
# ⋆(t::Token{T,K,S}, k::K) where {T,K,S} = Token(t.data, k, t.i, t.i, t.state)

# #-----------------------------------------------------------------------------# Tokenizer
# abstract type Tokenizer{T <: Token} end
# Base.IteratorSize(::Type{T}) where {T <: Tokenizer} = Base.SizeUnknown()
# Base.eltype(::Type{Tok}) where {T,Tok <: Tokenizer{T}} = T
# init(o::Tokenizer{T}) where {T} = T(o.data)
# Base.isdone(::Tokenizer, t::Token) = t.j == length(t.data)

# Base.show(io::IO, o::Tokenizer{T}) where {T} = print(io, styled"{bright_green:$(typeof(o).name.name)}: $(summary(o.data))")

# function Base.iterate(o::Tokenizer{T}, state = init(o)) where {T}
#     Base.isdone(o, state) && return nothing
#     next(o, state)
# end


# #-----------------------------------------------------------------------------# JSONTokens
# struct JSONTokens{T} <: Tokenizer{Token{T, Symbol, Nothing}}
#     data::T
# end

# rules(::JSONTokens) = (
#     curly_open = '{',
#     curly_close = '}',
#     square_open = '[',
#     square_close = ']',
#     comma = ',',
#     colon = ':',
#     var"true" = 't' → 4,
#     var"false" = 'f' → 5,
#     var"null" = 'n' → 4,
#     string = '"' → Unescaped('"'),
#     number = CharSet(b"-0123456789") → CharSet(b"-+eE.0123456789"),
#     whitespace = CharFun(isspace)
# )

# # function next(o::JSONTokens, token::Token)
# #     n = next(token)
# #     n = isspace ⊚ n ? n ∘ :whitespace ⊚ Until(!isspace) :
# #         '{' ∘ n ? n ⋆ :curly_open :
# #         '}' ∘ n ? n ⋆ :curly_close :
# #         '[' ∘ n ? n ⋆ :square_open :
# #         ']' ∘ n ? n ⋆ :square_close  :
# #         ',' ∘ n ? n ⋆ :comma :
# #         ':' ∘ n ? n ⋆ :colon :
# #         't' ∘ n ? n ∘ :var"true" ∘ 4 :
# #         'f' ∘ n ? n ∘ :var"false" ∘ 5 :
# #         'n' ∘ n ? n ∘ :null ∘ 4 :
# #         '"' ∘ n ? n ∘ :string ∘ Unescaped(UInt8('"')) :
# #         CharSet(b"-0123456789") ∘ n ? n ∘ :number ∘ CharSet(b"-+eE.0123456789") :
# #         n ⋆ :unknown
# #     return n, n
# # end

# #-----------------------------------------------------------------------------# HTMLTokens
# struct HTMLTokens{T} <: Tokenizer{Token{T, Symbol, Symbol}}
#     data::T
# end
# function next(o::HTMLTokens, t::Token)
#     n = next(t)
#     n = b"<!-" ∘ n ? n ∘ :comment ∘ b"-->" :
# #         b"<!" ∘ n ? n ∘ :doctype ∘ UInt8('>') :
# #         b"</" ∘ n ? n ∘ :tagclose ∘ UInt8('>') :
# #         b"<script" ∘ n ? n ∘ :scriptopen ∘ UInt8('>') :
# #         t.kind == :scriptopen ? n ∘ :scripttext ∘ Until(b"</script>") :
# #         b"<style" ∘ n ? n ∘ :styleopen ∘ UInt8('>') :
# #         t.kind == :styleopen ? n ∘ :styletext ∘ Until(b"</style>") :
# #         b"<" ∘ n ? n ∘ :tagopenstart ⊚ Until(!isletter) :
# #         b">" ∘ n ? n ⋆ :tagopenend :
# #         isspace ⊚ n ? n ∘ :ws ⊚ Until(!isspace) :
# #         isletter ⊚ n ? n ∘ :word ⊚ Until(x -> !(isletter(x) || x=='-')) :
# #         UInt8('"') ∘ n ? n ∘ :string ∘ Unescaped(UInt8('"')) :
# #         UInt8('=') ∘ n ? n ⋆ :equal :
#         n ⋆ :unknown
#     return n, n
# end


# JSONTokens(data::Data) = JSONTokens(Token(data, :init))
# function next(o::JSONTokens, n::Token)
#     n = isspace ⊚ n ? n ∘ :ws ⊚ Until(!isspace) :
#         '{' ∘ n ? n ⋆ :bracket_open :
#         '}' ∘ n ? n ⋆ :bracket_close  :
#         '[' ∘ n ? n ⋆ :square_open :
#         ']' ∘ n ? n ⋆ :square_close :
#         ',' ∘ n ? n ⋆ :comma :
#         ':' ∘ n ? n ⋆ :colon :
#         't' ∘ n ? n ∘ :var"true" ∘ 4 :
#         'f' ∘ n ? n ∘ :var"false" ∘ 5 :
#         'n' ∘ n ? n ∘ :null ∘ 4 :
#         '"' ∘ n ? n ∘ :string ∘ Unescaped(UInt8('"')) :
#         Set(b"-0123456789") ∘ n ? n ∘ :number ∘ Set(b"-+eE.0123456789") :
#         n ⋆ :unknown
#     return n, n
# end


# struct JSONTokens{T} <: Tokenizer{T, Symbol}
#     data::T
# end
# function next(o::JSONTokens, n::Token)
#     out = isspace ⊚ n ? n ∘ :ws ⊚ Until(!isspace) :
#         '{' ∘ n ? n ⋆ :bracket_open :
#         '}' ∘ n ? n ⋆ :bracket_close  :
#         '[' ∘ n ? n ⋆ :square_open :
#         ']' ∘ n ? n ⋆ :square_close :
#         ',' ∘ n ? n ⋆ :comma :
#         ':' ∘ n ? n ⋆ :colon :
#         't' ∘ n ? n ∘ :var"true" ∘ 4 :
#         'f' ∘ n ? n ∘ :var"false" ∘ 5 :
#         'n' ∘ n ? n ∘ :null ∘ 4 :
#         '"' ∘ n ? n ∘ :string ∘ Unescaped(UInt8('"')) :
#         Set(b"-0123456789") ∘ n ? n ∘ :number ∘ Set(b"-+eE.0123456789") :
#         n ⋆ :unknown
#     return out
# end


#-----------------------------------------------------------------------------# HTMLTokens
# struct HTMLTokens{T} <: Tokenizer{T, Symbol, Symbol}
#     data::T
# end
# function next(::HTMLTokens, n::Token)
#     # b"<!-" / n && return n / :comment / b"-->"
#     # b"<!" / n && return n / :doctype / UInt8('>')
# #     b"</" ≺ n && return n(:tagclose, '>')
# #     b"<script" ≺ n && return n(:scriptopen, '>')
# #     b"<" ≺ n && return n(:tagopen, '>')
# #     n.token.kind == :scriptopen && return n(:scripttext, Before(b"</script>"))
# #     n.token.kind == :tagopen && return n(:text, Before(b"<"))
# #     isspace ≺ n && return n(:ws, Before(!isspace))
#     # n / :unknown / 1
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
