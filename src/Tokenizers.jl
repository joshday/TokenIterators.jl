module Tokenizers

using StyledStrings, StringViews, Parsers

import Base: findfirst, findnext

export tokens, file_tokens, →,
    Token, TokenRule,
    # Domains
    BSplit, Partition, Rule

#-----------------------------------------------------------------------------# utils
limit_string(s::AbstractString, n::Int) = length(s) > n ? s[1:n-1] * "…" : s

format(x::Int) = replace(string(x), r"(\d)(?=(\d{3})+(?!\d))" => s"\1_")

firstchar(x::AbstractString)::Char = first(x)
firstchar(x::AbstractVector{UInt8})::Char = first(StringView(x))

struct CharFun{F} f::F end
Base.show(io::IO, o::CharFun) = print(io, "CharFun($(o.f))")

#-----------------------------------------------------------------------------# TokenDomain
abstract type TokenDomain end
name(o::T) where {T<:TokenDomain} = replace(string(T),  "Tokenizers." => "")
init(::TokenDomain) = :init
Base.show(io::IO, o::TokenDomain) =print(io, styled"{bright_green:$(name(o))}")

#-----------------------------------------------------------------------------# Token
struct Token{T, D <: AbstractVector{UInt8}, K}
    domain::T
    data::D
    kind::K
    i::Int
    j::Int
end
Token(D::Type, data) = Token(D(), data)
Token(domain::TokenDomain, data) = Token(domain, data, init(domain), 1, 0)
(t::Token)(kind, i::Int, j::Int) = Token(t.domain, t.data, kind, i, j)
Base.view(t::Token) = view(t.data, t.i:t.j)
Base.String(t::Token) = String(view(t))
Base.length(t::Token) = t.j - t.i + 1
function Base.show(io::IO, t::Token)
    k = styled"{bright_green:$(t.domain)} {bright_yellow:$(format(t.i)):$(format(t.j))} {bright_cyan:$(repr(t.kind))} "
    n = displaysize(io)[2] - length(k)
    s = limit_string(escape_string(String(t)), n)
    print(io, k, styled"{inverse:$s}")
end

#-----------------------------------------------------------------------------# Next
struct Next{D, V, SV, K}
    data::D
    view::V
    sview::SV
    i::Int
    prev::K
end
function Next(t::Token)
    Next(t.data, @view(t.data[t.j+1:end]), StringView(@view(t.data[t.j+1:end])), t.j + 1, t.kind)
end
Base.String(s::Next) = String(s.view)
function Base.show(io::IO, t::Next)
    k = styled"{bright_red:Next} {bright_yellow:$(format(t.i))} {bright_cyan:prev=$(repr(t.prev))} "
    n = displaysize(io)[2] - length(k) - 1
    print(io, k, styled"{bright_black:$(limit_string(repr(String(t)), n))}")
end

Base.first(n::Next) = first(n.view)
Base.first(n::Next, x::Integer) = first(n.view, x)

#-----------------------------------------------------------------------------# Token iteration
Base.IteratorSize(::Type{T}) where {T <: Token} = Base.SizeUnknown()
Base.eltype(::Type{T}) where {T <: Token} = T

Base.isdone(t::Token, n::Next) = n.i > length(t.data)

function Base.iterate(t::Token, n=Next(t))
    Base.isdone(t, n) && return nothing
    t2 = next(t.domain, n)
    return t2, Next(t2)
end

@noinline function next(o::TokenDomain, n::Next)
    kind, j = _next(o, n)
    Token(o, n.data, kind, n.i, n.i + j - 1)
end

#-----------------------------------------------------------------------------# Rule
# A rule is 1) How to Identify if the data is a given token, and 2) How to find the last index
struct Rule{T, S}
    id::T
    idx::S
end
Base.show(io::IO, r::Rule) = print(io, show_as(r.id), " ━ ", show_as(r.idx))

show_as(x) = x

~(a, b) = isfirst(a, b)
→(a, b) = findfirst(b, a)

isfirst(r::Rule, n::Next) = isfirst(r.id, n)
findfirst(r::Rule, n::Next) = findfirst(r.idx, n)

isfirst(r::Rule) = Base.Fix1(isfirst, r)
findfirst(r::Rule) = Base.Fix1(findfirst, r)

# UInt8
isfirst(x::UInt8, n::Next) = first(n.view) == x
findfirst(x::UInt8, n::Next) = findfirst(==(x), n.view)

# Char
isfirst(x::Char, n::Next) = first(n.view) == UInt8(x)
findfirst(x::Char, n::Next) = findfirst(==(UInt8(x)), n.view)
show_as(x::Char) = repr(x)

# AbstractVector{UInt8}
isfirst(x::AbstractVector{UInt8}, n::Next) = all(x -> x[1] == x[2], zip(n.view, x))
findfirst(x::AbstractVector{UInt8}, n::Next) = findfirst(x, n.view)
show_as(x::AbstractVector{UInt8}) = "b\"$(StringView(x))\""

# AbstractString
isfirst(x::AbstractString, n::Next) = startswith(n.sview, x)
findfirst(x::AbstractString, n::Next) = findfirst(x, n.sview)
show_as(x::AbstractString) = repr(x)

# Set
isfirst(x::Set, n::Next) = any(x -> isfirst(x, n), x)
findfirst(x::Set{Char}, n::Next) = findfirst(in(x), n.sview)
findfirst(x::Set{UInt8}, n::Next) = findfirst(in(x), n.view)
show_as(x::Set) = "⟨" * join([show_as(x) for x in x], ", ") * "⟩"

# Function
isfirst(f::Function, n::Next) = f(first(n.view))
findfirst(f::Function, n::Next) = findfirst(f, n.view)
show_as(f::Function) = "Function($f)"

# CharFun
isfirst(cf::CharFun, n::Next) = cf.f(first(n.sview))
findfirst(cf::CharFun, n::Next) = findfirst(cf.f, n.sview)

struct Unescaped
    char::Char
    escape::Char
    Unescaped(x::Char, esc::Char='\\') = new(x, esc)
end
function findfirst(o::Unescaped, s::Next)
    (; view) = s
    skip = false
    for j in 2:length(view)
        x = view[j]
        x == UInt8(o.char) && !skip && return j
        skip = x == UInt8(o.escape)
    end
end

struct First{T} x::T end
Base.show(io::IO, f::First) = print(io, "First($(show_as(f.x)))")
findfirst(f::First, n::Next) = first(findfirst(f.x, n))

struct Last{T} x::T end
Base.show(io::IO, l::Last) = print(io, "Last($(show_as(l.x)))")
findfirst(l::Last, n::Next) = last(findfirst(l.x, n))

struct Before{T} x::T end
Base.show(io::IO, b::Before) = print(io, "Before($(show_as(b.x)))")
findfirst(b::Before, n::Next) = (j = findfirst(b.x, n); isnothing(j) ? length(n.view) : j - 1)


STRING_RULE = Rule('"', Unescaped('"'))

macro trytok(n::Next, r::Rule, kind)
    esc(quote
        isfirst($(ex[1]), $n) && return $(ex[2]) => findfirst($(ex[1]), $n)
    end)
end

#-----------------------------------------------------------------------------# BSplit
# Binary split based on a single Rule
struct BSplit{R <: Rule} <: TokenDomain
    rule::R
end
Base.show(io::IO, b::BSplit{R}) where {R} = print(io, "BSplit: $(b.rule)")
init(o::BSplit) = false
function _next(o::BSplit, n::Next)
    isfirst(o.rule, n) ? true => findfirst(o.rule.idx, n) : false => findfirst(Before(o.rule.id), n)
end

#-----------------------------------------------------------------------------# Partition





    # = (n ~ o.rule) ? true => (n → o.rule) : false => (n → o.rule) - 1



# struct After{T} x::T end
# findfirst(a::After, n::Next) = findfirst(a.x, n) + 1

# token_end(s::Next, c::Char) = token_end(s, UInt8(c))  # Assumes char is ASCII

# token_end(s::Next, f::Function) = findnext(f, s.data, s.i)

# istok(s::Next, r::Rule) = istok(s, r.id)
# token_end(s::Next, r::Rule) = token_end(s, r.idx)


# string_rule = Rule('"', Unescaped('"'))


# istok(s::Next, x) = istok(s.view, x)
# istok(s::AbstractVector{UInt8}, x) = istok(first(s), x)

# istok(s::UInt8, x::UInt8) = s == x
# istok(s::UInt8, x::Char) = Char(s) == x
# istok(s::UInt8, x::Set) = any(x -> istok(s, x), x)
# istok(s::UInt8, x::Function) = hasmethod(x, (UInt8,)) ? x(s) : x(Char(s))

# #-----------------------------------------------------------------------------# Partition
# struct Partition{T} <: TokenDomain
#     x::T
# end
# init(o::Partition{<:Base.Callable}) = hasmethod(o.x, (UInt8,)) ? o.x(0x00) : o.x(Char(0x00))
# init(o::Partition{<:Set}) = false

# get_val(o::Partition{<:Base.Callable}, x::UInt8) = hasmethod(o.x, (UInt8,)) ? o.x(x) : o.x(Char(x))
# get_val(o::Partition{<:Set}, x::UInt8) = istok(o.x, x)

# function next(o::Partition, state::Next)
#     (; data, i) = state
#     val = get_val(o, data[i])
#     for j in i+1:length(data)
#         get_val(o, data[j]) != val && return Token(o, data, val, i, j-1)
#     end
#     return Token(o, data, val, i, length(data))
# end






# #-----------------------------------------------------------------------------# StringTokens
# struct StringTokens <: TokenDomain end

# function next(o::StringTokens, state::Next)
#     if string_rule.istok(state)
#         j = string_rule.lastindex(state)
#         return Token(o, state.data, :STRING, state.i, j)
#     else

#     end
# end

# #-----------------------------------------------------------------------------# Rule
# struct FirstCharEquals x::Char end
# (o::FirstCharEquals)(state::Next) = firstchar(state.view) == o.x



# # Rules for identifying a token
# struct Rule{T, S}
#     istok::T   # passed to `startswith(istok, state)`
#     lastindex::S  # passed to `lastindex(lastindex,)`
# end

# string_rule = Rule(FirstCharEquals('"'), x -> ascii_findnext_unescaped('"', x.data, x.i + 1))


# # #-----------------------------------------------------------------------------# Match
# # struct Match{T, S}
# #     id::T
# #     findindex::S
# # end
# # tmatch(data, m::Match) = tmatch(data, m.id)

# →(data, x) = tokenstartswith(data, x)

# # Does `data` begin with token identifier `x`?
# tokenstartswith(data, x::UInt8) = data[1] == x
# tokenstartswith(data, x::Char) = tokenstartswith(data, codeunits(string(x)))
# tokenstartswith(data, x::AbstractString) = tokenstartswith(data, codeunits(x))
# tokenstartswith(data, x::AbstractVector{UInt8}) = all(x -> x[1] == x[2], zip(data, x))
# tokenstartswith(data, x::Function) = x(first(data))
# tokenstartswith(data, x::Set) = any(x -> tokenstartswith(data, x), x)
# tokenstartswith(data, x::UnitRange) = any(x -> tokenstartswith(data, x), x)





# #-----------------------------------------------------------------------------# Tokenizer
# abstract type Tokenizer{T, K, I} end


# function Base.iterate(t::Tokenizer{T, K, I}, state=init_state(t)) where {T, K, I}
#     Base.isdone(t, state) && return nothing
# end



# #-----------------------------------------------------------------------------# HTMLTokens
# struct HTMLTokens{T <: AbstractVector{UInt8}} <: Tokenizer{T, Symbol, typeof(1:2)}
#     data::T
# end
# Token(t::H, kind, idx) = Tok

# kindtype(::Type{<:HTMLTokens}) = Symbol
# position_type(::Type{<: HTMLTokens}) = Tuple{Int, Int}


# Token(TT::Type, data) = Token(TT(), data, :init, 1, 0, :none)
# Token(tokenizer, data) = Token(data, tokenizer, :init, 1, 0, :none)
# (t::Token)(kind, i, j) = Token(t.tokenizer, t.data, kind, i, j, t.context)
# (t::Token)(kind, i, j, ctx) = Token(t.tokenizer, t.data, kind, i, j, ctx)

# Base.isdone(t::Token, state::Token) = state.j == length(t.data)
# Base.eltype(::Type{T}) where {T <: Token} = T
# Base.IteratorEltype(::Type{T}) where {T <: Token} = Base.HasEltype()
# Base.IteratorSize(::Type{T}) where {T <: Token} = Base.SizeUnknown()

# Base.String(t::Token) = String(t.data[t.i:t.j])

# function Base.iterate(t::Token, state=t)
#     Base.isdone(t, state) && return nothing
#     n = next(state)
#     return n, n
# end
# function Base.show(io::IO, t::Token{T, D}) where {T, D}
#     type = styled"{bright_black:$(T.name.name)} "
#     t.kind == :init && return print(io, type, styled"{bright_yellow:$(summary(t.data))}")
#     idx = styled"{bright_yellow:$(format(t.i))-$(format(t.j))} "
#     kind = styled"{bright_cyan:$(repr(t.kind))} "
#     s = String(t)
#     n = displaysize(io)[2] - length(type) - length(idx) - length(kind)
#     s = length(s) > n ? s[1:n-1] * "…" : s
#     s = replace(s, '\n' => "\\n")
#     print(io, type, idx, kind, styled"{bright_black:$s}")
# end


# next_char(t::Token) = t.j + 1, Char(t.data[t.j + 1])
# next_data(t::Token) = t.j + 1, @view t.data[t.j + 1:end]


# subtokens(t::Token) = subtokens(t, Val(t.kind))
# subtokens(t::Token, ::Val) = typeof(t)[]

# #-----------------------------------------------------------------------------# match utils
# findnext_first(pattern, data, i) = first(findnext(pattern, data, i))
# findnext_last(pattern, data, i) = last(findnext(pattern, data, i))
# findnext_before(pattern, data, i) = findnext_first(pattern, data, i) - 1
# findnext_after(pattern, data, i) = findnext_last(pattern, data, i) + 1

# _startswith(data, x) = all(x -> x[1] == x[2], zip(data, x))

# #-----------------------------------------------------------------------------# ASCIITokens
# struct ASCIITokens end
# function next(t::Token{ASCIITokens})
#     i, c = next_char(t)
#     t(isascii(c) ? :ASCII : :NOT_ASCII, i, i)
# end

# #-----------------------------------------------------------------------------# HTMLTokens
# struct HTMLTokens end

# function next(t::Token{HTMLTokens})
#     i, v = next_data(t)
#     _startswith(v, b"</") && return t(:CLOSE_TAG, i, findnext_last(b">", t.data, i))
#     _startswith(v, b"<!-") && return t(:COMMENT, i, findnext_last(b"-->", t.data, i))
#     _startswith(v, b"<!d") && return t(:DOCTYPE, i, findnext_last(b">", t.data, i))
#     _startswith(v, b"<!D") && return t(:DOCTYPE, i, findnext_last(b">", t.data, i))
#     _startswith(v, b"<scri") && return t(:SCRIPT, i, findnext_last(b"</script>", t.data, i))
#     Char(v[1]) == '<' && return t(:OPEN_TAG, i, findnext_last(b">", t.data, i))
#     return t(:TEXT, i, findnext_before(b"<", t.data, i))
# end

# function name(t::Token{HTMLTokens})
#     i = findnext(x -> isletter(Char(x)), t.data, t.i)
#     isnothing(i) || i > t.j && return nothing
#     j = findnext_before(x -> !isletter(Char(x)), t.data, i)
#     return t(:NAME, i, j)
# end








# function subtokens(t::Token{HTMLTokens}, ::Val{:OPEN_TAG})
#     data = view(t.data, t.i:t.j)
#     idx = t.i - 1
#     out = typeof(t)[]
#     # @assert t.kind ==
#     i = findfirst(x -> isletter(Char(x)), data)
#     j = findnext(x -> !isletter(Char(x)), data, i) - 1
#     push!(out, t(:TAG_NAME, idx + i, idx + j))
#     while true
#         i = findnext(x -> isletter(Char(x)), t.data, j + 1)
#         isnothing(i)
#         i > t.j &&  break
#         j = findnext_before(x -> !isletter(Char(x)), t.data, i)
#         key = t(:KEY, i, j)
#         i = findnext_first(b"\"", t.data, j + 1)
#         j = findnext_first(b"\"", t.data, i + 1)
#         val = t(:VALUE, i, j)
#         push!(out, key, val)
#     end
#     return out
# end
# subtokens(t::Token{HTMLTokens}, ::Val{:CLOSE_TAG}) = subtokens(t, Val(:OPEN_TAG))


# next(t::Token{ASCIIChars, TT}) where TT = Token(t.data, t.tokenizer, Char(t.data[t.j + 1]), t.j + 1, t.j + 1)

# tokens(tokenizer, x::AbstractString) = StringToken(tokenizer, String(x))

# function file_tokens(x::AbstractString)
#     ext = splitext(x)[2]
#     tok = get(TOKENIZERS, ext, nothing)
#     isnothing(tok) && return error("No tokenizer found for extension: $ext")
#     StringToken(tok, read(x, String))
# end
# file_tokens(x::AbstractString, tok) = StringToken(tok, read(x, String))

#-----------------------------------------------------------------------------# AbstractTokenizer
# abstract type AbstractTokenizer{K} end
# kindtype(::Type{T}) where {K, T <: AbstractTokenizer{K}} = K
# kindtype(o::AbstractTokenizer) = kindtype(typeof(o))
# init(o::AbstractTokenizer) = init(typeof(o))
# kindrepr(t, k) = k

# #-----------------------------------------------------------------------------# AbstractToken
# abstract type AbstractToken end
# position(t::AbstractToken) = (t.i, t.j)

# function Base.iterate(t::T, state=t) where {T <: AbstractToken}
#     isdone(t, state) && return nothing
#     n = next(t, state)
#     return n, n
# end
# Base.IteratorSize(::Type{T}) where {T <: AbstractToken} = Base.SizeUnknown()
# Base.eltype(::Type{T}) where {T <: AbstractToken} = T



# format(x::Int) = replace(string(x), r"(\d)(?=(\d{3})+(?!\d))" => s"\1_")




#-----------------------------------------------------------------------------# ASCIIToken
# # function Base.iterate(t::T, state=t) where {T <: Token}
# #     n = next(state)
# #     isnothing(n) ? nothing : (n, n)
# # end
# # Base.IteratorSize(::Type{T}) where {T <: Token} = Base.SizeUnknown()
# # Base.eltype(::Type{T}) where {T <: Token} = T


# struct Token{T, D <: AbstractVector{UInt8}, K} <: AbstractToken
#     tokenizer::T
#     data::D
#     kind::K
#     i::Int
#     j::Int
# end
# Token(T::Type, data) = Token(T(), data, init(T), 1, 0)
# (t::Token)(kind, i, j) = Token(t.tokenizer, t.data, kind, i, j)
# Base.String(t::Token) = String(t.data[t.i:t.j])
# Base.isdone(t::Token, state::Token) = state.j == length(t.data)

# #-----------------------------------------------------------------------------# CharTokenizer
# # This is mostly just a test of the interface
# struct CharTokenizer <: AbstractTokenizer{Char} end
# init(::Type{CharTokenizer}) = Char(0)
# kindrepr(::CharTokenizer, c) = repr(c)
# # function next(t::StringToken{CharTokenizer})
# #     i = t.j + 1
# #     @inbounds i > length(t.data) ? nothing : t(t.data[i], i, i)
# # end
# next(t::Token{CharTokenizer}, i) = Token(t.tokenizer, t.data, Char(t.data[i]), i, i), i+1

# #-----------------------------------------------------------------------------# WordTokenizer
# struct WordTokenizer <: AbstractTokenizer{Symbol} end
# init(::Type{WordTokenizer}) = :init
# function next(t::StringToken{WordTokenizer})
#     i = t.j + 1
#     i > length(t.data) && return nothing
#     c = t.data[i]
#     isletter(c) && isuppercase(c) && return t(:CAP_WORD, i, findnext(x -> !isletter(x), t.data, i) - 1)
#     isletter(c) && return t(:WORD, i, findnext(x -> !isletter(x), t.data, i) - 1)
#     ispunct(c) && return t(:PUNCTUATION, i, i)
#     isspace(c) && return t(:WHITESPACE, i, findnext(x -> !isspace(x), t.data, i) - 1)
#     return t(:UNCATEGORIZED, i, i)
# end

# push!(TOKENIZERS, ".txt" => WordTokenizer())

#-----------------------------------------------------------------------------# XMLTokenizer
# @enum XMLTokens begin
#     PI        # <?NAME $content ?>
#     DTD_OPEN  # <!DOCTYPE $root
#     DTD_DECL  # [ <!NAME $content > ]
#     ELEMENT_CLOSE  # </$name>
#     ELEMENT_OPEN # <$name
#     ATTRIBUTE_NAME # key in key="value"
#     ATTRIBUTE_VALUE  # "value" in key="value"
#     CLOSE # >
#     SELF_CLOSE # />
#     TEXT # text between tags
# # end
# @kwdef struct XMLTokenizer <: AbstractTokenizer{Symbol} end
# init(::Type{XMLTokenizer}) = :init

# function next(t::StringToken{XMLTokenizer})
#     i = t.j + 1
#     i > length(t.data) && return nothing
#     t.data[i] != '<' && return t(:TEXT, t.i, findnext('<', t.data, i) - 1)
#     s = t.data[i+1:i+2]
#     startswith(s, '?') && return t(:PROCESSING_INSTRUCTION, i, findnext('>', t.data, i + 3))
#     s == "!-" && return t(:COMMENT, i, maximum(findnext("-->", t.data, i + 4)))
#     s == "![" && return t(:CDATA, i, maximum(findnext("]]>", t.data, i + 9)))
#     startswith(s, r"!d"i) && return t(:DTD_START)
# end


# #-----------------------------------------------------------------------------# utils
# # Check if pattern `x` is present in `data` beginning at index `i`
# function is_pattern(data::AbstractVector{UInt8}, x::String, i::Int)
#     for (j, c) in enumerate(x)
#         i + j > length(data) && return false
#         @inbounds data[i + j - 1] != UInt8(c) && return false
#     end
#     return true
# end

# function findnext_unescaped(char::Char, data, i)
#     skip_next = false
#     for j in i:length(data)
#         c = Char(data[j])
#         c == char && !skip_next && return j
#         skip_next = c == '\\'
#     end
# end

# #-----------------------------------------------------------------------------# utils
# function try_collect(x)
#     out = eltype(x)[]
#     idx = 0
#     tok = first(x)
#     try
#         for (i, xi) in enumerate(x)
#             idx = i
#             tok = xi
#             push!(out, xi)
#         end
#     catch ex
#         @info "Failed to iterate at: $idx"
#         @info "Previous Token: $tok"
#         @info "Error: $ex"
#     end
#     return out
# end

# #-----------------------------------------------------------------------------# Token
# struct Token{T, K, D}
#     tokenizer::T
#     data::D
#     kind::K
#     i::Int
#     j::Int
# end
# (t::Token)(kind, i, j) = Token(t.tokenizer, t.data, kind, i, j)

# # iteration
# Base.IteratorSize(::Type{<:Token}) = Base.SizeUnknown()
# Base.IteratorEltype(::Type{<:Token}) = Base.HasEltype()
# Base.eltype(::Type{T}) where {T <: Token} = T
# function Base.iterate(t::Token, state=t)
#     n = next(state)
#     isnothing(n) && return nothing
#     return n, n
# end

# function Base.String(t::Token, limit = length(t.data))
#     (;data, i, j) = t
#     j == 0 ? "" : String(data[i:min(j, i + limit - 1)]) * (j > i + limit ? "…" : "")
# end
# function Base.show(io::IO, t::Token{T, K, D}) where {T, K, D}
#     (; i, j, kind, data) = t
#     print(io, "Token", styled" {bright_black:$(T.name.name)::$K}", styled" {bright_cyan:$i - $j}", styled" {bright_green:$(repr(kind))}")
#     print(io, styled" {bright_yellow:$(repr(String(t, 50)))}")
# end

# #-----------------------------------------------------------------------------# methods
# tokens(x::AbstractString, t) = Token(t, codeunits(x), init(t), 0, 0)
# tokens(x::AbstractVector{UInt8}, t) = Token(t, x, init(t), 0, 0)

# file_tokens(x, t) = tokens(read(x), t)
# file_tokens(x) = tokens(read(x), get(TOKENIZERS, splitext(x)[2], CharTokens()))






#-----------------------------------------------------------------------------# CharTokens
# Fallback, doesn't really do anything other than force every UInt8 to be a Char
# struct CharTokens end
# init(::CharTokens) = Char(0)
# Base.isdone(::Token{CharTokens}, t) = t.j == length(t.data)
# function next(t::Token{CharTokens})
#     i = t.i + 1
#     i > length(t.data) && return nothing
#     t(Char(t.data[i]), i, i)
# end

# struct StringTokens end
# init(::StringTokens) = :init
# function next(t::Token{StringTokens})
#     i = t.j + 1
#     i > length(t.data) && return nothing
#     j = findnext_unescaped('"', t.data, i + 1)
#     if Char(t.data[i]) == '"'
#         isnothing(j) && return t(:OTHER, i, length(t.data))
#         return t(:STRING, i, j)
#     else
#         j = isnothing(j) ? length(t.data) : j - 1
#         return t(:OTHER, i, j)
#     end
# end

# #-----------------------------------------------------------------------------# HTMLTokens
# # kinds: :tag_open, :tag_close, :comment, :doctype, :text, :unk
# struct HTMLTokens end
# TOKENIZERS[".html"] = HTMLTokens()
# init(::HTMLTokens) = :init

# function next(t::Token{HTMLTokens})
#     (; data, j) = t
#     i = findnext(x -> !isspace(Char(x)), data, j + 1)
#     isnothing(i) && return nothing
#     c = Char(data[i])
#     if c == '<'
#         j = findnext(x -> Char(x) == '>', data, i)
#         c2 = Char(data[i + 1])
#         c2 == '/' && return t(:tag_close, i, j)
#         if c2 == '!'
#             c3 = Char(data[i + 2])
#             c3 == '-' && return t(:comment, i, j)
#             return t(:doctype, i, j)
#         end
#         return t(:tag_open, i, j)
#     end
#     for j in i:length(data) - 1
#         Char(data[j]) == '<' && Char(data[j+1]) == '/' && return t(:text, i, j - 1)
#     end
# end


# #-----------------------------------------------------------------------------# JSONTokens
# struct JSONTokens end
# TOKENIZERS[".json"] = JSONTokens()
# init(::JSONTokens) = :init

# function next(t::Token{JSONTokens})
#     (; data, j) = t
#     i = findnext(x -> !isspace(Char(x)), data, j + 1)
#     isnothing(i) && return nothing
#     @inbounds c = Char(data[i])
#     c == '{' && return t(:OBJECT_OPEN, i, i)
#     c == '}' && return t(:OBJECT_CLOSE, i, i)
#     c == '[' && return t(:ARRAY_OPEN, i, i)
#     c == ']' && return t(:ARRAY_CLOSE, i, i)
#     c == '"' && return t(:STRING, i, findnext_unescaped('"', data, i + 1))
#     c == ':' && return t(:COLON, i, i)
#     c == ',' && return t(:COMMA, i, i)
#     c == 't' && return t(:TRUE, i, i + 3)
#     c == 'f' && return t(:FALSE, i, i + 4)
#     c == 'n' && return t(:NULL, i, i + 3)
#     return t(:NUMBER, i, findnext(x -> Char(x) ∉ "+-e0123456789", data, i) - 1)
# end

# #-----------------------------------------------------------------------------# XMLTokens
# struct XMLTokens end
# TOKENIZERS[".xml"] = XMLTokens()
# TOKENIZERS[".svg"] = XMLTokens()
# TOKENIZERS[".xsd"] = XMLTokens()
# init(::XMLTokens) = :init

# function next(t::Token{XMLTokens})
#     (; data, j) = t
#     i = findnext(x -> !isspace(Char(x)), data, j + 1)
#     isnothing(i) && return nothing
#     @inbounds c = Char(data[i])
#     if c == '<'
#         j = findnext(x -> Char(x) == '>', data, i)
#         c2 = Char(data[i + 1])
#         c2 == '/' && return t(:TAG_CLOSE, i, j)
#         if c2 == '!'
#             c3 = Char(data[i + 2])
#             c3 == '-' && return t(:COMMENT, i, findnext(Vector{UInt8}("-->"), data, i)[end])
#             return t(:DOCTYPE, i, j)
#         end
#         c2 == '?' && return t(:PROCESSING_INSTRUCTION, i, j)
#         c2 == '[' && return t(:CDATA, i, findnext(Vector{UInt8}("]]>"), data, i)[end])
#         return t(:TAG_OPEN, i, j)
#     else
#         return t(:TEXT, i, findnext(x -> Char(x) == '<', data, i) - 1)
#     end
# end







# function next(t::Token{HTMLTokens}, data, i, prevtoken)
# end

# function next(t::HTMLTokens, data, i, prevkind)
#     c = Char(data[i])
#     isspace(c) && return next(t, data, i + 1, prevkind)
#     c == '>' && return :open_tag_end, i, i
#     c == '/' && return :self_close, i, i + 1
#     is_pattern(data, "<!--", i) && return :comment, i, findnext(UInt8('>'), data, i) + 2
#     is_pattern(data, "<!") && return :doctype, i, findnext('>', data, i)
#     c == '<' && return :open_tag_start, i, findnext('>', data, i)


#     if prevkind in (:open_tag_start, :attr)
#         _j = findnext(x -> Char(x) == '"', data, i)
#         return :attr, i, findnext(x -> Char(x) == '"', data, _j + 1)
#     end
#     c2 = Char(data[i + 1])
#     if c != '<'
#         is_next(data, "script", i) && return :close_tag, i, findnext(Vector{UInt8}("</script>"), data, i)
#         return :text, i, findnext(x -> Char(x) == '<', data, i) - 1
#     end
#     isletter(c2) && return :open_tag_start, i, findnext(x -> isspace(Char(x)) || Char(x) == '>', data, i) - 1
#     j = findnext(x -> Char(x) == '>', data, i)
#     c2 == '/' && return :close_tag, i, j
#     c2 == '!' && return Char(data[i + 2]) == '-' ? (:comment, i, j) : (:doctype, i, j)

#     return :unk, i, i


#     # if c == '<'
#     #     c2 = Char(data[i + 1])
#     #     isletter(c2) && return :open_tag_start, findnext(x -> isspace(Char(x)) || Char(x) == '>', data, i) - 1
#     #     c2 == '/' && return :close_tag, findnext(x -> Char(x) == '>', data, i) - 1
#     #     if c2 == '!'
#     #         j = findnext(x -> Char(x) == '>', data, i)
#     #         return Char(data[i+2]) == '-' ? (:comment, j) : (:doctype, j)
#     #     end
#     # elseif prevkind in (:open_tag_start, :attribute_value)
#     #     return :attribute_name, findnext(x -> Char(x) in " =", data, i) - 1
#     # else
#     #     return :text, findnext(x -> Char(x) == '<', data, i) - 1
#     # end
# end

# function next(t::HTMLTokens, data, i, prevkind)
#     c = Char(data[i])
#     if c == '<'
#         j = i + 1
#         c = Char(data[j])
#         if isalpha(c) || c == '_'
#             return HTMLKind.TagName, j
#         elseif c == '!'
#             j += 1
#             c = Char(data[j])
#             if c == '-'
#                 return HTMLKind.Comment, j
#             elseif c == 'D' || c == 'd'
#                 return HTMLKind.Doctype, j
#             end
#         end
#     end
# end



# #-----------------------------------------------------------------------------# html_tokenizer
# html_tokenizer = (;
#     doctype = ('<', '!', OneOf('D', 'd'), Until('>')), # <!DOCTYPE
#     comment = ('<', '!', '-', '-'), # <!--
#     element = ('<', OneOf(Alpha, '_')), # <tag
# )
# TOKENIZERS[".html"] = html_tokenizer


end  # module
