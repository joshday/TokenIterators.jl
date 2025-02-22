module Tokenizers

using StyledStrings, StringViews, Parsers

import Base: findfirst, findnext, !

export tokens, file_tokens, →,
    Token, TokenRule,
    # Domains
    BSplit, Partition, Rule, JSONDomain, HTMLDomain

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

# #-----------------------------------------------------------------------------# Token
# struct Token{T, D <: AbstractVector{UInt8}, K}
#     domain::T
#     data::D
#     kind::K
#     i::Int
#     j::Int
# end
# Token(D::Type, data) = Token(D(), data)
# Token(domain::TokenDomain, data) = Token(domain, data, init(domain), 1, 0)
# (t::Token)(kind, i::Int, j::Int) = Token(t.domain, t.data, kind, i, j)
# Base.view(t::Token) = view(t.data, t.i:t.j)
# Base.String(t::Token) = String(view(t))
# Base.length(t::Token) = t.j - t.i + 1
# function Base.show(io::IO, t::Token)
#     k = styled"{bright_green:$(t.domain)} {bright_yellow:$(format(t.i)):$(format(t.j))} {bright_cyan:$(repr(t.kind))} "
#     n = displaysize(io)[2] - length(k)
#     s = escape_string(String(t))
#     limit = length(s) > n
#     print(io, k, styled"{inverse:$(s[1:min(n, end)])}", limit ? "…" : "")
# end

# #-----------------------------------------------------------------------------# Next
# struct Next{D, V, SV, K}
#     data::D
#     view::V
#     sview::SV
#     i::Int
#     prevkind::K
# end
# function Next(t::Token)
#     i = t.j + 1
#     v = @view(t.data[i:end])
#     return Next(t.data, v, StringView(v), i, t.kind)
# end
# Base.show(io::IO, n::Next) = print(io, "Next: $(n.i)")

# #-----------------------------------------------------------------------------# Token iteration
# Base.IteratorSize(::Type{T}) where {T <: Token} = Base.SizeUnknown()
# Base.eltype(::Type{T}) where {T <: Token} = T

# Base.isdone(t::Token, n::Next) = n.i > length(t.data)

# function Base.iterate(t::Token, n=Next(t))
#     Base.isdone(t, n) && return nothing
#     t2 = next(t.domain, n)
#     return t2, Next(t2)
# end

# # @noinline function next(domain, n::Next)
# #     kind, j = _next(domain, n)
# #     Token(domain, n.data, kind, n.i, n.i + j - 1)
# # end

# #-----------------------------------------------------------------------------# Rule
# # A rule is 1) How to Identify if the data is a given token, and 2) How to find the last index
# struct Rule{T, S}
#     id::T
#     idx::S
# end
# Rule(id::Char) = Rule(UInt8(id), nothing)
# Rule(id::String) = Rule(id, ncodeunits(id))
# Rule(id::AbstractVector{UInt8}) = Rule(id, length(id))
# Base.show(io::IO, r::Rule) = print(io, show_as(r.id), " => ", show_as(r.idx))
# !(r::Rule) =

# show_as(x) = x

# ~(a, b) = isfirst(a, b)
# →(a, b) = findfirst(b, a)

# isfirst(r::Rule, n::Next) = isfirst(r.id, n)
# findfirst(r::Rule, n::Next) = findfirst(r.idx, n)

# isfirst(r::Rule) = Base.Fix1(isfirst, r)
# findfirst(r::Rule) = Base.Fix1(findfirst, r)

# # idx-only types
# findfirst(::Nothing, n::Next) = 1
# findfirst(j::Int, n::Next) = j

# # UInt8
# isfirst(x::UInt8, n::Next) = first(n.view) == x
# findfirst(x::UInt8, n::Next) = findfirst(==(x), n.view)

# # Char
# isfirst(x::Char, n::Next) = first(n.view) == UInt8(x)
# findfirst(x::Char, n::Next) = findfirst(==(UInt8(x)), n.view)
# show_as(x::Char) = repr(x)

# # AbstractVector{UInt8}
# isfirst(x::AbstractVector{UInt8}, n::Next) = all(x -> x[1] == x[2], zip(n.view, x))
# findfirst(x::AbstractVector{UInt8}, n::Next) = findfirst(x, n.view)
# show_as(x::AbstractVector{UInt8}) = "b\"$(StringView(x))\""

# # AbstractString
# isfirst(x::AbstractString, n::Next) = startswith(n.sview, x)
# findfirst(x::AbstractString, n::Next) = findfirst(x, n.sview)
# show_as(x::AbstractString) = repr(x)

# # Set
# isfirst(x::Set, n::Next) = any(x -> isfirst(x, n), x)
# findfirst(x::Set{Char}, n::Next) = findfirst(in(x), n.sview)
# findfirst(x::Set{UInt8}, n::Next) = findfirst(in(x), n.view)
# show_as(x::Set) = "⟨" * join([show_as(x) for x in x], ", ") * "⟩"

# # Function
# isfirst(f::Function, n::Next) = f(first(n.view))
# findfirst(f::Function, n::Next) = findfirst(f, n.view)
# show_as(f::Function) = "Function($f)"

# # CharFun
# isfirst(cf::CharFun, n::Next) = cf.f(first(n.sview))
# findfirst(cf::CharFun, n::Next) = findfirst(cf.f, n.sview)

# struct Unescaped
#     char::Char
#     escape::Char
#     Unescaped(x::Char, esc::Char='\\') = new(x, esc)
# end
# function findfirst(o::Unescaped, s::Next)
#     (; view) = s
#     skip = false
#     for j in 2:length(view)
#         x = view[j]
#         x == UInt8(o.char) && !skip && return j
#         skip = x == UInt8(o.escape)
#     end
# end

# struct First{T} x::T end
# Base.show(io::IO, f::First) = print(io, "First($(show_as(f.x)))")
# findfirst(f::First, n::Next) = first(findfirst(f.x, n))

# struct Last{T} x::T end
# Base.show(io::IO, l::Last) = print(io, "Last($(show_as(l.x)))")
# findfirst(l::Last, n::Next) = last(findfirst(l.x, n))

# struct Before{T} x::T end
# Base.show(io::IO, b::Before) = print(io, "Before($(show_as(b.x)))")
# findfirst(b::Before, n::Next) = (j = findfirst(b.x, n); isnothing(j) ? length(n.view) : j - 1)


# macro tok(k, r...)
#     esc(:(r = Rule($(r...)); isfirst(r, n) && return $k => findfirst(r, n)))
# end

# STRING = Rule('"', Unescaped('"'))
# WHITESPACE = Rule(CharFun(isspace), Before(CharFun(!isspace)))

# @generated function next(o::TokenDomain, n::Next)
#     exprs = map(collect(pairs(rules(o)))) do (k, rule)
#         :(isfirst($rule, n) && return Token(o, n.data, $(QuoteNode(k)), n.i, findfirst($rule, n)))
#     end
#     Expr(:block, exprs...)
# end

# # Using a Rule as the domain will split everything into `tok` or `!tok`
# Token(r::Rule, data) = Token(r, data, false, 1, 0)
# rules(r::Rule) = (var"true" => r, var"false" => !r)

# #-----------------------------------------------------------------------------# JSONDomain
# struct JSONDomain <: TokenDomain end

# rules(::Type{JSONDomain}) = (
#     bracket_open = Rule('{'),
#     bracket_close = Rule('}'),
#     square_open = Rule('['),
#     square_close = Rule(']'),
#     comma = Rule(','),
#     colon = Rule(':'),
#     string = STRING,
#     number = Rule(Set("-0123456789"), Set("-+eE.0123456789")),
#     var"true" = Rule("true"),
#     var"false" = Rule("false"),
#     null = Rule("null"),
#     ws = WHITESPACE
# )

# #-----------------------------------------------------------------------------# HTMLDomain
# struct HTMLDomain <: TokenDomain end

# rules(::Type{HTMLDomain}) = (
#     ws = WHITESPACE,
#     tag_end = Rule("</", '>'),
#     comment = Rule("<!-", Last("-->")),
#     doctype = Rule("<!", '>'),
#     script = Rule("<script", Last("</script>")),
#     style = Rule("<style", Last("</style>")),
#     tag = Rule('<', '>'),
#     text = Rule(x -> true, Before('<'))
# )

end  # module
