module Tokenizers

using StyledStrings, StringViews, Parsers

import Base: findfirst, findnext, !, ∈

export tokens, file_tokens, →,
    Token, TokenRule,
    # Domains
    BSplit, Partition, Rule, JSONDomain, HTMLDomain


#-----------------------------------------------------------------------------# TokenDomain
abstract type TokenDomain end
Base.show(io::IO, o::TokenDomain) =print(io, styled"{bright_green:$(name(o))}")

name(o::T) where {T} = replace(string(T),  "Tokenizers." => "")
init(x) = :init

#-----------------------------------------------------------------------------# Token
struct Token{T, D <: AbstractVector{UInt8}, K}
    domain::T
    data::D
    kind::K
    i::Int
    j::Int
end
Token(T::Type, data) = Token(T(), data)
Token(x, data) = Token(x, data, init(x), 1, 0)

Base.sizeof(t::Token) = t.j - t.i + 1
Base.view(t::Token) = view(t.data, t.i:t.j)
StringViews.StringView(t::Token) = StringView(view(t))

format(x) = x
format(x::Integer) = (s=replace(string(x), r"(\d)(?=(\d{3})+(?!\d))" => s"\1_"); styled"{bright_yellow:$s}")
format(x::Bool) = x ? styled"{bright_green:true}" : styled"{bright_black:false}"
format(x::Symbol) = styled"{bright_blue:$(repr(x))}"

function Base.show(io::IO, t::Token)
    s = styled"$(t.domain) $(format(t.i))-$(format(t.j)) " *
    styled"{magenta:($(Base.format_bytes(sizeof(t))))} $(format(t.kind)) "
    n = displaysize(io)[2] - length(s)
    _s2 = escape_string(StringView(t))
    s2 = length(_s2) > n ? styled"{inverse:$(_s2[1:n-1])}…" : styled"{inverse:$_s2}"
    print(io, s, s2)
end

Base.IteratorSize(::Type{T}) where {T <: Token} = Base.SizeUnknown()
Base.eltype(::Type{T}) where {T <: Token} = T
Base.isdone(::Token, t::Token) = t.j == length(t.data)

function Base.iterate(t::Token, state=t)
    Base.isdone(t, state) && return nothing
    n = Token(Next(state))
    return n, n
end



#-----------------------------------------------------------------------------# Next
struct Next{T, D}
    domain::T
    data::D
    i::Int
end
Next(t::Token) = Next(t.domain, t.data, t.j + 1)
Base.view(n::Next) = @view n.data[n.i:end]
StringViews.StringView(n::Next) = StringView(view(n))

rules(::Type{Next{T,D}}) where {T,D} = rules(T)

@generated function Token(n::Next)
    exprs = map(collect(pairs(rules(n)))) do (k, rule)
        :(isfirst($rule, n) && return Token(o, n.data, $(QuoteNode(k)), n.i, n.i + findfirst($rule, n) - 1))
    end
    Expr(:block, exprs...)
end
function Token(n::Next{T,D}) where {T <: Rule, D}
    rule = n.domain
    isfirst(rule, n) && return Token(rule, n.data, true, n.i, n.i + findfirst(rule, n) - 1)
    return Token(rule, n.data, false, n.i, n.i + findfirst(Before(rule), n) - 1)
end

# @generated function Token(n::Next)
#     r = rules(n)
#     exprs = map(collect(pairs(rules(n)))) do (k, r)
#         quote
#             isfirst($r, n) && return Token(n.domain, n.data, $(repr(k)))
#         end
#     end
# end

# # TODO START HERE
# # Want generated function body to be unrolled version of this loop:
# # for state in states(domain)
# #     for rule in rules(domain)
# #         isfirst(rule, n) && return Token(domain, n.data, rule)
# #     end
# # end



#-----------------------------------------------------------------------------# Rule
# A rule is 1) How to Identify if the data is a given token, and 2) How to find the last index
struct Rule{T, S}
    id::T
    idx::S
end
Rule(id::Char) = Rule(UInt8(id), nothing)
Rule(id::String) = Rule(id, ncodeunits(id))
Rule(id::AbstractVector{UInt8}) = Rule(id, length(id))
Rule(p::Pair) = Rule(p...)
Base.show(io::IO, r::Rule) = print(io, styled"{blue:Rule($(repr(r.id)), $(repr(r.idx)))}")
Base.print(io::IO, r::Rule) = show(io, r)

init(x::Rule) = false
rules(::Type{R}) where {R<:Rule} = (; var"true" = r, var"false" = !r)
!(r::Rule) = Rule(Not(r.id), Before(r.id))


~(a, b) = isfirst(a, b)
→(a, b) = findfirst(b, a)

isfirst(r::Rule, n::Next) = isfirst(r.id, n)
findfirst(r::Rule, n::Next) = findfirst(r.idx, n)

isfirst(r::Rule) = Base.Fix1(isfirst, r)
findfirst(r::Rule) = Base.Fix1(findfirst, r)

# idx-only types
findfirst(::Nothing, n::Next) = 1
findfirst(j::Int, n::Next) = j

# UInt8
isfirst(x::UInt8, n::Next) = first(view(n)) == x
findfirst(x::UInt8, n::Next) = findfirst(==(x), view(n))

# Char
isfirst(x::Char, n::Next) = first(StringView(n)) == x
findfirst(x::Char, n::Next) = findfirst(x, StringView(n))
# show_as(x::Char) = repr(x)

# AbstractVector{UInt8}
isfirst(x::AbstractVector{UInt8}, n::Next) = all(x -> x[1] == x[2], zip(view(n), x))
findfirst(x::AbstractVector{UInt8}, n::Next) = findfirst(x, view(n))
# show_as(x::AbstractVector{UInt8}) = "b\"$(StringView(x))\""

# AbstractString
isfirst(x::AbstractString, n::Next) = startswith(StringView(n), x)
findfirst(x::AbstractString, n::Next) = findfirst(x, StringView(n))
# show_as(x::AbstractString) = repr(x)

# Set
isfirst(x::Set, n::Next) = any(x -> isfirst(x, n), x)
findfirst(x::Set{Char}, n::Next) = findfirst(in(x), StringView(n))
findfirst(x::Set{UInt8}, n::Next) = findfirst(in(x), view(n))
# show_as(x::Set) = "⟨" * join([show_as(x) for x in x], ", ") * "⟩"

# Function
isfirst(f::Function, n::Next) = f(first(view(n)))
findfirst(f::Function, n::Next) = findfirst(f, view(n))
# show_as(f::Function) = "Function($f)"

# CharFun
struct CharFun{F} f::F end
Base.show(io::IO, o::CharFun) = print(io, "CharFun($(o.f))")
isfirst(cf::CharFun, n::Next) = cf.f(first(StringView(n)))
findfirst(cf::CharFun, n::Next) = findfirst(cf.f, StringView(n))

struct Unescaped
    char::Char
    escape::Char
    Unescaped(x::Char, esc::Char='\\') = new(x, esc)
end
Base.show(io::IO, o::Unescaped) = print(io, "Unescaped($(repr(o.char)))")
function findfirst(o::Unescaped, n::Next)
    v = view(n)
    skip = false
    for j in 2:length(v)
        x = v[j]
        x == UInt8(o.char) && !skip && return j
        skip = x == UInt8(o.escape)
    end
end

struct First{T} x::T end
Base.show(io::IO, f::First) = print(io, "First($(repr(f.x)))")
findfirst(f::First, n::Next) = first(findfirst(f.x, n))

struct Last{T} x::T end
Base.show(io::IO, l::Last) = print(io, "Last($(repr(l.x)))")
findfirst(l::Last, n::Next) = last(findfirst(l.x, n))

struct Before{T} x::T end
Base.show(io::IO, b::Before) = print(io, "Before($(repr(b.x)))")
findfirst(b::Before, n::Next) = (j = findfirst(b.x, n); isnothing(j) ? length(view(n)) : j - 1)

struct Not{T} x::T end
Base.show(io::IO, n::Not) = print(io, "Not($(repr(n.x)))")
isfirst(not::Not, n) = !isfirst(not.x, x)

# macro tok(k, r...)
#     esc(:(r = Rule($(r...)); isfirst(r, n) && return $k => findfirst(r, n)))
# end

STRING = Rule('"', Unescaped('"'))
WHITESPACE = Rule(CharFun(isspace), Before(CharFun(!isspace)))



# # Using a Rule as the domain will split everything into `tok` or `!tok`
# Token(r::Rule, data) = Token(r, data, false, 1, 0)


#-----------------------------------------------------------------------------# JSONDomain
struct JSONDomain <: TokenDomain end

rules(::Type{JSONDomain}) = (
    bracket_open = '{',
    bracket_close = '}',
    square_open = '[',
    square_close = ']',
    comma = ',',
    colon = ':',
    string = '"' => Unescaped('"'),
    number = Set("-0123456789") => Set("-+eE.0123456789"),
    var"true" = 't' => 3,
    var"false" = 'f' => 4,
    null = 'n' => 3,
    ws = CharFun(isspace) => CharFun(!isspace)
)



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
