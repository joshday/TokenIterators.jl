module Tokenizers

using StyledStrings, StringViews

import Base: startswith, findfirst, findnext, ∈

export Token, Next, JSONTokens, HTMLTokens, XMLTokens, Rule,
    next, next_data, rules, findj, isfirst, isbyte, nbytes, →, ¬, ∿, ≺

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
findj(x::Data, data::Data, i) = last(findnext(x, data, i))

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

#-----------------------------------------------------------------------------# XMLTokens
struct XMLTokens end
function next(n::Token{XMLTokens})
    @tryrule n :pi b"<?" → b"?>"
    @tryrule n :cdata b"<![" → b"]]>"
    @tryrule n :close_tag b"</" → '>'
    @tryrule n :comment b"<!--" → b"-->"
    @tryrule n :open_tag '<' → '>'
    @tryrule n :whitespace ∿(isspace) → ≺(∿(!isspace))
    @tryrule n :text Unknown() → ≺('<')
    @dorule n :unknown Unknown()
end


end  # module
