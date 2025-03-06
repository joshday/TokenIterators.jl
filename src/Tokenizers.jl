module Tokenizers

using StyledStrings, StringViews

import Base: startswith, findfirst, findnext, /, tail

export Token, State,
    JSONTokens, HTMLTokens, XMLTokens,
    Before, Not, First, Last, ≺, ¬, ←, →, ⋆, ≛, ≪, 𝑠,
    next, is, isfirst, _findnext

#-----------------------------------------------------------------------------# utils
const Data = AbstractVector{UInt8}
const SData = AbstractString


#-----------------------------------------------------------------------------# Selectors
for (Sel, sym) in (:Before => :≺, :First => :←, :Last => :→, :Not => :¬, :UseStringView => :𝑠)
    @eval (struct $Sel{T} x::T end; $sym(x) = $Sel(x))
end

# A selector `x` must satisfy: if `isfirst(x, data) == true` then `first(_findnext(x, data, 1)) == 1`

@inline ≪(a, b) = isfirst(a, b)

@inline is(a) = Base.Fix1(is, a)
@inline is(f::Function) = f

@inline is(a::T, b::T) where {T} = a == b
@inline is(a::Char, b::UInt8) = UInt8(a) == b
@inline is(a::UInt8, b::Char) = Char(a) == b
@inline is(f::Function, b) = f(b)
@inline is(n::Not, b) = !is(n.x, b)

# Fallback methods
@inline isfirst(x, d) = is(x, first(d))
@inline isfirst(o::UseStringView, d) = isfirst(o.x, StringView(d))
@inline _findnext(arg, data, i) = findnext(is(arg), data, i)
@inline _findnext(o::UseStringView, data, i) = findnext(is(o.x), StringView(data), i)

@inline isfirst(x::AbstractVector, d) = all(is(x)(d) for (x,d) in zip(x,d))
@inline _findnext(x::AbstractVector, d, i) = findnext(x, d, i)

@inline isfirst(t::Tuple, d) = isempty(t) ? true : is(t[1], d[1]) && isfirst(tail(t), @view(d[2:end]))
@inline _findnext(t::Tuple, d, i) = (j = _findnext(t[1], d,i); isfirst(t, @view(d[j:end])) ? j + length(t) - 1 : _findnext(t, d, j + 1))

# Selectors that only work with _findnext
@inline _findnext(j::Int, d, i) = j
@inline _findnext(f::First, d, i) = (rng = _findnext(f.x, d, i); isnothing(rng) ? nothing : first(rng))
@inline _findnext(f::Last, d, i) =  (rng = _findnext(f.x, d, i); isnothing(rng) ? nothing : last(rng))
@inline _findnext(b::Before, d, i) = (j = _findnext(b.x, d, i); isnothing(j) ? length(d) : j - 1)


#-----------------------------------------------------------------------------# State
struct State{T} x::T end
State(; kw...) = State(NamedTuple(kw))
(o::State{T})(state) where {T} = o.x
(o::State{<:Function})(state) = o.x(state)
(o::State{<:NamedTuple})(state) = merge(state, o.x)


#-----------------------------------------------------------------------------# Token
struct Token{T <: Data, K, S} <: Data
    data::T
    kind::K
    i::Int
    j::Int
    state::S
end
Token(data, kind, state=nothing) = Token(data, kind, 1, 0, state)

⋆(t::Token{T,K,S}, k::K) where {T,K,S} = Token(t.data, k, t.i, t.j, t.state)
≛(t::Token{T,K,S}, k::K) where {T,K,S} = Token(t.data, k, t.i, t.i, t.state)
⋆(t::Token{T,K,S}, s::State) where {T,K,S} = Token(t.data, t.kind, t.i, t.j, s(t.state))
⋆(t::Token{T,K,S}, x) where {T,K,S} = Token(t.data, t.kind, t.i, t.i - 1 + _findnext(x, t, 2), t.state)
# ≀(t::Token{T,K,S}, x) where {T,K,S} = Token(t.data, t.kind, t.i, t.i - 1 + _findnext(x, StringView(t), 2), t.state)

Base.view(t::Token) = view(t.data, t.i:t.j)
StringViews.StringView(t::Token) = StringView(view(t))
Base.length(t::Token) = t.j - t.i + 1
Base.size(t::Token) = (length(t),)
Base.getindex(t::Token, i::Integer) = getindex(view(t), i)
function Base.show(io::IO, ::MIME"text/plain", t::Token)
    format(x::Int) = replace(string(x), r"(\d)(?=(\d{3})+(?!\d))" => s"\1_")
    rng = styled"{bright_black:$(format(t.i)):$(format(t.j)) len=$(format(length(t)))}"
    s = styled"$rng {bright_cyan:$(t.kind)} "
    n = displaysize(io)[2] - length(s) - 1
    _s2 = styled"{inverse:{bright_cyan:$(escape_string(StringView(t)))}}"
    s2 = _s2[1:min(n, end)] * (length(_s2) > n ? styled"{bright_cyan:…}" : "")
    print(io, s, s2)
end
Base.show(io::IO, t::Token) = show(io, MIME("text/plain"), t)

after(t::Token) = Token(t.data, t.kind, t.j + 1, length(t.data), t.state)




#-----------------------------------------------------------------------------# Tokenizer
abstract type Tokenizer{T, K, S} end
Base.IteratorSize(::Type{T}) where {T <: Tokenizer} = Base.SizeUnknown()
Base.eltype(::Type{Tok}) where {T, K, S, Tok <: Tokenizer{T, K, S}} = Token{T, K}
Base.isdone(o::Tokenizer, n::Token) = isempty(n)
Base.show(io::IO, o::Tokenizer) = print(io, typeof(o).name.name, " ($(summary(o.data)))")

init(::Type{Symbol}) = :init
init(::Type{Bool}) = false
init(::Type{Nothing}) = nothing
init(::Type{Vector{T}}) where {T} = T[]
init(::Type{Set{T}}) where {T} = Set{T}()
init(::Type{NT}) where {NT <: NamedTuple} = NT(init.(NT.types))

init(t::Tokenizer{T,K,S}) where {T,K,S} = Token(t.data, init(K), 1, length(t.data), init(S))

function Base.iterate(o::Tokenizer, n = init(o))
    Base.isdone(o, n) && return nothing
    n = next(o, n)
    return n, after(n)
end


#-----------------------------------------------------------------------------# JSONTokens
struct JSONTokens{T <: Data} <: Tokenizer{T, Symbol, Nothing}
    data::T
end
function next(o::JSONTokens, n::Token)
    '{' ≪ n && return n ≛ :curly_open
    '}' ≪ n && return n ≛ :curly_close
    '[' ≪ n && return n ≛ :square_open
    ']' ≪ n && return n ≛ :square_close
    ',' ≪ n && return n ≛ :comma
    ':' ≪ n && return n ≛ :colon
    't' ≪ n && return n ⋆ Symbol("true") ⋆ 4
    'f' ≪ n && return n ⋆ Symbol("false") ⋆ 5
    'n' ≪ n && return n ⋆ :null ⋆ 4
    '"' ≪ n && return n ⋆ :string ⋆ →((¬('\\'), '"'))
    ∈(b"-0123456789") ≪ n && return n ⋆ :number ⋆ ≺(∉(b"-+eE.0123456789"))
    ∈(b" \t\n\r") ≪ n && return n ⋆ :whitespace ⋆ ≺(∉(b" \t\n\r"))
    return n ≛ :unknown
end

# # Array = "or"
# # Tuple = specific ordering
# # Symbol = rule
# json_grammar = (
#     json = [:element],
#     value = [:object, :array, :string, :number, "true", "false", "null"],
#     object = [('{', :members, '}'), ('{', :ws, '}')],
#     members = [:member, (:member, ',', :members)],
#     member = [(:ws, :string, :ws, ':', :element)],
#     array = [('[', :elements, ']'), ('[', :ws, ']')],
#     elements = [:element, (:element, ',', :elements)],
#     element = [(:ws, value :ws)],
#     string = [('"', :characters, '"')],
#     characters = ["", (:character, :characters)],
#     character = [SetDiff(' ':'\U10ffff', "\"\\"), ('\\', :escape)],
#     escape = ['"', '\\', '/', 'b', 'f', 'n', 'r', 't', ('u', :hex, :hex, :hex, :hex)],
#     hex = [:digit, 'A':'F', 'a':'f'],
#     number = [:integer :fraction :exponent],
#     integer = [:digit, (:onenine, :digits), ('-', :digit), ('-', :onenine, :digits)],
#     digits = [:digit, (:digit, :digits)],
#     digit = ['0', :onenine],
#     onenine = ['1':'9'],
#     fraction = ["", ('.', :digits)],
#     exponent = ["", ('E', :sign, :digits), ('e', :sign, :digits)],
#     sign = ["", '+', '-'],
#     ws = ["", (' ', :ws), ('\t', :ws), ('\n', :ws), ('\r', :ws)]
# )

#-----------------------------------------------------------------------------# HTMLTokens
struct HTMLTokens{T <: Data} <: Tokenizer{T, Symbol, @NamedTuple{style::Bool, script::Bool, tag::Bool}}
    data::T
end
function next(o::HTMLTokens, n)
    𝑠(isspace) ≪ n && return n ⋆ :whitespace ⋆ ≺(𝑠(!isspace))
    # state tag=true
    b"<script" ≪ n && return n ⋆ :open_tag_start ⋆ ≺(∈(b" >")) ⋆ State(script=true, tag=true)
    b"<style" ≪ n && return n ⋆ :open_tag_start ⋆ ≺(∈(b" >")) ⋆ State(script=true, tag=true)
    if n.state.tag
        ∈(UInt8('a'):UInt8('z')) ≪ n && return n ⋆ :tag_name ⋆ ≺(∉(UInt8('a'):UInt8('z')))
        '=' ≪ n && return n ≛ :equals
        '"' ≪ n && return n ⋆ :attr_val ⋆ '"'
    end
    n.state.script && return n ⋆ :text ⋆ ≺(←(b"</script>")) ⋆ State(script=false)
    n.state.style && return n ⋆ :text ⋆ ≺(←(b"</style>")) ⋆ State(style=false)
    b"<!--" ≪ n && return n ⋆ :comment ⋆ →(b"-->")
    b"<!" ≪ n && return n ⋆ :doctype_start ⋆ 9 ⋆ State(tag=true)
    b"</" ≪ n && return n ⋆ :close_tag_start ⋆ '>'
    '>' ≪ n && return (n ≛ :open_tag_end) ⋆ State(tag=false)
    '<' ≪ n && return n ⋆ :open_tag_start ⋆ ≺(∈(b" >")) ⋆ State(tag=true)
    return n ⋆ :text ⋆ ≺('<')
end

#-----------------------------------------------------------------------------# XMLTokens
struct XMLTokens{T <: Data} <: Tokenizer{T, Symbol, Bool}
    data::T
end
function next(o::XMLTokens, n::Token)
    𝑠(isspace) ≪ n && return n ⋆ :whitespace ⋆ ≺(𝑠(!isspace))
    if n.state  # in_tag
        '=' ≪ n && return n ≛ :equals
        '"' ≪ n && return n ⋆ :attr_val ⋆ '"'
        '>' ≪ n && return (n ≛ :open_tag_end) ⋆ State(false)
        b"/>" ≪ n && return n ⋆ :self_close_tag_end ⋆ 2 ⋆ State(false)
        return n ⋆ :attr_name ⋆ ≺(∈(b" ="))
    end
    b"<?" ≪ n && return n ⋆ :pi_start ⋆ ≺(∈(b" ")) ⋆ State(true)
    b"?>" ≪ n && return n ⋆ :pi_end ⋆ 2 ⋆ State(false)
    b"<!--" ≪ n && return n ⋆ :comment ⋆ →(b"-->")
    b"<![" ≪ n && return n ⋆ :cdata ⋆ →(b"]]>")
    b"</" ≪ n && return n ⋆ :close_tag ⋆ '>'
    '<' ≪ n && return n ⋆ :open_tag_start ⋆ ≺(∈(b" >")) ⋆ State(true)
    return n ⋆ :text ⋆ ≺('<')
end

#-----------------------------------------------------------------------------# Rule
# struct Rule{I, F, T, O}
#     in_state::I
#     from::F
#     to::T
#     out_state::O
# end

end  # module
