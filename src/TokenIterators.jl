module TokenIterators

using StringViews, StyledStrings
import Base: startswith, findnext

#------------------------------------------------------------------------------# Token
struct Token{T<:AbstractVector{UInt8},K} <: AbstractVector{UInt8}
    data::T
    kind::K
    i::Int
    j::Int
end
Token(data::AbstractVector{UInt8}, kind=nothing) = Token(data, kind, 1, 0)
Token(s::AbstractString, args...) = Token(codeunits(s), args...)

Base.view(t::Token) = view(t.data, t.i:t.j)
StringViews.StringView(t::Token) = StringView(view(t))
Base.length(t::Token) = t.j - t.i + 1
Base.size(t::Token) = (length(t),)
Base.getindex(t::Token, i::Integer) = view(t)[i]

function Base.show(io::IO, ::MIME"text/plain", t::Token)
    s = styled"$(format(t.i))→$(format(t.j)) {bright_black:($(Base.format_bytes(length(t))))} " *
        styled"{bright_cyan:$(t.kind)} "
    n = displaysize(io)[2] - length(s) - 1
    _s2 = styled"{inverse:{bright_cyan:$(escape_string(StringView(t)))}}"
    s2 = _s2[1:min(n, end)] * (length(_s2) > n ? styled"{bright_cyan:…}" : "")
    print(io, s, s2)
end
Base.show(io::IO, t::Token) = show(io, MIME("text/plain"), t)

after(t::Token) = Token(t.data, t.kind, t.j + 1, length(t.data))

# "Anchor: First/Last/Before/After anchors a UnitRange returned from findnext into an Int"
@enum Anchor NoAnchor First Last Before After
function pick(t::Token, rng::UnitRange{Int}, anchor::Anchor)
    anchor == First ? rng[1] :
    anchor == Last ? rng[end] :
    anchor == Before ? rng[1] - 1 :
    anchor == After ? rng[end] + 1 :
    error("NoAnchor")
end
pick(t::Token, ::Nothing, anchor::Anchor) = anchor == Before ? length(t) : error("$anchor can't be applied to `nothing`")
pick(::Token, j::Int, anchor::Anchor) = anchor == Before ? j - 1 : anchor == After ? j + 1 : j

function emit(t::Token, kind, start_pattern_width::Int, stop_pattern, anchor::Anchor=Last)
    len = findnext(stop_pattern, t, start_pattern_width + 1, anchor)
    Token(t.data, kind, t.i, t.i + len - 1)
end


#-----------------------------------------------------------------------------# utils
format(x::Int) = replace(string(x), r"(\d)(?=(\d{3})+(?!\d))" => s"\1_")

function token_error(data::AbstractVector{UInt8}, i::Int, nchars=50)
    sv = StringView(data)
    pre = escape_string(sv[max(1, i - nchars):i-1])
    c = escape_string(string(sv[i]))
    post = escape_string(sv[i+1:min(i + nchars, end)])
    error(styled"""
    {bright_red:No Token Identified in $(summary(data)) beginning at position $i}

    {bright_black:$pre}{inverse:{bright_red:$c}}{bright_black:$post}
    """)
end

#------------------------------------------------------------------------------# interface
# startswith(::Token, ::Pattern) --> Bool
# findnext(::Pattern, ::Token, ::Int) -> Union{Int, UnitRange{Int}, Nothing}
#
# Pipeline:
# 1) startswith(token, start_pattern) --> true
# 2) findnext(stop_pattern, token, width(start_pattern), anchor?)
# 3) If findnext returns range, adjust with `Anchor`


#------------------------------------------------------------------------------# startswith patterns
width(::Token, x) = width(x)

startswith(t::Token, x::UInt8) = t[1] == x
width(::UInt8) = 1

startswith(t::Token, x::Char) = isascii(x) ? t[1] == UInt8(x) : StringView(t)[1] == x
width(x::Char) = ncodeunits(x)

function startswith(t::Token, x::AbstractVector{UInt8})
    length(x) > length(t) && return false
    all(a == b for (a, b) in zip(t, x))
end
width(x::AbstractVector{UInt8}) = length(x)

startswith(t::Token, x::AbstractString) = startswith(t, codeunits(x))
width(x::AbstractString) = ncodeunits(x)

startswith(t::Token, f::Function) = f(t[1])
width(::Function) = 1

drop(t::Token, n::Integer) = Token(t.data, t.kind, t.i + n, t.j)
startswith(t::Token, tup::Tuple) = isempty(tup) ? true : startswith(t, tup[1]) && startswith(drop(t, width(tup[1])), Base.tail(tup))
width(t::Token, tup::Tuple) = sum(width(t, x) for x in tup)

#------------------------------------------------------------------------------# findnext patterns
findnext(pattern, t::Token, i::Integer, anchor::Anchor) = pick(t, findnext(pattern, t, i), anchor)

findnext(f::Function, t::Token, i::Integer) = findnext(f, view(t), i)
findnext(x::Integer, t::Token, i::Integer) = x
findnext(x::UInt8, t::Token, i::Integer) = findnext(==(x), view(t), i)
findnext(x::Char, t::Token, i::Integer) = isascii(x) ? findnext(==(UInt8(x)), view(t), i) : findnext(==(x), StringView(t), i)
findnext(x::AbstractString, t::Token, i::Integer, anchor::Anchor) = pick(t, findnext(x, StringView(t), i), anchor)

#------------------------------------------------------------------------------# Char functions
for f in [isletter, isdigit, isnumeric, isspace, isuppercase, islowercase, isxdigit, ispunct, isprint, isascii, iscntrl]
    @eval begin
        startswith(t::Token, ::typeof($f)) = $f(StringView(t)[1])
        findnext(::typeof($f), t::Token, i::Int) = findnext($f, StringView(t), i)
    end
end

#-----------------------------------------------------------------------------# Unescaped
struct Unescaped{T<:Union{UInt8,Char}}
    char::T
    esc::T
    Unescaped(c::T, esc::T=T('\\')) where {T<:Union{UInt8,Char}} = new{T}(c, esc)
end

function findnext(x::Unescaped{UInt8}, t::Token, i::Integer)
    escaped = false
    @inbounds for j in i:length(t)
        byte = t.data[t.i+j-1]
        byte == x.char && !escaped && return j
        escaped = byte == x.esc ? !escaped : false
    end
    return nothing
end

function findnext(x::Unescaped{Char}, t::Token, i::Integer)
    escaped = false
    sv = StringView(t)
    j = i
    while j <= ncodeunits(sv)
        c = sv[j]
        c == x.char && !escaped && return j
        escaped = c == x.esc ? !escaped : false
        j = nextind(sv, j)
    end
    return nothing
end


#-----------------------------------------------------------------------------# AbstractTokenizer
abstract type AbstractTokenizer{T,K,S} end

Base.show(io::IO, o::AbstractTokenizer) = print(io, typeof(o).name.name, " ($(Base.format_bytes(length(o.data))))")

Base.IteratorSize(::Type{<:AbstractTokenizer}) = Base.SizeUnknown()
Base.eltype(::Type{Tok}) where {T,K,S,Tok<:AbstractTokenizer{T,K,S}} = Token{T,K}
Base.isdone(o::AbstractTokenizer, (prev, state)) = prev.j == length(prev.data)

init_kind(K) = error("No init_kind method for kind type $K")
init_state(::Type{K}) where {K} = nothing

# Returns (sentinel_token, initial_state)
init(o::AbstractTokenizer{T,K,S}) where {T,K,S} = (Token(o.data, init_kind(K), 1, 0), init_state(K))

next(o::AbstractTokenizer{T,K,S}, tok::Token{T,K}, state::S) where {T,K,S} = error("$o has not implemented the required next(...) method.")

function Base.iterate(o::AbstractTokenizer, (prev, state)=init(o))
    Base.isdone(o, (prev, state)) && return nothing
    tok, new_state = next(o, after(prev), state)
    return tok, (tok, new_state)
end

function debug(o::AbstractTokenizer)
    tok = nothing
    try
        for (i, ti) in enumerate(o)
            tok = ti
            i > length(o.data) && error("Found more tokens than bytes available")
        end
    catch
        @warn "Probably an infinite loop.  Stopping after $(length(o.data)) tokens."
        token_error(tok.data, tok.j + 1)
    end
    @info "success!"
end

#------------------------------------------------------------------------------# Rule
struct Rule{Start,Stop}
    startswith_pattern::Start
    findnext_pattern::Stop
    anchor::Anchor
end
Rule(start, stop) = Rule(start, stop, NoAnchor)

startswith(t::Token, r::Rule) = startswith(t, r.startswith_pattern)
findnext(r::Rule, t::Token) = findnext(r.findnext_pattern, t, width(r.startswith_pattern))

const STRING_DQ = Rule('"', Unescaped('"'))
const STRING_SQ = Rule(''', Unescaped('''))
const STRING_BACKTICK = Rule('`', Unescaped('`'))
const STRING_TRIPLE_DQ = Rule("\"\"\"", "\"\"\"", Last)
const STRING_TRIPLE_BACKTICK = Rule("```", "```", Last)

const NUMBER = Rule(∈(b"-0123456789"), !∈(b"-+eE.0123456789"), Before)
const INTEGER = Rule(isdigit, !isdigit, Before)
const HEX = Rule("0x", !isxdigit, Before)

const WHITESPACE = Rule(isspace, !isspace, Before)
const NEWLINE = Rule('\n', 1)  # exactly one newline byte
const LINE = Rule(Returns(true), "\n", Before)  # up to (not including) \n

const LINE_COMMENT_HASH = Rule('#', "\n", Before)
const LINE_COMMENT_SLASH = Rule("//", "\n", Before)
const BLOCK_COMMENT_C = Rule("/*", "*/", Last)
const BLOCK_COMMENT_JULIA = Rule("#=", "=#", Last)

#------------------------------------------------------------------------------# @tokenizer
# DSL: kind = [state_guard -->] start_pat [.. stop_pat [>> anchor]] [=> new_state]
# Operator precedence (high to low): >>  >  ..  >  -->  >  =>
function _parse_rule_expr(ex)
    guard = nothing
    state_change = nothing
    if Meta.isexpr(ex, :call) && ex.args[1] === :(=>)
        state_change = ex.args[3]
        ex = ex.args[2]
    end
    if Meta.isexpr(ex, :-->)
        guard = ex.args[1]
        ex = ex.args[2]
    end
    if Meta.isexpr(ex, :call) && ex.args[1] === :(..)
        start_pat = ex.args[2]
        rhs = ex.args[3]
        if Meta.isexpr(rhs, :call) && rhs.args[1] === :(>>)
            return (; guard, start=start_pat, stop=rhs.args[2], anchor=rhs.args[3], state_change)
        end
        return (; guard, start=start_pat, stop=rhs, anchor=nothing, state_change)
    end
    return (; guard, start=ex, stop=nothing, anchor=nothing, state_change)
end

macro tokenizer(name, body)
    pairs = Pair{Symbol,Any}[]
    for ex in body.args
        ex isa LineNumberNode && continue
        Meta.isexpr(ex, :(=)) || error("@tokenizer: expected `kind = expr`, got: $ex")
        push!(pairs, ex.args[1] => ex.args[2])
    end
    isempty(pairs) && error("@tokenizer body must not be empty")

    kind_syms = [p.first for p in pairs]
    parsed = [k => _parse_rule_expr(ex) for (k, ex) in pairs]

    is_stateful = any(p -> !isnothing(p.second.guard) || !isnothing(p.second.state_change), parsed)
    S = is_stateful ? :Symbol : :Nothing

    stmts = Expr[]
    for (kind, r) in parsed
        new_state = isnothing(r.state_change) ? :state : r.state_change
        emit_ex = if isnothing(r.stop)
            :(return (TokenIterators.emit(t, $kind, TokenIterators.width($(r.start)), TokenIterators.width($(r.start))), $new_state))
        elseif isnothing(r.anchor)
            :(return (TokenIterators.emit(t, $kind, TokenIterators.width($(r.start)), $(r.stop)), $new_state))
        else
            :(return (TokenIterators.emit(t, $kind, TokenIterators.width($(r.start)), $(r.stop), $(r.anchor)), $new_state))
        end
        match_ex = :(Base.startswith(t, $(r.start)))
        push!(stmts, isnothing(r.guard) ? :($match_ex && $emit_ex) : :(state == $(r.guard) && $match_ex && $emit_ex))
    end

    init_state_ex = is_stateful ? :(TokenIterators.init_state(::Type{Kinds}) = :default) : :()

    rule_exprs = Expr[]
    for (kind, r) in parsed
        rule_val = if isnothing(r.stop)
            r.start
        elseif isnothing(r.anchor)
            :(TokenIterators.Rule($(r.start), $(r.stop)))
        else
            :(TokenIterators.Rule($(r.start), $(r.stop), $(r.anchor)))
        end
        push!(rule_exprs, :($kind => $rule_val))
    end

    Expr(:toplevel, esc(:(module $name
    import TokenIterators
    using TokenIterators: Rule, Unescaped, Before, After, First, Last, NoAnchor

    @enum Kinds $(kind_syms...)

    struct Tokenizer{T} <: TokenIterators.AbstractTokenizer{T,Kinds,$S}
        data::T
    end

    TokenIterators.init_kind(::Type{Kinds}) = $(kind_syms[1])
    $init_state_ex

    function TokenIterators.next(o::Tokenizer, t::TokenIterators.Token, state::$S)
        $(stmts...)
        TokenIterators.token_error(t.data, t.i)
    end

    const RULES = Dict{Kinds,Any}($(rule_exprs...))
    end)))
end

#------------------------------------------------------------------------------# JSONTokens
# States: :default (top-level / array), :expect_key, :expect_colon,
#         :expect_value (after :), :in_object (after a value in object context)
@tokenizer JSONTokens begin
    curly_open   = '{'                                                    => :expect_key
    curly_close  = '}'                                                    => :default
    square_open  = '['                                                    => :default
    square_close = ']'                                                    => :default
    colon        = :expect_colon --> ':'                                  => :expect_value
    comma        = :in_object    --> ','                                  => :expect_key
    True         = "true"                                                 => :in_object
    False        = "false"                                                => :in_object
    null         = "null"                                                 => :in_object
    whitespace   = ∈(b"\t\n\r ") .. !∈(b"\t\n\r ") >> Before
    key          = :expect_key   --> '"' .. Unescaped('"')                => :expect_colon
    string       = :expect_value --> '"' .. Unescaped('"')                => :in_object
    number       = ∈(b"-0123456789") .. !∈(b"-+eE.0123456789") >> Before => :in_object
    unknown      = Returns(true)
end


#------------------------------------------------------------------------------# XMLTokens
# States: :default (text/between-tags), :in_open_tag (inside <tagname ...>)
@tokenizer XMLTokens begin
    comment    = "<!--"     .. "-->"
    cdata      = "<![CDATA[" .. "]]>"
    doctype    = "<!"        .. '>'
    pi         = "<?"        .. "?>"
    close_tag  = "</"        .. '>'
    open_tag   = '<'         .. ∈(b" \t\n\r>/") >> Before => :in_open_tag
    tag_end    = :in_open_tag --> '>'                      => :default
    self_close = :in_open_tag --> "/>"                     => :default
    attr_val   = :in_open_tag --> '"' .. Unescaped('"')
    equals     = :in_open_tag --> '='
    attr_key   = :in_open_tag --> isletter .. ∈(b"= \t\n\r>/") >> Before
    whitespace = :in_open_tag --> ∈(b" \t\n\r") .. !∈(b" \t\n\r") >> Before
    text       = :default     --> Returns(true) .. '<' >> Before
    unknown    = Returns(true)
end


#------------------------------------------------------------------------------# HTMLTokens
# States: :default       (text / between tags)
#         :in_open_tag   (inside <tagname ...>)
#         :in_script_tag (inside <script ...>, attr parsing)
#         :in_style_tag  (inside <style ...>, attr parsing)
#         :in_script     (raw JavaScript content)
#         :in_style      (raw CSS content)
@tokenizer HTMLTokens begin
    # Raw content rules MUST come first so they fire before unguarded rules
    script_close_tag   = :in_script     --> "</script" .. '>'                  => :default
    style_close_tag    = :in_style      --> "</style"  .. '>'                  => :default
    script             = :in_script     --> Returns(true) .. "</script" >> Before
    style              = :in_style      --> Returns(true) .. "</style"  >> Before

    comment            = "<!--"         .. "-->"
    doctype            = "<!"           .. '>'
    script_open        = "<script"      .. ∈(b" \t\n\r>/") >> Before          => :in_script_tag
    style_open         = "<style"       .. ∈(b" \t\n\r>/") >> Before          => :in_style_tag
    close_tag          = :default       --> "</"  .. '>'
    open_tag           = '<'            .. ∈(b" \t\n\r>/") >> Before          => :in_open_tag

    # Regular open tag attribute parsing
    tag_end            = :in_open_tag   --> '>'                                => :default
    self_close         = :in_open_tag   --> "/>"                               => :default
    attr_val           = :in_open_tag   --> '"'  .. Unescaped('"')
    attr_val_sq        = :in_open_tag   --> '\'' .. Unescaped('\'')
    equals             = :in_open_tag   --> '='
    attr_key           = :in_open_tag   --> isletter .. ∈(b"= \t\n\r>/") >> Before
    whitespace         = :in_open_tag   --> ∈(b" \t\n\r") .. !∈(b" \t\n\r") >> Before

    # <script ...> attribute parsing (identical rules, tag_end transitions to :in_script)
    script_tag_end     = :in_script_tag --> '>'                                => :in_script
    script_self_close  = :in_script_tag --> "/>"                               => :default
    script_attr_val    = :in_script_tag --> '"'  .. Unescaped('"')
    script_attr_val_sq = :in_script_tag --> '\'' .. Unescaped('\'')
    script_equals      = :in_script_tag --> '='
    script_attr_key    = :in_script_tag --> isletter .. ∈(b"= \t\n\r>/") >> Before
    script_whitespace  = :in_script_tag --> ∈(b" \t\n\r") .. !∈(b" \t\n\r") >> Before

    # <style ...> attribute parsing (identical rules, tag_end transitions to :in_style)
    style_tag_end      = :in_style_tag  --> '>'                                => :in_style
    style_self_close   = :in_style_tag  --> "/>"                               => :default
    style_attr_val     = :in_style_tag  --> '"'  .. Unescaped('"')
    style_attr_val_sq  = :in_style_tag  --> '\'' .. Unescaped('\'')
    style_equals       = :in_style_tag  --> '='
    style_attr_key     = :in_style_tag  --> isletter .. ∈(b"= \t\n\r>/") >> Before
    style_whitespace   = :in_style_tag  --> ∈(b" \t\n\r") .. !∈(b" \t\n\r") >> Before

    text               = :default       --> Returns(true) .. '<' >> Before
    unknown            = Returns(true)
end

end  # module
