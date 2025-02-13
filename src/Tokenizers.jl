module Tokenizers

using StyledStrings

export Token, tokens, file_tokens, next

const TOKENIZERS = Dict{String, Any}()

#-----------------------------------------------------------------------------# utils
function is_pattern(data::AbstractVector{UInt8}, x::String, i::Int)
    for (j, c) in enumerate(x)
        i + j > length(data) && return false
        @inbounds data[i + j - 1] != UInt8(c) && return false
    end
    return true
end

function findnext_unescaped(char::Char, data, i)
    skip_next = false
    for j in i:length(data)
        c = Char(data[j])
        c == char && !skip_next && return j
        skip_next = c == '\\'
    end
end

#-----------------------------------------------------------------------------# utils
function try_collect(x)
    out = eltype(x)[]
    idx = 0
    tok = first(x)
    try
        for (i, xi) in enumerate(x)
            idx = i
            tok = xi
            push!(out, xi)
        end
    catch ex
        @info "Failed to iterate at: $idx"
        @info "Previous Token: $tok"
        @info "Error: $ex"
    end
    return out
end

#-----------------------------------------------------------------------------# Token
struct Token{T, K, D <: AbstractVector{UInt8}}
    tokenizer::T
    data::D
    kind::K
    i::Int
    j::Int
end
(t::Token)(kind, i, j) = Token(t.tokenizer, t.data, kind, i, j)
Base.IteratorSize(::Type{<:Token}) = Base.SizeUnknown()
Base.IteratorEltype(::Type{<:Token}) = Base.HasEltype()
Base.eltype(::Type{T}) where {T <: Token} = T

tokens(x::AbstractString, t) = Token(t, codeunits(x), init(t), 0, 0)
tokens(x::AbstractVector{UInt8}, t) = Token(t, x, init(t), 0, 0)

file_tokens(x) = tokens(read(x), get(TOKENIZERS, splitext(x)[2], CharTokens()))

function Base.show(io::IO, t::Token{T, K, D}) where {T, K, D}
    (; i, j, kind, data) = t
    print(io, "Token", styled" {bright_black:$(T.name.name)::$K}", styled" {bright_cyan:$i - $j}", styled" {bright_green:$(repr(kind))}")
    print(io, styled" {bright_yellow:$(String(t, 50))}")
end
function Base.String(t::Token, n = length(t.data))
    (;data, i, j) = t
    j == 0 ? "" : String(data[i:min(j, i + n - 1)]) * (j > i + n ? "…" : "")
end

function Base.iterate(t::Token, state=t)
    n = next(state)
    isnothing(n) && return nothing
    return n, n
end

#-----------------------------------------------------------------------------# CharTokens
struct CharTokens end
init(::CharTokens) = Char(0)
next(::CharTokens, data, i, prevkind) = @inbounds Char(data[i]), i, j

#-----------------------------------------------------------------------------# HTMLTokens
# kinds: :tag_open, :tag_close, :comment, :doctype, :text, :unk
struct HTMLTokens end
TOKENIZERS[".html"] = HTMLTokens()
init(::HTMLTokens) = :init

function next(t::Token{HTMLTokens})
    (; data, j) = t
    i = findnext(x -> !isspace(Char(x)), data, j + 1)
    isnothing(i) && return nothing
    c = Char(data[i])
    if c == '<'
        j = findnext(x -> Char(x) == '>', data, i)
        c2 = Char(data[i + 1])
        c2 == '/' && return t(:tag_close, i, j)
        if c2 == '!'
            c3 = Char(data[i + 2])
            c3 == '-' && return t(:comment, i, j)
            return t(:doctype, i, j)
        end
        return t(:tag_open, i, j)
    end
    for j in i:length(data) - 1
        Char(data[j]) == '<' && Char(data[j+1]) == '/' && return t(:text, i, j - 1)
    end
end


#-----------------------------------------------------------------------------# JSONTokens
struct JSONTokens end
TOKENIZERS[".json"] = JSONTokens()
init(::JSONTokens) = :init

function next(t::Token{JSONTokens})
    (; data, j) = t
    i = findnext(x -> !isspace(Char(x)), data, j + 1)
    isnothing(i) && return nothing
    @inbounds c = Char(data[i])
    c == '{' && return t(:OBJECT_OPEN, i, i)
    c == '}' && return t(:OBJECT_CLOSE, i, i)
    c == '[' && return t(:ARRAY_OPEN, i, i)
    c == ']' && return t(:ARRAY_CLOSE, i, i)
    c == '"' && return t(:STRING, i, findnext_unescaped('"', data, i + 1))
    c == ':' && return t(:COLON, i, i)
    c == ',' && return t(:COMMA, i, i)
    c == 't' && return t(:TRUE, i, i + 3)
    c == 'f' && return t(:FALSE, i, i + 4)
    c == 'n' && return t(:NULL, i, i + 3)
    return t(:NUMBER, i, findnext(x -> Char(x) ∉ "+-e0123456789", data, i) - 1)
end

#-----------------------------------------------------------------------------# XMLTokens
struct XMLTokens end
TOKENIZERS[".xml"] = XMLTokens()
TOKENIZERS[".svg"] = XMLTokens()
TOKENIZERS[".xsd"] = XMLTokens()
init(::XMLTokens) = :init

function next(t::Token{XMLTokens})
    (; data, j) = t
    i = findnext(x -> !isspace(Char(x)), data, j + 1)
    isnothing(i) && return nothing
    @inbounds c = Char(data[i])
    if c == '<'
        j = findnext(x -> Char(x) == '>', data, i)
        c2 = Char(data[i + 1])
        c2 == '/' && return t(:TAG_CLOSE, i, j)
        if c2 == '!'
            c3 = Char(data[i + 2])
            c3 == '-' && return t(:COMMENT, i, findnext(Vector{UInt8}("-->"), data, i)[end])
            return t(:DOCTYPE, i, j)
        end
        c2 == '?' && return t(:PROCESSING_INSTRUCTION, i, j)
        c2 == '[' && return t(:CDATA, i, findnext(Vector{UInt8}("]]>"), data, i)[end])
        return t(:TAG_OPEN, i, j)
    else
        return t(:TEXT, i, findnext(x -> Char(x) == '<', data, i) - 1)
    end
end

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


end
