#-----------------------------------------------------------------------------# JSONTokens
struct JSONTokens{T} <: AbstractTokenizer{T, Symbol}
    data::T
end
function next(o::JSONTokens, t::Token)
    '{' .. t && return t(:curly_open)
    '}' .. t && return t(:curly_close)
    '[' .. t && return t(:square_open)
    ']' .. t && return t(:square_close)
    ':' .. t && return t(:colon)
    ',' .. t && return t(:comma)
    't' .. t && return t(:True, 4)
    'f' .. t && return t(:False, 5)
    'n' .. t && return t(:null, 4)
    ∈(b"\t\n\r ") .. t && return t(:whitespace, Before(!∈(b"\t\n\r ")))
    '"' .. t && return t(:string, u('"'))
    ∈(b"-0123456789") .. t && return t(:number, Before(!∈(b"-+eE.0123456789")))
    return t(:unknown)
end
