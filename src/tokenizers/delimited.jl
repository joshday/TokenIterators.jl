#-----------------------------------------------------------------------------# DelimFileTokens
struct DelimFileTokens{T} <: AbstractTokenizer{T, Symbol}
    data::T
    delim::UInt8
    DelimFileTokens(data::T, delim::Char=',') where {T} = new{T}(data, UInt8(delim))
end
function next(o::DelimFileTokens, t::Token)
    s(isspace) .. t && return t(:whitespace, Before(s(!isspace)))
    t[1] == o.delim && return t(:delim)
    '"' .. t && return t(:string, u('"'))
    s(isletter) .. t && return t(:word, Before(s(!isletter)))
    s(isnumeric) .. t && return t(:numeric, Before(s(!isnumeric)))
    return t(:unknown)
end
