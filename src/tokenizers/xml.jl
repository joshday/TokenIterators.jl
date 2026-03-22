#-----------------------------------------------------------------------------# XMLTokens
mutable struct XMLTokens{T} <: AbstractTokenizer{T, Symbol}
    data::T
    state::Set{Symbol}
    XMLTokens(data::T) where {T} = new{T}(data, Set{Symbol}())
end
function next(o::XMLTokens, t::Token)
    if "<script" .. t
        push!(o.state, :in_script); push!(o.state, :in_tag)
        return t(:tag_start, 7)
    end
    if "</" .. t
        delete!(o.state, :in_script)
        return t(:close_tag, '>')
    end
    s(isspace) .. t && return t(:whitespace, Before(s(!isspace)))
    "<?" .. t && return t(:processing_instruction, Last("?>"), 3)
    "<!-" .. t && return t(:comment, Last("-->"))
    "<!" .. t && return t(:doctype, '>')
    "<![" .. t && return t(:cdata, Last("]]>"))
    if '<' .. t
        push!(o.state, :in_tag)
        return t(:tag_start, Before(s(!isletter)))
    end
    if :in_tag in o.state
        s(isletter) .. t && return t(:attr_key, Before(s(x -> isspace(x) || x == '=')))
        '=' .. t && return t(:equals)
        '"' .. t && return t(:attr_value, '"')
        if '>' .. t
            delete!(o.state, :in_tag)
            return t(:tag_end)
        end
        if "/>" .. t
            delete!(o.state, :in_tag)
            return t(:self_close_tag_end)
        end
    end
    return t(:text, Before(b"</"))
end
