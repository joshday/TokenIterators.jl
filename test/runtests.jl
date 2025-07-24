using TokenIterators
using TokenIterators: s, u, width, Before, Last
using StringViews
using Test
using Downloads: download

#-----------------------------------------------------------------------------# Quick Checks
function quick_check(url, T)
    file = download(url)
    data = read(file)
    o = T(data)
    @test length(collect(o)) ≤ length(data)
end
quick_check("https://github.com/plotly/plotly.js/raw/v3.0.1/dist/plot-schema.json", JSONTokens)
quick_check("https://github.com/", HTMLTokens)
quick_check("https://schemas.opengis.net/kml/2.3/ogckml23.xsd", XMLTokens)
quick_check("https://gist.githubusercontent.com/netj/8836201/raw/6f9306ad21398ea43cba4f7d537619d0e07d5ae3/iris.csv", DelimFileTokens)


_token(s) = TokenIterators.after(Token(s))



#-----------------------------------------------------------------------------# starting patterns
@testset "starting patterns" begin
    t = _token("abc")

    @test 'a' .. t
    @test width(t, 'a') == 1

    @test "ab" .. t
    @test "abc" .. t
    @test width(t, "ab") == 2
    @test width(t, "abc") == 3

    @test s(isletter) .. t
    @test s(!isnumeric) .. t
    @test !s(isnumeric) .. t
    @test width(t, s(isletter)) == 1

    @test 0x61 .. t
    @test width(t, 0x61) == 1

    @test b"abc" .. t
    @test width(t, b"abc") == 3

    @test ('a', 'b') .. t
    @test width(t, ('a','b')) == 2

    @test (s(isletter), 'b', s(∈('a':'z'))) .. t
    @test width(t, (s(isletter), 'b', s(∈('a':'z')))) == 3
end

#-----------------------------------------------------------------------------# ending patterns
@testset "ending patterns" begin
    t = _token("abc \"text\" def")

    @test findnext('b', t, 2) == 2
    @test findnext(UInt8('b'), t, 2) == 2
    @test findnext(Last("bc"), t, 2) == 3
    @test findnext(Before("c"), t, 2) == 2
    @test findnext(Last(b"def"), t, 2) == length(t.data)
    @test findnext(Before(b"def"), t, 2) == length(t.data) - 3
    @test findnext(u('"'), t, 5) == findnext('"', StringView(t), 6)
end

#-----------------------------------------------------------------------------# Token shift
@testset "token shift" begin
    t = _token("abcdefg")
    @test length(t) == length(t.data)
    @test length(t >> 1) == length(t.data) - 1
end
