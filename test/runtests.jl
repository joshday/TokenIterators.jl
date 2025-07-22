using TokenIterators
using Test
using Downloads: download

#-----------------------------------------------------------------------------# Quick Checks
function quick_check(url, T)
    file = download(url)
    data = read(file)
    o = T(data)
    collect(o)
end

quick_check("https://github.com/plotly/plotly.js/raw/v3.0.1/dist/plot-schema.json", JSONTokens)
quick_check("https://github.com/", HTMLTokens)
quick_check("https://schemas.opengis.net/kml/2.3/ogckml23.xsd", XMLTokens)
# quick_check("https://gist.githubusercontent.com/netj/8836201/raw/6f9306ad21398ea43cba4f7d537619d0e07d5ae3/iris.csv", DelimFileTokens)


# @testset "TokenIterators.jl" begin
#     @testset "Patterns" begin
#         @test isfirst(0x0, [0x0])
#         @test isfirst('a', [UInt8('a')])
#         @test isfirst(1:3, [1,2,3,4,5])
#         @test isfirst(isspace, " ")
#         @test isfirst(𝑠(isspace), b" ")
#         @test isfirst(𝑠(¬(isspace)), b"abc")
#         @test _findnext(0x0, [0x0], 1) == 1
#         @test _findnext('a', [UInt8('a')], 1) == 1
#         @test _findnext('a', "12a", 1) == 3
#         @test _findnext(Before('a'), "12a", 1) == 2
#         @test _findnext([0x0,0x1], [0x0,0x0,0x1], 1) == 2:3
#         @test _findnext(First([0x0,0x1]), [0x0,0x0,0x1], 1) == 2
#         @test _findnext(Last([0x0,0x1]), [0x0,0x0,0x1], 1) == 3
#         @test _findnext(10, rand(UInt8, 20), 1) == 10
#     end

#     @testset "JSONTokens" begin
#         for (data, kinds) in [
#                 b"[]" => [:square_open, :square_close]
#                 b"{}" => [:curly_open, :curly_close]
#                 b"true" => [:var"true"]
#                 b"false" => [:var"false"]
#                 b"null" => [:var"null"]
#                 codeunits("\"A string with \\\"escaped\\\" quotes.\"") => [:string]
#                 b"123" => [:number]
#                 b"1.23" => [:number]
#                 b"1.23e4" => [:number]
#                 b"1.23e-4" => [:number]
#                 b"-1.23E+4" => [:number]
#             ]
#             tokkinds = [x.kind for x in JSONTokens(data)]
#             @test tokkinds == kinds
#         end
#         @test_nowarn for url in [
#                 "https://raw.githubusercontent.com/clojure/data.json/refs/heads/master/dev-resources/1000-null.json"
#                 "https://raw.githubusercontent.com/clojure/data.json/refs/heads/master/dev-resources/1000-numbers.json"
#                 "https://raw.githubusercontent.com/clojure/data.json/refs/heads/master/dev-resources/1000-strings.json"
#                 "https://github.com/plotly/plotly.js/raw/v3.0.1/dist/plot-schema.json"
#             ]
#             file = download(url)
#             data = read(file)
#             t = JSONTokens(data)
#             collect(t)
#         end
#     end

#     @testset "HTMLTokens" begin
#         @test_nowarn for url in ["https://github.com", "https://google.com"]
#             collect(HTMLTokens(read(download(url))))
#         end
#     end

#     @testset "XMLTokens" begin
#         @test_nowarn for url in [
#                 "https://developers.google.com/static/kml/documentation/KML_Samples.kml"
#                 "https://schemas.opengis.net/kml/2.2.0/ogckml22.xsd"
#             ]
#             collect(XMLTokens(read(download(url))))
#         end
#     end

#     @testset "DelimFileTokens" begin
#         @test_nowarn for url in [
#                 "https://raw.githubusercontent.com/JuliaData/CSV.jl/refs/heads/main/test/testfiles/FL_insurance_sample.csv"
#             ]
#             collect(DelimFileTokens(read(download(url))))
#         end
#     end

#     @testset "CharFunTokens" begin
#         @test_nowarn for url in [
#                 "https://raw.githubusercontent.com/JuliaData/CSV.jl/refs/heads/main/test/testfiles/FL_insurance_sample.csv"
#             ]
#             data = read(download(url))
#             collect(CharFunTokens(data, isletter))
#             collect(CharFunTokens(data, isascii))
#             collect(CharFunTokens(data, islowercase))
#         end
#     end
# end
