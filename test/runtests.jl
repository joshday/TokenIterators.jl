using Tokenizers
using Test

@testset "Tokenizers.jl" begin
    @testset "Patterns" begin
        @test isfirst(0x0, [0x0])
        @test isfirst('a', [UInt8('a')])
        @test isfirst(1:3, [1,2,3,4,5])
        @test isfirst(isspace, " ")
        @test isfirst(𝑠(isspace), b" ")
        @test isfirst(𝑠(¬(isspace)), b"abc")
        @test _findnext(0x0, [0x0], 1) == 1
        @test _findnext('a', [UInt8('a')], 1) == 1
        @test _findnext('a', "12a", 1) == 3
        @test _findnext(Before('a'), "12a", 1) == 2
        @test _findnext([0x0,0x1], [0x0,0x0,0x1], 1) == 2:3
        @test _findnext(First([0x0,0x1]), [0x0,0x0,0x1], 1) == 2
        @test _findnext(Last([0x0,0x1]), [0x0,0x0,0x1], 1) == 3
        @test _findnext(10, rand(UInt8, 20), 1) == 10
    end

    @testset "JSONTokens" begin
        for (data, kinds) in [
                b"[]" => [:square_open, :square_close]
                b"{}" => [:curly_open, :curly_close]
                b"true" => [:var"true"]
                b"false" => [:var"false"]
                b"null" => [:var"null"]
                codeunits("\"A string with \\\"escaped\\\" quotes.\"") => [:string]
                b"123" => [:number]
                b"1.23" => [:number]
                b"1.23e4" => [:number]
                b"1.23e-4" => [:number]
                b"-1.23E+4" => [:number]
            ]
            tokkinds = [x.kind for x in JSONTokens(data)]
            @test tokkinds == kinds
        end
        for url in [
                "https://raw.githubusercontent.com/clojure/data.json/refs/heads/master/dev-resources/1000-null.json"
                "https://raw.githubusercontent.com/clojure/data.json/refs/heads/master/dev-resources/1000-numbers.json"
                "https://raw.githubusercontent.com/clojure/data.json/refs/heads/master/dev-resources/1000-strings.json"
            ]
            file = download(url)
            data = read(file)
            collect(JSONTokens(data))
        end
    end
end
