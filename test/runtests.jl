using Tokenizers
using Test

@testset "Tokenizers.jl" begin
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
