using TokenIterators
using TokenIterators: Token, after, drop, emit, pick, format, token_error, Anchor, First, Last, Before, After, width, Unescaped, JSONTokens
using StringViews
using Test

# Helper: token spanning the full string
tok(s) = after(Token(s))

@testset "Token" begin
    t = tok("hello")
    @test length(t) == 5
    @test size(t) == (5,)
    @test t[1] == UInt8('h')
    @test StringView(t) == "hello"
    @test view(t) == codeunits("hello")
end

@testset "after / drop" begin
    t = tok("abcdef")
    @test StringView(after(t)) == ""           # after a full-span token is empty
    t2 = Token("abcdef")                       # sentinel: i=1, j=0
    @test StringView(after(t2)) == "abcdef"    # after sentinel is full span
    @test StringView(drop(t, 2)) == "cdef"
    @test drop(t, 0) == t
end

@testset "startswith - UInt8" begin
    t = tok("abc")
    @test startswith(t, UInt8('a'))
    @test !startswith(t, UInt8('b'))
    @test width(UInt8('a')) == 1
end

@testset "startswith - Char" begin
    t = tok("abc")
    @test startswith(t, 'a')
    @test !startswith(t, 'b')
    @test width('a') == 1

    # multibyte
    t2 = tok("αβγ")
    @test startswith(t2, 'α')
    @test !startswith(t2, 'β')
    @test width('α') == ncodeunits('α')
end

@testset "startswith - AbstractVector{UInt8}" begin
    t = tok("abcdef")
    @test startswith(t, b"abc")
    @test startswith(t, b"abcdef")
    @test !startswith(t, b"abd")
    @test !startswith(t, b"abcdefg")   # longer than token
    @test width(b"abc") == 3
end

@testset "startswith - AbstractString" begin
    t = tok("abcdef")
    @test startswith(t, "abc")
    @test !startswith(t, "abd")
    @test width("abc") == 3
end

@testset "startswith - Function" begin
    t = tok("abc")
    @test startswith(t, isletter ∘ Char)
    @test !startswith(t, isdigit ∘ Char)
    @test width(isletter) == 1
end

@testset "startswith - Tuple" begin
    t = tok("abc")
    @test startswith(t, ('a', 'b'))
    @test startswith(t, ('a', 'b', 'c'))
    @test !startswith(t, ('a', 'c'))
    @test width(t, ('a', 'b')) == 2
    @test width(t, ('a', 'b', 'c')) == 3
end

@testset "findnext - Function" begin
    t = tok("abcde")
    @test findnext(==(UInt8('c')), t, 1) == 3
    @test isnothing(findnext(==(UInt8('z')), t, 1))
end

@testset "findnext - Integer" begin
    t = tok("abcde")
    @test findnext(3, t, 1) == 3
end

@testset "findnext - UInt8" begin
    t = tok("abcde")
    @test findnext(UInt8('c'), t, 1) == 3
    @test isnothing(findnext(UInt8('z'), t, 1))
end

@testset "findnext - Char" begin
    t = tok("abcde")
    @test findnext('c', t, 1) == 3
    @test isnothing(findnext('z', t, 1))
end

@testset "findnext - AbstractString with Anchor" begin
    t = tok("hello world")
    @test findnext("world", t, 1, First)  == 7
    @test findnext("world", t, 1, Last)   == 11
    @test findnext("world", t, 1, Before) == 6
    @test findnext("world", t, 1, After)  == 12
    @test findnext("zzz", t, 1, Before)   == length(t)   # nothing fallback
end

@testset "pick" begin
    t = tok("hello world")
    @test pick(t, 3:7, First)  == 3
    @test pick(t, 3:7, Last)   == 7
    @test pick(t, 3:7, Before) == 2
    @test pick(t, 3:7, After)  == 8
    @test pick(t, nothing, Before) == length(t)
    @test_throws Exception pick(t, nothing, First)
end

@testset "findnext - Unescaped" begin
    t = tok("""say "hello" and \\"world\\" """)
    u = Unescaped('"', '\\')
    # finds first unescaped "
    @test findnext(u, t, 1) == findfirst('"', StringView(t))
    # finds closing unescaped " after hello
    i = findnext(u, t, 1)
    j = findnext(u, t, i + 1)
    @test StringView(t)[i:j] == "\"hello\""
    # escaped " is skipped
    t2 = tok("no quote here \\\" only escaped")
    @test isnothing(findnext(Unescaped('"', '\\'), t2, 1))
    # custom escape character
    t3 = tok("a|b||c")
    @test findnext(Unescaped('|', '|'), t3, 1) == 2   # first | at position 2
    @test findnext(Unescaped('|', '|'), t3, 3) == 4   # next | at position 4
end

@testset "format" begin
    @test format(1000) == "1_000"
    @test format(1000000) == "1_000_000"
    @test format(999) == "999"
end

@testset "JSONTokens" begin
    sv(t) = StringView(t)

    # helper: tokenize and return (kind, text) pairs
    tokenize(s) = [(t.kind, sv(t)) for t in JSONTokens.Tokenizer(codeunits(s))]

    @testset "single-char tokens" begin
        @test tokenize("{")[1] == (JSONTokens.curly_open,   "{")
        @test tokenize("}")[1] == (JSONTokens.curly_close,  "}")
        @test tokenize("[")[1] == (JSONTokens.square_open,  "[")
        @test tokenize("]")[1] == (JSONTokens.square_close, "]")
        # colon/comma are state-guarded; test them in context
        @test tokenize("{\"k\":")[3] == (JSONTokens.colon, ":")
        @test tokenize("{\"k\":1,")[5] == (JSONTokens.comma, ",")
    end

    @testset "keywords" begin
        @test tokenize("true")[1]  == (JSONTokens.True,  "true")
        @test tokenize("false")[1] == (JSONTokens.False, "false")
        @test tokenize("null")[1]  == (JSONTokens.null,  "null")
    end

    @testset "whitespace" begin
        @test tokenize(" ")[1]       == (JSONTokens.whitespace, " ")
        @test tokenize("   ")[1]     == (JSONTokens.whitespace, "   ")
        @test tokenize("\t\n\r ")[1] == (JSONTokens.whitespace, "\t\n\r ")
    end

    @testset "key vs string" begin
        # key: string in object key position
        @test tokenize("{\"hello\":")[2]        == (JSONTokens.key, "\"hello\"")
        @test tokenize("{\"with \\\"esc\\\"\":")[2] == (JSONTokens.key, "\"with \\\"esc\\\"\"")
        @test tokenize("{\"\":")[2]             == (JSONTokens.key, "\"\"")
        # string: string in value position
        @test tokenize("{\"k\":\"hello\"}")[4]        == (JSONTokens.string, "\"hello\"")
        @test tokenize("{\"k\":\"with \\\"esc\\\"\"")[4] == (JSONTokens.string, "\"with \\\"esc\\\"\"")
        @test tokenize("{\"k\":\"\"")[4]              == (JSONTokens.string, "\"\"")
    end

    @testset "number" begin
        @test tokenize("42")[1]     == (JSONTokens.number, "42")
        @test tokenize("-1")[1]     == (JSONTokens.number, "-1")
        @test tokenize("3.14")[1]   == (JSONTokens.number, "3.14")
        @test tokenize("-1e7")[1]   == (JSONTokens.number, "-1e7")
        @test tokenize("1.5E-3")[1] == (JSONTokens.number, "1.5E-3")
    end

    @testset "full document" begin
        toks = tokenize("""{"key": "value", "n": -1e7}""")
        kinds = first.(toks)
        texts = last.(toks)
        @test kinds == [
            JSONTokens.curly_open,
            JSONTokens.key,         # object key → :key kind
            JSONTokens.colon,
            JSONTokens.whitespace,
            JSONTokens.string,      # object value → :string kind
            JSONTokens.comma,
            JSONTokens.whitespace,
            JSONTokens.key,
            JSONTokens.colon,
            JSONTokens.whitespace,
            JSONTokens.number,
            JSONTokens.curly_close,
        ]
        @test texts == ["{", "\"key\"", ":", " ", "\"value\"", ",", " ", "\"n\"", ":", " ", "-1e7", "}"]
    end

    @testset "RULES dict" begin
        @test haskey(JSONTokens.RULES, JSONTokens.curly_open)
        @test JSONTokens.RULES[JSONTokens.curly_open] == '{'
        @test JSONTokens.RULES[JSONTokens.key]    isa TokenIterators.Rule
        @test JSONTokens.RULES[JSONTokens.string] isa TokenIterators.Rule
    end
end
