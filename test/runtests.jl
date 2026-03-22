using TokenIterators
using TokenIterators: Token, after, drop, emit, pick, format, token_error, Anchor, First, Last, Before, After, width, Unescaped
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
