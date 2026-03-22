using BenchmarkTools
using TokenIterators
using TokenIterators: Token, after, drop, emit, width, Unescaped, JSONTokens, XMLTokens
using StringViews

const SUITE = BenchmarkGroup()

#-----------------------------------------------------------------------------# Helpers
tok(s) = after(Token(s))

function count_tokens(tokenizer)
    n = 0
    for _ in tokenizer
        n += 1
    end
    n
end

#-----------------------------------------------------------------------------# Test data
const JSON_SMALL = """{"key": "value", "n": -1e7, "flag": true}"""

const JSON_MEDIUM = """
{
  "name": "TokenIterators",
  "version": "0.4.0",
  "dependencies": ["BenchmarkTools", "StringViews"],
  "config": {
    "timeout": 30,
    "retries": 3,
    "tags": ["fast", "zero-copy", "iterators"],
    "nested": {"a": 1, "b": 2, "c": 3}
  },
  "items": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
  "enabled": true,
  "ratio": 3.14159,
  "description": "A zero-copy tokenizer library for Julia"
}
"""

const JSON_LARGE = let
    entries = join(("{\"id\": $i, \"value\": \"item_$i\", \"score\": $(i * 0.1), \"active\": $(iseven(i))}" for i in 1:500), ",\n")
    "[" * entries * "]"
end

const XML_SMALL = """<root><child attr="val">text</child></root>"""

const XML_MEDIUM = """
<?xml version="1.0" encoding="UTF-8"?>
<library name="TokenIterators">
  <!-- Main package metadata -->
  <package version="0.4.0" author="Josh Day">
    <description>Zero-copy token iterators for Julia</description>
    <deps>
      <dep name="StringViews" version="1.3.4"/>
      <dep name="StyledStrings" version="1.11.0"/>
    </deps>
  </package>
  <features>
    <feature name="zero-copy">Tokens reference original data</feature>
    <feature name="stateful">Support for stateful tokenization</feature>
    <feature name="composable">Composable pattern matchers</feature>
  </features>
</library>
"""

const XML_LARGE = let
    items = join(("""  <item id="$i" class="entry-$i" active="$(iseven(i))">
    <title>Item $i</title>
    <value>$(i * 0.5)</value>
    <tags><tag>tag$(i % 5)</tag><tag>group$(i % 10)</tag></tags>
  </item>""" for i in 1:200), "\n")
    "<root>\n" * items * "\n</root>"
end

#-----------------------------------------------------------------------------# JSON benchmarks
SUITE["json"] = BenchmarkGroup()

SUITE["json"]["small"]  = @benchmarkable count_tokens(JSONTokens.Tokenizer(codeunits($JSON_SMALL)))
SUITE["json"]["medium"] = @benchmarkable count_tokens(JSONTokens.Tokenizer(codeunits($JSON_MEDIUM)))
SUITE["json"]["large"]  = @benchmarkable count_tokens(JSONTokens.Tokenizer(codeunits($JSON_LARGE)))

# Collect all tokens (measures allocation of the result vector, not iteration itself)
SUITE["json"]["collect_small"]  = @benchmarkable collect(JSONTokens.Tokenizer(codeunits($JSON_SMALL)))
SUITE["json"]["collect_medium"] = @benchmarkable collect(JSONTokens.Tokenizer(codeunits($JSON_MEDIUM)))

#-----------------------------------------------------------------------------# XML benchmarks
SUITE["xml"] = BenchmarkGroup()

SUITE["xml"]["small"]  = @benchmarkable count_tokens(XMLTokens.Tokenizer(codeunits($XML_SMALL)))
SUITE["xml"]["medium"] = @benchmarkable count_tokens(XMLTokens.Tokenizer(codeunits($XML_MEDIUM)))
SUITE["xml"]["large"]  = @benchmarkable count_tokens(XMLTokens.Tokenizer(codeunits($XML_LARGE)))

#-----------------------------------------------------------------------------# startswith benchmarks
SUITE["startswith"] = BenchmarkGroup()

const T_HELLO  = tok("hello world")
const T_DIGITS = tok("12345 rest")
const T_SPACES = tok("   text")
const T_QUOTE  = tok("\"escaped \\\" string\"")

SUITE["startswith"]["uint8"]    = @benchmarkable startswith($T_HELLO, UInt8('h'))
SUITE["startswith"]["char"]     = @benchmarkable startswith($T_HELLO, 'h')
SUITE["startswith"]["string"]   = @benchmarkable startswith($T_HELLO, "hello")
SUITE["startswith"]["bytes"]    = @benchmarkable startswith($T_HELLO, b"hello")
SUITE["startswith"]["function"] = @benchmarkable startswith($T_DIGITS, isdigit)
SUITE["startswith"]["tuple"]    = @benchmarkable startswith($T_HELLO, ('h', 'e', 'l'))

#-----------------------------------------------------------------------------# findnext benchmarks
SUITE["findnext"] = BenchmarkGroup()

const T_LONG = tok("abcdefghijklmnopqrstuvwxyz0123456789")

SUITE["findnext"]["uint8"]    = @benchmarkable findnext(UInt8('z'), $T_LONG, 1)
SUITE["findnext"]["char"]     = @benchmarkable findnext('z', $T_LONG, 1)
SUITE["findnext"]["function"] = @benchmarkable findnext(isdigit, $T_LONG, 1)
SUITE["findnext"]["integer"]  = @benchmarkable findnext(10, $T_LONG, 1)

#-----------------------------------------------------------------------------# Unescaped benchmarks
SUITE["unescaped"] = BenchmarkGroup()

# Short string: quote found quickly
const T_SHORT_QUOTED = tok("\"hello\"")
# Long string with escaped quotes: must scan all
const T_LONG_QUOTED  = tok("\"" * "x" ^ 100 * "\\\"" ^ 10 * "end\"")
# No unescaped quote (pathological)
const T_NO_CLOSE     = tok("\"" * "a\\\"" ^ 50)

const U_DQ = Unescaped(UInt8('"'))

SUITE["unescaped"]["short"] = @benchmarkable findnext($U_DQ, $T_SHORT_QUOTED, 2)
SUITE["unescaped"]["long"]  = @benchmarkable findnext($U_DQ, $T_LONG_QUOTED, 2)
SUITE["unescaped"]["none"]  = @benchmarkable findnext($U_DQ, $T_NO_CLOSE, 2)

#-----------------------------------------------------------------------------# Token construction and view benchmarks
SUITE["token"] = BenchmarkGroup()

const DATA = codeunits("hello world this is a test string")
SUITE["token"]["construct"]  = @benchmarkable Token($DATA, nothing, 1, length($DATA))
SUITE["token"]["view"]       = @benchmarkable view($T_LONG)
SUITE["token"]["stringview"] = @benchmarkable StringView($T_LONG)
SUITE["token"]["length"]     = @benchmarkable length($T_LONG)

#-----------------------------------------------------------------------------# Run and display results
if abspath(PROGRAM_FILE) == @__FILE__
    tune!(SUITE)
    results = run(SUITE; verbose=false)
    display(results)
end
