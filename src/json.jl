module JSON

struct Seq{T} end
Seq(x) = Seq{Tuple(x)}()




ws = Tuple(" \t\n\r")

sign = Tuple("+-")

onenine = Tuple("123456789")

digit = ('0', onenine)

digits =

exponent = (
    "",
    Seq(Tuple("eE"), sign, digits)
)

fraction = ("", Seq('.', digits))

integer = (digit, Seq(onenine, digits), Seq('-', digit), Seq('-', onenine, digits))
number = (integer, fraction, exponent)
hex = (digit, 'a':'f', 'A':'F')
escape = ('"', '\\', '/', 'b', 'f', 'n', 'r', 't', Seq('u', hex, hex, hex, hex))



end
