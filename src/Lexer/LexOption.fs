namespace Waxt.Lexer

type Eol =
    | Cr
    | Lf
    | Crlf

type LexOption = LexOption of eol: Eol
