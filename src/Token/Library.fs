namespace Waxt.Token

type Token =
    | Ident of Ident
    | LeftParen of LeftParen
    | RightParen of RightParen
    | LeftBracket of LeftBracket
    | RightBracket of RightBracket

module Token =
    let toJSON (token: Token) =
        match token with
        | Ident ident -> Ident.toJSON ident
        | LeftParen leftParen -> LeftParen.toJSON leftParen
        | RightParen rightParen -> RightParen.toJSON rightParen
        | LeftBracket leftBracket -> LeftBracket.toJSON leftBracket
        | RightBracket rightBracket -> RightBracket.toJSON rightBracket
