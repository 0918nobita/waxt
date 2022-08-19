[<AutoOpen>]
module Waxt.Location.Pos

/// ソースファイル上の位置
type Pos =
    private
    | Pos of line: uint * col: uint

    override this.ToString() =
        match this with
        | Pos (line, col) ->
            let line = line + 1u
            let col = col + 1u
            $"%i{line}:%i{col}"

module Pos =
    let origin = Pos(0u, 0u)

    let make line col = Pos(line, col)

    let line (Pos (line, _)) = line

    let col (Pos (_, col)) = col

    let toString (pos: Pos) = pos.ToString()

    let nextCol (Pos (line, col)) = Pos(line, col + 1u)

    let previousCol (Pos (line, col)) = Pos(line, col - 1u)

    let nextLine (Pos (line, _)) = Pos(line + 1u, 0u)
