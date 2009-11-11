module Main where

left, right, up, down, closer, farther :: Coord
left = (-1, 0, 0)
right = (1, 0, 0)
up = (0, 1, 0)
down = (0, -1, 0)
closer = (0, 0, 1)
farther = (0, 0, -1)

type Piece = Int
type Coord = (Int, Int, Int)

processPieceDesc :: [(Char, Coord, (Coord, Coord), [String])] ->
  ([(Coord, S.Set Coord)], Piece -> Char)
processPieceDesc desc = (ps, showFunc) where
  ps = map (\ (_, pos0, deltas, grid) -> (pos0, doDeltaGrid deltas grid)) desc
  doDeltaGrid (xd, yd) grid =
  showFunc i = if i >= 0 && i < length desc
    then (map (\ (x, _, _, _) -> x) desc) !! i
    else error $ "showPiece: unknown piece " ++ show i

(pieces, showPiece) = processPieceDesc $ [
  ('G', (-3, 2, 1), (right, down), [
    ".......",
    ".     .",
    ".......",
    ".     .",
    "......."]),
  ('O', (-3, 2, -1), (right, down), [
    ".... ..",
    ".     .",
    "..   ..",
    ".     .",
    "......."]),
  ('B', (2, 1, -3), (closer, left), [
    ".... ..",
    ". .   .",
    "..   ..",
    ".     .",
    "......."]),
  ('P', (2, -1, -3), (closer, left), [
    ".. ....",
    ".     .",
    "..   ..",
    ".     .",
    "......."]),
  ('Y', (1, 3, -2), (down, closer), [
    ".... ..",
    ".     .",
    ".... ..",
    ".     .",
    "......."]),
  ('R', (-1, 3, -2), (down, closer), [
    ".. . ..",
    ".  .  .",
    ".......",
    ".     .",
    ".. ...."])]

main :: IO ()
main = do
  putStrLn "hi"

{- graveyard
instance Num Coord where
  (a1, b1, c1) + (a2, b2, c2) = (a1 + a2, b1 + b2, c1 + c2)
  (a1, b1, c1) * (a2, b2, c2) = (a1 * a2, b1 * b2, c1 * c2)
  (a1, b1, c1) - (a2, b2, c2) = (a1 - a2, b1 - b2, c1 - c2)
  negate (a, b, c) = (-a, -b, -c)
  abs (a, b, c) = (abs a, abs b, abs c)
  signum (a, b, c) = (signum a, signum b, signum c)
  fromInteger a = (a, 0, 0)
-}
