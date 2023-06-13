-- right triangles with side <= m and perimeter == p
module RightTriangles where

triangles m p = [(x, y, z) | x <- [1 .. m], y <- [1 .. m], z <- [1 .. m], x ^ 2 + y ^ 2 == z ^ 2, x + y + z == p]