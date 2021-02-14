module Chapter13 where
    main = putStr "chapter 13"

    data Tree = Int

    type Parser0 = String -> Tree

    -- A parser might not consume the entire string:
    type Parser1 = String -> (Tree, String)

    -- A parser might not result anything, or return many things:
    type Parser2 = String -> [(Tree, String)]

    type Parser a = String -> [(a, String)]
