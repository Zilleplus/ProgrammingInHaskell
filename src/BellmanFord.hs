module BellmanFord where
data Node = Node{
    name :: String
}deriving(Show,Eq)

data Edge = Edge{
    start :: Node
    ,end ::Node
    ,cost :: Int
}deriving(Show,Eq)


demo :: ([Node],[Edge])
demo = (nodes,edges)
    where 
        nodeA = Node{name="A"}
        nodeB = Node{name="B"}
        nodeC = Node{name="C"}
        nodeD = Node{name="D"}
        nodeE = Node{name="E"}
        nodes = [nodeA,nodeB,nodeC,nodeD,nodeE]
        edges =
                [Edge{start=nodeA,end=nodeB,cost=1}
                ,Edge{start=nodeB,end=nodeC,cost=2}
                ,Edge{start=nodeA,end=nodeE,cost=1}
                ,Edge{start=nodeA,end=nodeD,cost=1}
                ,Edge{start=nodeD,end=nodeC,cost=1}
                ,Edge{start=nodeE,end=nodeB,cost=1}]


dijkstra :: ([Node],[Edge]) -> [Edge]
dijkstra (node,edges) = []

bellmanFord :: ([Node],[Edge]) -> [Edge]
bellmanFord (node,edges) = []

-- isValidPath :: ([Node],[Edge]) -> [Edge] -> Bool
-- isValidPath graph path = True 

--             where
--                 isContinue es = foldl (\(buff, new)-> ) True t a
                    