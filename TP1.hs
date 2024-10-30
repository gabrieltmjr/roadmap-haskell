{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import qualified Data.List
--import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

type Visited = Bool

{-
City - Source node
[(City, Distance)] - List of nodes connected to
Distance - Distance to arrive at node
City - Previous Node

-}
type DijkstraNode = (City, [(City, Distance)], Distance, City)
type RoadMapDijkstra = [DijkstraNode]

{-

-}
type RoadMapDijkstraQueue = [(City, Distance)]

inf :: Int
inf = 1000000

und :: City
und = "undefined"

-- This function takes all the cities in the RoadMap (returned by cities') and removes all duplicates.
cities :: RoadMap -> [City]
cities [] = []
cities roadMap = map head (Data.List.group (Data.List.sort (cities' roadMap)))

-- This auxiliary function creates a List with every city in every road in the RoadMap, including duplicates.
cities' :: RoadMap -> [City]
cities' [] = []
cities' ((city1, city2, _):xs) = [city1] ++ [city2] ++ cities [(city1_, city2_, distance) | (city1_, city2_, distance)<-xs]

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] _ _ = False
areAdjacent ((rmcity,rmcity2,rmdistance):xs) city1 city2 | (rmcity == city1 && rmcity2 == city2) || (rmcity == city2 && rmcity2 == city1) = True
                                                         | otherwise = areAdjacent xs city1 city2

distance :: RoadMap -> City -> City -> Maybe Distance
distance ((c1, c2, d):xs) city1 city2 | c1 == city1 && c2 == city2 = Just d
                                      | otherwise = distance xs city1 city2
distance [] city1 city2 = Nothing

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] _ = []
adjacent ((rmcity, rmcity2, rmdistance):xs) city | rmcity == city && areAdjacent ((rmcity, rmcity2, rmdistance):xs) rmcity rmcity2 = (rmcity2, rmdistance): adjacent xs city
                                                 | rmcity2 == city && areAdjacent ((rmcity, rmcity2, rmdistance):xs) rmcity rmcity2 = (rmcity, rmdistance): adjacent xs city
                                                 | otherwise = adjacent xs city

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance = undefined

-- This auxiliary function returns how many connections a city has in the format (City, Number of Connections), by checking the city's adjacency
connectionsOfCity :: RoadMap -> City -> (String,Int)
connectionsOfCity roadmap city = (city, length (adjacent roadmap city))

-- This auxiliary function returns all connections of each city in the RoadMap in the format of a list of (City, Number of Connections), sorted from highest to lowest
connectionsOfCities :: RoadMap -> [(String,Int)]
connectionsOfCities roadmap = Data.List.sortBy (\(_, conn1) (_, conn2) -> compare conn2 conn1) [ connectionsOfCity roadmap auxcity | auxcity <- auxcities]
                              where auxcities = cities roadmap

rome :: RoadMap -> [City]
rome roadmap = [city | (city, conn) <- connectionsOfCities roadmap, conn == highest]
               where (_, highest) = head (connectionsOfCities roadmap)

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

sourceToZero :: RoadMapDijkstra -> City -> RoadMapDijkstra
sourceToZero [] _ = []
sourceToZero ((city, destinations, distance, previous):xs) source | city == source = (city, destinations, 0, und): sourceToZero xs source
                                                                 | otherwise = (city, destinations, distance, und): sourceToZero xs source
roadMapToRoadMapDijkstra :: RoadMap -> City -> RoadMapDijkstra
roadMapToRoadMapDijkstra roadmap source = sourceToZero [(city, adjacent roadmap city, inf, und) | city <- cities roadmap] source

getDistance :: RoadMapDijkstra -> City -> Distance
getDistance roadmap city |  null distance = -1
                         | otherwise = head distance
    where distance = [rmdistance | (rmcity, _, rmdistance, _) <- roadmap, city == rmcity]

dijkstraQueue :: RoadMapDijkstra -> RoadMapDijkstraQueue
dijkstraQueue [] = []
dijkstraQueue ((city, destinations, distance, previous):tail) = Data.List.filter (\(x,y) -> y /= -1) ((city, distance) : [(destCity, getDistance ((city, destinations, distance, previous):tail) destCity ) |(destCity, _) <- destinations]) ++ dijkstraQueue tail

dijkstraFilteredQueue :: RoadMapDijkstraQueue -> RoadMapDijkstraQueue
dijkstraFilteredQueue [] = []
dijkstraFilteredQueue (x:xs) = x: dijkstraFilteredQueue (Data.List.filter (/= x)  xs)

dijkstraQueueRemove :: RoadMapDijkstraQueue -> RoadMapDijkstraQueue
dijkstraQueueRemove (head:tail) = tail


dijkstraFilteredRoadMap :: RoadMapDijkstra -> RoadMapDijkstra
dijkstraFilteredRoadMap [] = []
dijkstraFilteredRoadMap ((city, destinations, distance, previous):xs) = 
                      (city, destinations, distance, previous): dijkstraFilteredRoadMap (Data.List.filter (\(nextCity, _, _, _) -> nextCity /= city) xs)

dijkstraDistanceDefine :: DijkstraNode -> RoadMapDijkstra -> RoadMapDijkstra
dijkstraDistanceDefine (city, destinations, distance, previous)
                       nodes = dijkstraFilteredRoadMap ([(toChangeCity, toChangeDestinations, distance + destDistance, city)
                       | (toChangeCity, toChangeDestinations, toChangeDistance, toChangePrevious) <- nodes,
                         (destCity, destDistance) <- destinations,
                          destCity == toChangeCity,
                          (distance + destDistance) < toChangeDistance, toChangeDistance == inf] ++ nodes)


dijkstraFindShortestPathDistance :: RoadMapDijkstra -> City -> Distance
dijkstraFindShortestPathDistance roadmap destination = let matchingNodes = filter (\(c1, _, _, _) -> c1 == destination) roadmap
    in case matchingNodes of
        [] -> error "Destination not found in the roadmap"  -- Or handle in some other way
        _  -> let (city, _, distance, _) = Data.List.minimumBy (\(_, _, dist1, _) (_, _, dist2, _) -> compare dist1 dist2) matchingNodes
              in distance


updateQueue :: RoadMapDijkstra -> RoadMapDijkstraQueue -> RoadMapDijkstraQueue
updateQueue _ [] = []
updateQueue [] queue = queue  -- Se não houver mais elementos no RoadMap, retornamos a fila original
updateQueue ((city, destinations, distance, previous):restOfRoadMap) queue =
            updateQueue restOfRoadMap (updateCityInQueue (city, distance) queue)

sortedQueue :: RoadMapDijkstra -> RoadMapDijkstraQueue -> RoadMapDijkstraQueue
sortedQueue roadMapDijkstra queue = Data.List.sortBy (\(_, distance1) (_, distance2) -> compare distance1 distance2) (updateQueue roadMapDijkstra queue)

-- Função auxiliar para atualizar a distância de uma cidade específica na fila
updateCityInQueue :: (String, Int) -> RoadMapDijkstraQueue -> RoadMapDijkstraQueue
updateCityInQueue (city, newDistance) [] = []
updateCityInQueue (city, newDistance) ((queueCity, queueDistance) : queueTail)
    | city == queueCity = (queueCity, newDistance) : queueTail  -- Atualiza a cidade
    | otherwise = (queueCity, queueDistance) : updateCityInQueue (city, newDistance) queueTail

-- Apply the Dijkstra algorithm to an auxiliary type RoadMapDijksta with the current city being visited, a List of cities that serve as queue
dijkstra :: RoadMapDijkstra -> RoadMapDijkstraQueue -> RoadMapDijkstra
dijkstra roadmap [] = roadmap -- if queue is empty
dijkstra roadMapDijkstra ((queueCity, queueDistance):queueTail) =
                                                                dijkstra (dijkstraDistanceDefine chosenFromQueue roadMapDijkstra) (sortedQueue (dijkstraDistanceDefine chosenFromQueue roadMapDijkstra) queueTail)
                                 where
                                    chosenFromQueue = head (Data.List.filter (\(city, _, _, _) -> city == queueCity) roadMapDijkstra)

roadMapDijkstraToPath :: RoadMapDijkstra -> City -> City -> Path
roadMapDijkstraToPath [(city, _, _, "undefined")] _ _ = [city]
roadMapDijkstraToPath ((city, destinations, distance, previous):restOfRoadMap) source destination | city == destination = city : roadMapDijkstraToPath restOfRoadMap source previous
                                                                                                   | otherwise = roadMapDijkstraToPath restOfRoadMap source destination

roadMapDijkstraToPaths :: RoadMapDijkstra -> City -> City -> [Path]
roadMapDijkstraToPaths roadmap source destination = [ roadMapDijkstraToPath roadmap source destination | (city, _, distance, _) <- roadmap, city == destination && distance == dijkstraFindShortestPathDistance roadmap destination]

callDijkstra :: City -> RoadMap -> RoadMapDijkstra
callDijkstra source roadmap = dijkstra (roadMapToRoadMapDijkstra roadmap source) (dijkstraFilteredQueue (dijkstraQueue (roadMapToRoadMapDijkstra roadmap source)))

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap source destination = roadMapDijkstraToPaths (callDijkstra source roadmap) source destination

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

gTest4 :: RoadMap -- graph with 3 different shortest paths with same distance
gTest4 = [("A", "B", 5), ("A", "C", 2), ("C", "D", 3),  -- Path A-C-D with distance 5
          ("B", "D", 5),                                  -- Path A-B-D with distance 5
          ("A", "E", 1), ("E", "F", 4), ("F", "D", 4),    -- Path A-E-F-D with distance 5
          ("D", "G", 2), ("F", "G", 3)]