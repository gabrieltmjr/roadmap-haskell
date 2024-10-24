import qualified Data.List
--import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

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

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

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