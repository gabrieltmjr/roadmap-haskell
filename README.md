# PFL - Haskell Coursework

## Group T13G11
- Gabriel Tomaz Machado Júnior (202008860) - 50% contribution
- Manuel Rivera Villatte (202401168) - 50% contribution


## Functions

### 1. cities :: RoadMap -> [City] (Manuel)

_Returns all the cities in the graph._

### 2. areAdjacent :: RoadMap -> City -> City -> Bool (Gabriel)

_Returns a boolean indicating whether two cities are linked directly._

This function recursively goes through the RoadMap and tests each city in the tuple with the given cities to see if they match. If they match, it returns True, otherwise False.

### 3. distance :: RoadMap -> City -> City -> Maybe Distance (Manuel)

_Returns a Just value with the distance between two cities connected directly, given two city names, and Nothing otherwise._

### 4. adjacent :: RoadMap -> City -> [(City,Distance)] (Gabriel)

_Returns the cities adjacent to a particular city (i.e. cities with a direct edge between them) and the respective distances to them._

This function tests if the given city belongs to the current tuple in the recursion call and creates a list with the destination (the other city in the tuple) and the distance, while recursively going through the roadmap. 

### 5. pathDistance :: RoadMap -> Path -> Maybe Distance (Manuel)

_Returns the sum of all individual distances in a path between two cities in a Just value, if all the consecutive pairs of cities are directly connected by roads. Otherwise, it returns a Nothing._

### 6. rome :: RoadMap -> [City] (Gabriel)

_Returns the names of the cities with the highest number of roads connecting to them (i.e. the vertices with the highest degree)._

This function uses 2 auxiliary functions to get the connections of all cities and check which is the city with most connections. One of the auxiliary functions uses the adjacent function to get the list of nodes adjacent and then calculate it's length to get the nummber of connections.

### 7. isStronglyConnected :: RoadMap -> Bool (Manuel)

_Returns a boolean indicating whether all the cities in the graph are connected in the roadmap (i.e., if every city is reachable from every other city)._

### 8. shortestPath :: RoadMap -> City -> City -> [Path] (Gabriel)

_Computes all shortest paths connecting the two cities given as input. Note that there may be more than one path with the same total distance. If there are no paths between the input cities, then return an empty list. Note that the (only) shortest path between a city c and itself is [c]._

This function uses a lot of auxiliary functions to calculate the shortest path. The algorithm implemented was Dijkstra's, and it was quite challenging to make it work with Haskell. Most of the functions use recursion, which can be a problem when dealing with bigger graphs. Also, the solution created only returns one shortest path, because the algorithm only goes through the RoadMap once, returning the shortest path found. 

### 9. travelSales :: RoadMap -> Path, given a roadmap (Manuel)

_Returns a solution of the Traveling Salesman Problem (TSP). In this problem, a traveling salesperson has to visit each city exactly once and come back to the starting town. The problem is to find the shortest route, that is, the route whose total distance is minimum. This problem has a known solution using dynamic programming. Any optimal TSP path will be accepted and the function only needs to return one of them, so the starting city (which is also the ending city) is left to be chosen by each group. Note that the roadmap might not be a complete graph (i.e. a graph where all vertices are connected to all other vertices). If the graph does not have a TSP path, then return an empty list._

## About the Solution for the Traveling Salesman Problem

To provide a somewhat efficient solution for the problem, we decided change the representation of the RoadMap to an Adjacency Matrix,
so lookups for distances were faster. First, we needed a way to convert a RoadMap to an Adjacency Matrix. For that, we have the
function `roadMapToAdjMatrix`. It is worth noting that since RoadMap `City` names are of type `String`, while Adjacency Matrices must have `Int` indexes, we opted to convert `City` names from `String` to `Int` using `read`. For this to work, the `City` names must be valid integers. This is also the reason behind the type `City'`, which is of type `Int`, used instead of the type `City` for this solution.

The auxiliary function `getDistance'` returns the distance between two Cities. The name `getDistance` is already in use for a function that does the same but for a standard RoadMap.

Our solution for the **Traveling Salesman Problem** is based on the **Held-Karp algorithm**, which uses dynamic programming to reduce the complexity from factorial time to `O(n^2 * 2^n)`, and makes use of the GHC Modules Data.Array and Data.Bits.

The function `tsp` uses the dynamic function `tspDP` to recursively calculate the shortest paths. A key aspect of how this solution works is the tracking of visited cities, using the variable `visited`. This variable is a bitmask: a single integer that shows in constant time if a city was visited or not. The GHC Module `Data.Bits` proved extremely useful for this matter, providing fast methods for updating the visited cities (`.|.`), and testing if a city was visited (`testBit`).

The function also uses a memoization table that stores intermediate results of the minimal path for every city that is visited for the first time, to increase efficiency.

In every recursive call, the function checks if all cities are visited (`visited == (1 'Data.Bits.shiftL' numCities) - 1`). If they are, it returns the path. If not, it checks for all unvisited cities so far (`unvisited`), and verifies if there is a route for each of them. If a route exists, the algorithm updates `visited`, and calls `tspDP` recursively for the `next` city. The result of this recursive call is added to the `current` city, forming a partial `path`. Each `path` is added to `paths`. Each element in `paths` is either `Nothing`, if there's no connection available to an unvisited city, or a `path`.

The resulting `paths` are filtered so that there are no `Nothing` elements inside of it, and also sorted so the `path` with the least total distance is computed first.

Thus, for a given RoadMap, the function `travelSales` converts the RoadMap to an Adjacency Matrix, calculates the amount of cities using `length (cities roadmap)`, and calculates the `tspPath` using the function `tsp`. If a valid solution is found, it is returned, otherwise returning an empty list. 
