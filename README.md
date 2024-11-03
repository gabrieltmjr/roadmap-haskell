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
