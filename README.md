# PFL - Haskell Coursework

## Group T13G11
- Gabriel Tomaz Machado Júnior (202008860) - 50% contribution
- Manuel Rivera Villatte (202401168) - 50% contribution


## Functions

### 1. cities :: RoadMap -> [City] (Manuel)

_Returns all the cities in the graph._

### 2. areAdjacent :: RoadMap -> City -> City -> Bool (Gabriel)

_Returns a boolean indicating whether two cities are linked directly._

### 3. distance :: RoadMap -> City -> City -> Maybe Distance (Manuel)

_Returns a Just value with the distance between two cities connected directly, given two city names, and Nothing otherwise._

### 4. adjacent :: RoadMap -> City -> [(City,Distance)] (Gabriel)

_Returns the cities adjacent to a particular city (i.e. cities with a direct edge between them) and the respective distances to them._

### 5. pathDistance :: RoadMap -> Path -> Maybe Distance (Manuel)

_Returns the sum of all individual distances in a path between two cities in a Just value, if all the consecutive pairs of cities are directly connected by roads. Otherwise, it returns a Nothing._

### 6. rome :: RoadMap -> [City] (Gabriel)

_Returns the names of the cities with the highest number of roads connecting to them (i.e. the vertices with the highest degree)._

### 7. isStronglyConnected :: RoadMap -> Bool (Manuel)

_Returns a boolean indicating whether all the cities in the graph are connected in the roadmap (i.e., if every city is reachable from every other city)._

### 8. shortestPath :: RoadMap -> City -> City -> [Path] (Gabriel)

_Computes all shortest paths connecting the two cities given as input. Note that there may be more than one path with the same total distance. If there are no paths between the input cities, then return an empty list. Note that the (only) shortest path between a city c and itself is [c]._

### 9. travelSales :: RoadMap -> Path, given a roadmap (Manuel)

_Returns a solution of the Traveling Salesman Problem (TSP). In this problem, a traveling salesperson has to visit each city exactly once and come back to the starting town. The problem is to find the shortest route, that is, the route whose total distance is minimum. This problem has a known solution using dynamic programming. Any optimal TSP path will be accepted and the function only needs to return one of them, so the starting city (which is also the ending city) is left to be chosen by each group. Note that the roadmap might not be a complete graph (i.e. a graph where all vertices are connected to all other vertices). If the graph does not have a TSP path, then return an empty list._
