## Search Algorithm 1

`O(n^2)`
Visit all nodes with shortest path

1. Create a set for visited nodes
2. Set current node to start node
3. From the current node compare all outgoing nodes
4. Select the outgoing node with lowest weight that is not marked as visited
5. If a node was found:
    1. Mark current node as visited
    2. Push next node to stack
    3. Set next node to current node
6. Otherwise:
    1. Return with current stack


## Search Algorithm 2 (Depth First Search)

`O(n^2)`
Visit all nodes with longest path

1. Create a set for visited nodes
2. Begin search at start node
3. Mark current node as visited
4. Iterate through all outgoing nodes that are not marked as visited
5. Add edge weight to local length
6. Recursively repeat steps 3 to 7 with outgoing nodes until all were visited
7. When encountering a visited node compare max distance with local distance
    1. Replace max distance with local distance if max distance is smaller
9. Reset local length for next node
