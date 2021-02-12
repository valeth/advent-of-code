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
