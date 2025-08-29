# rbtrack-node-api
API for rbtrack plugins.

## Global concept

### Registrar
The registrar keeps track of all the different components.

Components:
- Toolset
    - std nodes
    - user-defined nodes
    - groups
- Entry point (group)

The toolset is essentially a map of type `<Uuid, Node>`.

The entry point is a group itself.

### Item
Items are any component that can be contained in a graph. It has metadata to describe the component:
- Name
- Description

### Node
A node is an item that refers to a processing operation.

Components:
- Inputs
- Outputs
- Parent (group)

### Port
Ports are items and they are the inputs and outputs of nodes.

Components:
- For outputs:
    - connections (inputs)
- For inputs:
    - at most one output

### Group
A group is a node that contains other nodes.

Components:
- Children (nodes)
- Node map (of type `<Uuid, Node>`)


## IR example

```
Toolset:
    IntValueNode
    IntSumNode
    IntSubNode
    TripleIntSumGroup
    MainGroup

IntValueNode:
    Inputs:
    Outputs:
        value: int

IntSumNode:
    Inputs:
        a: int
        b: int
    Outputs:
        res: int

IntSubNode:
    Inputs:
        a: int
        b: int
    Outputs:
        res: int

TripleIntSumGroup:
    Inputs:
        a: int
        b: int
        c: int
    Outputs:
        res: int

TripleSumGroupBody:
    sum1: IntSumNode
    sum2: IntSumNode

    self.a -> sum1.a
    self.b -> sum1.b
    
    sum1.res -> sum2.a
    self.c -> sum2.b

    sum2.res -> self.res

MainGroupBody:
    sum1: IntSumNode
    triple_sum1: TripleIntSumGroup
    triple_sum2: TripleIntSumGroup
    sub1: IntSubNode
    v1: IntValueNode
    v2: IntValueNode
    v3: IntValueNode
    v4: IntValueNode

    v1.value -> sum1.a
    v2.value -> sum1.b

    v1.value -> sub1.a
    v2.value -> sub1.b

    sum1.res -> triple_sum1.a
    v3.value -> triple_sum1.b
    v4.value -> triple_sum1.c

    sub1.res -> triple_sum2.a
    v3.value -> triple_sum2.b
    v4.value -> triple_sum2.c
```