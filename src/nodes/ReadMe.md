# Nodes
This module contains:
- The node system
- The solver for node graphs

## Outline
Here's a general outline of the node system.

### Nodes
`Node`s are components of a `Group`. They are composed of `Port`s which act as `Input`s and `Output`s.

### Ports
`Port`s are components of a `Node`. They allow connecting `Node`s together. An `Input Port` can only be connected to one `Output Port`.
An `Output Port` can be connected to any number of `Input Port`s.

### Groups
`Group`s are `Node`s that contain other `Node`s. Being a `Node` itself means it has `Port`s. However these `Port`s are `Passthrough`s.

### Passthrough Ports
`Passthrough Port`s are special types of `Port`s that can act either like an `Input` or an `Output` depending on the context.
A `Passthrough Port` can either be `Inner` or `Outter` based on whether it is being accessed from inside or outside of a `Group` respectively.
They can also obviously either be a `Group`'s `Input` or `Output`.

### Paralelism
Since the goal of this library is to provide a high level GUI for realtime processing, dispatching the worload on multiple threads is key.
This is achieved by splitting arrays across multiple instances of a group.
Each group can then be run in separate threads.
This means that `Node`s and `Port`s need to be clonable.

### Serialization
In order to save and load a node graph, `Node`s need to be serializable.