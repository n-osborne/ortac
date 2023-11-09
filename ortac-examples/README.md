# `ortac` examples

This package centralises examples of libraries with their Gospel
specifications. `ortac` is then used to generate code with runtime assertion
checking. This is thought both as a learning ressource and a way to experiment
with the limitations of the `ortac` tool which is still quite young.

Specifications are written in a style that allows the use the `ortac` tool,
specifically the `qcheck-stm` plugin.

## `lwt_dllist` library

Specifications for this library can be found in the edited copy of the
interface `lwt_dllist_spec.mli`. There is one modification of the actual
interface: the type `'a node` is not abstract anymore.

The STM `postcond` function pattern match on the returned value encapsulated in
a `STM.ty`. As functions `add_l` and `add_r` both return the node that has been
added, we need to extend the `STM.ty` type with an encoding for
`Lwt_dllist.node`. We add this extension by editing the generated file. For the
typechecker to accept this extension, the implementation of the `node` type
need to be public.

