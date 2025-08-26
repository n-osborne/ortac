# Experimentations on re-introducing string testing for SUT-returning functions

The general idea is to add a flag to the `cmd` type so that `run` and
`next_state` can adapt their behaviour depending whether the context is the
sequential prefix or one of the two spawned domain.

TODO:

- [X] change the type cmd definition to contain a flag
- [X] provide `arb_cmd_flag`, a parameterized `arb_cmd`
- [X] make run and next_state adapt their behaviour to the flag inside the cmd
- [ ] provide a `agree_test_flag` that takes `arb_cmd_flag` as argument
- [ ] make generated code uses `agree_test_flag`
