# PGM

A PGM/bayes net library designed with a particular goal in mind:

If `x` and `y` are random variables, then `z = x + y` should be all that's needed to
compute a new random variable that is distributed as their sum, **even taking into account
their joint distribution**. No special syntax, new operators, or do-blocks should be necessary.

This library accomplishes this task with probabilistic graphical models for all operations
in `Num` over the `Rational`s (and thus any composition of those functions), serving as a
proof of concept for a more general extension. An example is in `Main`.
