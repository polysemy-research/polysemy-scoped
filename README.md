# `polysemy-scoped`

An experimental effect that allows for execution of a "locally scoped" effect, that cannot be interpreted without using `runScoped`.

This is useful when an interpretation of the "inner" effect depends on some resource.

For example, this is a way to seamlessly have database transactions in your business logic,
without having to add a "token" parameter to all the actions in the effect you use for interaction with your database.

**Note** that the interpreter for the internal effect will be run **multiple times** - once for each internal action.

Credits to @KingoftheHomeless for the implementation.
