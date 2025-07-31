
# Currying

we need to distinguish between partial and total
application, aka are all the required arguments present. We
can't do that currently, since a function that returns a
function is a valid type, but a partially applied function of
two arguments is also typed as a curried function.

See:
https://www.cambridge.org/core/journals/journal-of-functional-programming/article/making-a-fast-curry-pushenter-vs-evalapply-for-higherorder-languages/02447DB613E94DC35ACDCB24DB39F085

I don't think we want to support partial application at the
moment, since it requires complexity in the runtime I don't
want to deal with currently. The issue is that there isn't really
a good place to insert the check that each application is total
and not partial. This is mostly cause things like applying a
let block that returns a function to an arg is valid, so we have
the function type but not the number of arguments at the
application site.

TODO we could adjust the types to be more expressive to distinguish
int -> int -> int and int -> (int -> int)
or we could find some place where we can check this property
and fail on partial applications. I'd like to do the second if
possible since in the longer term partial application is
something we will want, and it seems annoying to change it
just to change it back later.

At the moment we are going to use something similar to the push
model.
