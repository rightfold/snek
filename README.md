# SNEK

SNEK is a programming language, featuring purity, currying, rank-N polymorphism,
higher-kinded types, and strict evaluation.

A PHP code generator is included.

## Examples

    ; Comments start with a semicolon and extend until the end of the line.

    ; The identity function. The fn special form denotes a lambda function. The
    ; first argument to fn is the parameter list. When the parameter list is a
    ; list (delimited by parentheses), the lambda function takes values as
    ; arguments. When the parameter list is an array (delimited by brackets),
    ; the lambda function takes types as arguments, giving rise to polymorphism.
    (fn [t *] (fn (x t) x))

    ; Applying the identity function to a Boolean value. Note how arrays are
    ; used to pass types as arguments, and lists are used to pass values as
    ; arguments.
    ([(fn [t *] (fn (x t) x)) bool] true)
