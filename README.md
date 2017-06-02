# Compiler from BNGL to Kappa

This tool is aimed at translating rule-based models written in the 
BNGL language into Kappa. The translation is mostly syntactic and no
semantic check of the original BNGL file is made.

Here are a few subtelties about this translation:

+ In BNGL, a pattern is given by a sequence of _species pattern_ separated
by the `+` operator. Within a species pattern, agents are separated by the `.` operator
and they have to be matched to agents belonging to the same connected component. 
Kappa does not make this distinction between `+` and `.` and so it may happen
that a BNGL rule is not expressible in Kappa.

    * When the agents of a species pattern are explicitly connected by bonds, 
    the translation is easy: 
    `K(d!1).S(d!1) -> K(d)+S(d)` becomes `K(d!1), S(d!1) -> K(d!.), S(d!.)`.
    * Otherwise, if the rule's LHS only features one species pattern with
    only two components that are connected by bonds, the dual rate notation is used:
    `C(k!1).K(c!1).S(x~u) -> C(k!1).K(c!1).S(x~p) gamma` becomes 
    `C(k!1),K(c!1),S(x~u) -> C(k!1),K(c!1), S(x~p) @ 0 {gamma}`.
    * Otherwise, an error is issued.


+ BNGL allows using the `dot` operator in defining observables. This feature
is not available in Kappa yet but it should not be too hard to implement.


## Usage

The following command:

```
bngl-to-kappa [INPUT_FILE]
```

translates the content of `INPUT_FILE` into Kappa and prints it
on the standard output. If no input file is provided, the input is
taken on stdin.