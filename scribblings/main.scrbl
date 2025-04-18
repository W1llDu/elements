#lang scribble/manual 

@(require (for-label racket elements))

@title{elements: NOT a small Parsing Expression Grammars implementation}

@defmodule[elements]


@defform[(peg-parse peg expr)]{

Parses the string produced by @racket[expr] using the given PEG expression. Returns the result of a semantic action, or the matched string for a successful parse without a semantic action, or @racket[#f] indicating a parse failure.

}