wort
====

**wort** is a core concatenative programming language with 'full' type inference, in the sense of Algorithm W (principal types at the expression level). It is not intended for general purpose use, but is surprisingly expressive for as minimal as it is. This particular variant of wort is implemented in Racket's `#lang` facility, so you'll need to have Racket installed first. At the top of your wort file, type `#lang wort` to make sure racket knows what you want (after you've installed the package). The syntax for wort is pretty simple:

```
<<e>> := <<w>>*
<<w>> := 0 | 1 | 2 ...
       | true | false
       | <<p>>
       | <<x>>
       | { <<e>> }
       | bind <<x>> ( <<e>> )
       | let <<x>> = <<e>> ( <<e>> )
<<p>> := add | call | fix | if | eq | less
<<x>> := variables beginning with letter, followed by zero or more letters or numbers
```

Error reporting for type inference is currently very minimal, to keep the implementation small and focused.