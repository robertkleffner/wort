#lang brag

wrt-program : wrt-expr

wrt-expr : wrt-word*
wrt-word : INT
         | wrt-prim
         | VAR
         | wrt-block
         | wrt-bind
         | wrt-let

wrt-prim : "add" | "call" | "fix" | "if"
         | "true" | "false" | "eq" | "less"
wrt-block : "{" wrt-expr "}"
wrt-bind : "bind" VAR "(" wrt-expr ")"
wrt-let : "let" VAR "=" wrt-expr "(" wrt-expr ")"