#lang wort

let compose = bind f (bind g ({g call f call})) (
let partial = bind f (bind x ({x f call})) (
let add1 = 1 add (
let add1p = 1 { add } partial call (

{ 1 2 3 } { add add add1 } compose call
bind x (
    x add1
    x add1p
    eq
)

))))