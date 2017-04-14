#lang wort

let swap = bind x (bind y (x y)) (
let dup = bind x (x x) (
let zap = bind x () (
let bury = bind x (bind y (bind z (x z y))) (

1 2 3 bury swap zap dup add

))))