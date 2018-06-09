---
title: toy
author: Ramana Nagasamudram
header-includes:
    - \usepackage{syntax}
    - \setlength{\grammarparsep}{20pt plus 1pt minus 1pt}
    - \setlength{\grammarindent}{12em}
---

# Example Code

Operations on parts. `:=` is used for assignment. Introducing assignment in the language
is still up for debate. `::` is used to place an object at a certain location. The other
key operators are `|>`, `|>>`, `|@`, `|@.` and `|@@`. All rotations are counter clockwise.
Comments are denoted using `;;` as in \textsc{Lisp}.

~~~~~~~~~~~~~~~
x := unitBox     ;; x is at position 0 0 0
x |> (1 1 1)     ;; move x to position 1 1 1
x |@ 90          ;; rotate x by 90 degrees
x |@. 1          ;; rotate x by 1 radians
x |>> (x y z)    ;; relative move
x |@@ alpha      ;; relative rotate
x -> (1 1 1 90)  ;; place x at abs location-angle
~~~~~~~~~~~~~~~~~~~~

Having the ability to sequence (or compose?) any number of operations is useful. Here is
an example shown in two different styles of formatting.

~~~~~~~~~~~~~~~
x |> (1 1 1) |# 90 |## 35 |>> ( 2 -1 10 )

x |> ( 1 1 1 )
  |# 90
  |## 35
  |>> ( 2 -1 10 ) 
~~~~~~~~~~~~~~~~~

## Constructs

Examples of other necessary language constructs

~~~~~~~~~~~~~~~
box := unitBox

if (volume box > 2)
then box |>> ( 2 2 2 )
else box |@. 90

let y = unitBox + unitCyl
in  box - y

;; Lambda -- almost like in Haskell
getVolume = \s . volume box

func moveCubeToOrigin x {
    if type x == "Cube"
    then x -> (0 0 0 0)
    else nothing
}

func moveToPoint x y {
    if type x == "Cylinder"
    then x -> y
    else nothing
}

;; Curried
map (moveToPoint unitBox) [(1,1,1),(2,2,2),(3,3,3)]

~~~~~~~~~~~~~~~~~~~~

# Grammar

TODO!

\begin{grammar}
<statement> ::= <ident> `=' <expr>
\alt `for' <ident> `=' <expr> `to' <expr> `do' <statement>
\alt `{' <stat-list> `}'
\alt <empty>

<stat-list> ::= <statement> `;' <stat-list> | <statement>
\end{grammar}

\begin{grammar}

\end{grammar}

