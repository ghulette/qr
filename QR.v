From Coq Require Import Reals.
From mathcomp Require Import matrix all_ssreflect ssralg ssrnum bigop.

Open Scope ring_scope.
Delimit Scope R_scope with R.

Definition Givens (n : nat) (i j : 'I_n.+1) (c s : R) : 'M_n.+1 :=
  \matrix_(p < n.+1, q < n.+1)
    if (p == i) && (q == i) then c
    else if (p == j) && (q == j) then c
    else if (p == i) && (q == j) then s
    else if (p == j) && (q == i) then (-s)%R
    else if (p == q) then 1%R
    else 0%R.
