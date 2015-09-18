Require Import Rat.

Local Open Scope rat.

Definition x := (1/2) * (3/4).
Print StdNat.posnat.

Definition rat_noproof r :=
match r with
RatIntro a b => (a, StdNat.posnatToNat b)
end.

