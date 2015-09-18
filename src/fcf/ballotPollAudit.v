Extraction Language Haskell.

Require Import ln.
Require Import Qround.
Require Import List.
Require Import Sorting.Mergesort.
Require Import Orders.
Require Import Basics.

Import ListNotations.
Import MonadNotation.
Local Open Scope monad.


Definition N_to_Q n :=
Z.of_N n # 1.

Definition findASN' ballots vw vl risk :=
if Qge_bool vw vl then
 let sw := vw / (vw + vl) in
 zw <- ln ((2 # 1) * sw)  ;;
 zl <- ln ((2 # 1) * (1-sw)) ;;
 let pw := vw/ballots in
 let pl := vl/ballots in
 lr <- (ln (1/risk)) ;;
 ret (sw, zw, zl, pw, pl, lr, Qceiling ((lr + (zw/(2 # 1)))/(((vw + vl)/ballots) * (pw * zw + pl * zl))))
else
 None.


Definition findASN ballots vw vl risk := 
res <- (findASN' ballots (N_to_Q vw) (N_to_Q vl) risk) ;;
ret (snd res).


Definition ble_nat a b :=
match nat_compare a b with
| Lt => true
| Eq => true
| _ => false
end.

Definition votes_contest (contest : list N) : N :=
(fold_right N.add 0%N contest) .

Fixpoint contests_ok (contests : list (list N)) (no_winners : list nat) (ballots : N) : bool :=
match contests, no_winners with
| contest :: contests', no_winner :: no_winners' => 
  if andb (ble_nat no_winner (length contest))
          (N.leb (votes_contest contest) (N.of_nat no_winner * ballots)%N) 
            then
              contests_ok contests' no_winners' ballots
            else
              false
| nil, nil => true
| _, _ => false
end.


(*actually defined as geb for descending sort*)
Module NOrderBool <: TotalLeBool.
  Definition t := N.
  Definition leb x y :=
    (N.leb x y).
  Theorem leb_total : forall a1 a2, leb a1 a2 = true \/ leb a2 a1 = true.
  Proof. 
Admitted.
(*    intros.
    unfold leb.
    destruct (a1 <? a2)%N eqn:?, (a2 <? a1)%N eqn:?; auto.
    rewrite N.ltb_lt in *.
    eapply N.lt_trans in Heqb0; eauto.
    destruct N.lt_strorder as [X Y].
    apply X in Heqb0. auto.
Qed.
 *)   
End NOrderBool.

Module Import NSort := Sort NOrderBool.

Locate compose.

Fixpoint findMargin' (contest : list N) (index : nat) :=
match index with 
| O => match contest with 
       | lastwin :: firstlose :: _ => (lastwin - firstlose)%N
       | lastwin :: _ => lastwin
       | _ => 0%N
       end
| S n' => findMargin' contest n'
end.

Definition findMargin (contest_no_winners : (list N * nat)) :=
let (contest, no_winners) := contest_no_winners in
findMargin' contest (NPeano.Nat.pred no_winners) .


Fixpoint split_after_index'' {A} (l : list A) n :=
match n with
| O => (nil, l)
| S n' => match l with
          | h :: t => let res := split_after_index'' t n' in
                      (h :: fst res, snd res)
          | nil => (nil, nil)
          end
end.

(*bad tail recursive way*)
Fixpoint split_after_index' {A} (l : list A) n ls :=
match n with
| O => (fst ls, l)
| S n' => match l with 
          | h :: t => split_after_index' t n' ((fst ls) ++ [h], nil)
          | nil => ls
          end
end.
 
Definition split_after_index {A} (l : list A) n :=
split_after_index'' l n.

Definition split_after_index_pair {A} (i : list A * nat) :=
let (l, n) := i in
split_after_index l n.

Definition findASN_pair ballots risk (v : N * N) :=
let (wv,lv) := v in
findASN ballots wv lv risk.

(*cani is number of winners or variable no_winners*)
(*contests is a list of some number of candidates*)


Definition maxASN_contest ballots risk (wl : list N * list N) :=
let (winners, losers) := wl in
fold_right 
(fun (x y : option Z) => r <- x ;;
                         r2 <- y;;
ret (Z.max r r2)) (Some 0%Z) (map (findASN_pair ballots risk) (list_prod winners losers)).

Definition maxASN_all_contests all_winners all_losers ballots risk :=
fold_right (fun (x y : option Z) => r <- x ;;
                         r2 <- y;;
ret (Z.max r r2))
(Some 0%Z)
(map (maxASN_contest ballots risk) (combine all_winners all_losers)).

Check maxASN_all_contests.

Local Open Scope Q.

Definition waldFactor (winvotes losevotes : positive) (winAudited loseAudited : Z) :=
let waldFactor := Zpos (winvotes) # (winvotes + losevotes) in
Qred (Qinv ((Qpower ((2 # 1) * waldFactor) winAudited) * 
      (Qpower ((2 # 1) * (1 - (waldFactor))) loseAudited))).

Definition findMinMargin (ballots : positive) (contests : list (list N)) 
           (risk : Q) (no_winners : list nat)  :=
  if negb (contests_ok contests no_winners (Npos ballots))
  then
    (nil, 0%N, 0, None)
  else
    let sorted_contests := map (compose (@rev N) sort) contests in
    let margins := map findMargin (combine sorted_contests no_winners) in
    let min_margin := fold_right N.min (Npos ballots) margins in
    let diluted_margin := Z.of_N min_margin # ballots in
    let (winners, losers) := split (map split_after_index_pair (combine sorted_contests no_winners)) in
    (sorted_contests, min_margin, diluted_margin, (maxASN_all_contests winners losers ((Zpos ballots) # 1) risk)).  

Extract Inductive list => "([])" [ "[]" "(:)" ].

Extraction "BallotPollAudit.hs" findMinMargin.

Definition contests := ([[5050; 4050]; [3000; 7000]]%N).
Definition no_winners :=  [1%nat; 1%nat].
Definition ballots := (10000%positive).
Compute negb (contests_ok contests  no_winners (Npos ballots)).
Compute findMinMargin ballots contests (1 # 10) no_winners.

Definition Qpower_N q n :=
match n with
| N0 => 1
| Npos p => Qpower_positive q p
end.


Definition auditRisk (votesWinner : N) (votesLoser : N) (winnerShare : Q) : Q :=
(Qinv ((Qpower_N ((2 # 1) * winnerShare) votesWinner) * (Qpower_N ((2 # 1) * (1 - winnerShare)) votesLoser))).

Compute (auditRisk 900 90 (7 # 10)).

Definition Qlt_bool a b:= negb (Qge_bool a b).

Definition auditDone (votesWinner : N) (votesLoser : N) (winnerShare : Q) (risk : Q) : bool :=
Qlt_bool (auditRisk votesWinner votesLoser winnerShare) risk.

