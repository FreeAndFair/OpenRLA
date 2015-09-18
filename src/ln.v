Require Export QArith_base.
Require Import BinNat.
Require Export Qround.
Require Export ExtLib.Structures.Monad.
Require Export ExtLib.Data.Monads.OptionMonad.
Require Export Qreduction.
Require Export BinPos.

Import Pos.

Local Open Scope Q.
Local Open Scope positive.

Fixpoint z_lg2' (p:Q) (n : Z) fuel :=
match fuel with 
| O => None
| S fuel' => 
  if (Qfloor p =? 0)%Z
  then
    match  (Qpower (2 # 1) n ?= p)%Q with
    | Lt => Some (n)%Z
    | Eq => Some (n+1)%Z
    | _ => z_lg2' p (n-1) fuel'
    end
  else
    match  (2 ^ n ?= (Qfloor p))%Z with
    | Gt => Some (n-1)%Z
    | Eq => Some n
    | _ => z_lg2' p (n+1) fuel'
    end
end.

Definition z_lg2 q := 
z_lg2' q 0 100.

Compute z_lg2 (1 # 10). 

Local Close Scope positive.



Definition powN (p: positive) n :=
match n with
| N0 => 1%positive
| Npos np => (p ^ np)%positive
end.
Check Qpower.

Definition y_0' (n : Z) x :=
(Qpower (2 # 1) (- n)) * (x). 



Definition Q_to_N_floor (q : Q) : option N :=
match Qfloor q with
| Zpos p => Some (Npos p)
| Z0 => Some N0
| _ => None
end.

Import MonadNotation.
Local Open Scope monad.

Definition y_0 (x : Q) :=
l2 <- z_lg2 x ;;
(*px <- (Q_to_N_floor x) ;;
l2 <- (z_lg2 (Z.of_N px)) ;; *)
(ret (l2, (y_0' l2 x))).

Compute (y_0 (1#10)).

Definition Qge_bool x y := 
(Z.geb (Qnum x * QDen y) (Qnum y * QDen x))%Z.

Lemma Qge_bool_iff : forall x y, Qge_bool x y = true <-> x >= y. 
Proof.
unfold Qge_bool.  unfold Qle. intros. 
symmetry. rewrite  Zle_is_le_bool.
rewrite Z.geb_leb. reflexivity.
Qed.



Fixpoint card_P p : positive :=
match p with
| xI  p' | xO p' => (card_P p' + 1)%positive
| _ => 1%positive
end.


Definition truncate_P_by : positive -> positive -> positive  :=
peano_rec (fun _ : positive => positive -> positive) (fun d => d)
(fun _ r d => match d with 
               | xH => xH
               | xI d' | xO d' => r d'
                                    end).

Definition truncate_P_to (p d : positive) : positive :=
let toremove := ((card_P p) - d)%positive in
truncate_P_by toremove p.

Compute truncate_P_to 8 3.

Definition truncate_Q_by (q : Q) (toRemove : positive) : Q :=
  Qmake (match Qnum q with
           | Zpos p => Zpos (truncate_P_by toRemove p)
           | Zneg p => Zneg (truncate_P_by toRemove p)
           | Z0 => Z0
         end) (truncate_P_by toRemove (Qden q)).


Definition divide_Q (q:Q) (plength : positive) := 
let numpos :=  match Qnum q with
               | Zpos nump | Zneg nump => nump 
               | _ => 1%positive 
               end in
let numlength := card_P numpos  in
let denlength := card_P (Qden q) in
let toRemove := ((max numlength denlength) - plength)%positive in
truncate_Q_by q toRemove.

Fixpoint m' (y : Q) (mr : Z) fuel :=
match fuel with
| S fuel' => if Qeq_bool y 1 then 
    (0%Z, y)
  else 
    if (Qge_bool y (2#1)) then
      (mr, y)
    else m' (divide_Q (Qred (y ^ 2)) 64) (mr + 1) fuel'
| _ => (mr, y)
end.  

Fixpoint m y := 
m' y 0%Z 100.


Fixpoint populate_ms (y : Q) (n : nat) {struct n} :=
match n with 
| O => nil
| S n' => let (m_n, y') := m y in
          cons (m_n(*, y*)) (populate_ms ((Qred(y'/ (2#1)))) n')
end.

Compute populate_ms (3 # 2) 10.



Fixpoint log2' (n : nat) (ms : list Z) (msum : Z)  :=
match n with
| S n' => match ms with
         | cons m ms' => match m, msum with
                         | Zpos p, Zpos msumpos => 
                           let msum' := (p + msumpos)%positive in
                           (Qred (1 # (2 ^ msum'))) + (Qred (log2' n' ms' (Zpos msum')))
                         | Zpos p, Z0 => 
                           let msum' := (p)%positive in
                           (Qred (1 # (2 ^ msum'))) + (Qred (log2' n' ms' (Zpos msum')))
                                                           
                         | _, _ => 0
                         end
         | nil => 0
          end
| _ => 0
end.

Definition log2 (q : Q) (c : nat) :=
y_0r <- y_0 q ;;
(let (n, y0) := (y_0r : Z * Q) in
let ms := populate_ms y0 c in  
  ret (Qred (log2' c ms 0) + (n # 1))).


Definition lg2e :=
100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 #
144269504088896340735992468100189213742664595415298593413544940693110921918118507988552662289350634449699751830965254425.


Definition lnc q c digs2:= 
r <- (log2 (Qred q) c) ;;
ret (divide_Q (Qred(lg2e * r)) digs2).

Compute (r <- (lnc (3 # 1) 20 50) ;; 
         ret (r)).

Definition ln q := lnc q 20 64.






    