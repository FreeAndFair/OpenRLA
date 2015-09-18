{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{- For Hugs, use the option -F"cpp -P -traditional" -}

module BallotPollAudit where

import qualified Prelude


unsafeCoerce :: a -> b
#ifdef __GLASGOW_HASKELL__
import qualified GHC.Base
unsafeCoerce = GHC.Base.unsafeCoerce#
#else
-- HUGS
import qualified IOExts
unsafeCoerce = IOExts.unsafeCoerce
#endif

__ :: any
__ = Prelude.error "Logical or arity value used"

and_rect :: (() -> () -> a1) -> a1
and_rect f =
  f __ __

and_rec :: (() -> () -> a1) -> a1
and_rec f =
  and_rect f

eq_rect :: a1 -> a2 -> a1 -> a2
eq_rect x f y =
  f

eq_rec :: a1 -> a2 -> a1 -> a2
eq_rec x f y =
  eq_rect x f y

eq_rec_r :: a1 -> a2 -> a1 -> a2
eq_rec_r x h y =
  eq_rec x h y

data Bool =
   True
 | False

andb :: Bool -> Bool -> Bool
andb b1 b2 =
  case b1 of {
   True -> b2;
   False -> False}

orb :: Bool -> Bool -> Bool
orb b1 b2 =
  case b1 of {
   True -> True;
   False -> b2}

xorb :: Bool -> Bool -> Bool
xorb b1 b2 =
  case b1 of {
   True ->
    case b2 of {
     True -> False;
     False -> True};
   False -> b2}

negb :: Bool -> Bool
negb b =
  case b of {
   True -> False;
   False -> True}

data Nat =
   O
 | S Nat

nat_rect :: a1 -> (Nat -> a1 -> a1) -> Nat -> a1
nat_rect f f0 n =
  case n of {
   O -> f;
   S n0 -> f0 n0 (nat_rect f f0 n0)}

nat_rec :: a1 -> (Nat -> a1 -> a1) -> Nat -> a1
nat_rec =
  nat_rect

data Option a =
   Some a
 | None

data Prod a b =
   Pair a b

fst :: (Prod a1 a2) -> a1
fst p =
  case p of {
   Pair x y -> x}

snd :: (Prod a1 a2) -> a2
snd p =
  case p of {
   Pair x y -> y}

length :: (([]) a1) -> Nat
length l =
  case l of {
   [] -> O;
   (:) y l' -> S (length l')}

app :: (([]) a1) -> (([]) a1) -> ([]) a1
app l m0 =
  case l of {
   [] -> m0;
   (:) a l1 -> (:) a (app l1 m0)}

data Comparison =
   Eq
 | Lt
 | Gt

compOpp :: Comparison -> Comparison
compOpp r =
  case r of {
   Eq -> Eq;
   Lt -> Gt;
   Gt -> Lt}

data CompareSpecT =
   CompEqT
 | CompLtT
 | CompGtT

compareSpec2Type :: Comparison -> CompareSpecT
compareSpec2Type c =
  case c of {
   Eq -> CompEqT;
   Lt -> CompLtT;
   Gt -> CompGtT}

type CompSpecT a = CompareSpecT

compSpec2Type :: a1 -> a1 -> Comparison -> CompSpecT a1
compSpec2Type x y c =
  compareSpec2Type c

type Sig a =
  a
  -- singleton inductive, whose constructor was exist
  
data Sumbool =
   Left
 | Right

sumbool_rect :: (() -> a1) -> (() -> a1) -> Sumbool -> a1
sumbool_rect f f0 s =
  case s of {
   Left -> f __;
   Right -> f0 __}

sumbool_rec :: (() -> a1) -> (() -> a1) -> Sumbool -> a1
sumbool_rec =
  sumbool_rect

data Sumor a =
   Inleft a
 | Inright

pred :: Nat -> Nat
pred n =
  case n of {
   O -> n;
   S u -> u}

plus :: Nat -> Nat -> Nat
plus n m0 =
  case n of {
   O -> m0;
   S p -> S (plus p m0)}

mult :: Nat -> Nat -> Nat
mult n m0 =
  case n of {
   O -> O;
   S p -> plus m0 (mult p m0)}

minus :: Nat -> Nat -> Nat
minus n m0 =
  case n of {
   O -> n;
   S k ->
    case m0 of {
     O -> n;
     S l -> minus k l}}

max :: Nat -> Nat -> Nat
max n m0 =
  case n of {
   O -> m0;
   S n' ->
    case m0 of {
     O -> n;
     S m'0 -> S (max n' m'0)}}

min :: Nat -> Nat -> Nat
min n m0 =
  case n of {
   O -> O;
   S n' ->
    case m0 of {
     O -> O;
     S m'0 -> S (min n' m'0)}}

nat_iter :: Nat -> (a1 -> a1) -> a1 -> a1
nat_iter n f x =
  case n of {
   O -> x;
   S n' -> f (nat_iter n' f x)}

data Positive =
   XI Positive
 | XO Positive
 | XH

positive_rect :: (Positive -> a1 -> a1) -> (Positive -> a1 -> a1) -> a1 ->
                 Positive -> a1
positive_rect f f0 f1 p =
  case p of {
   XI p0 -> f p0 (positive_rect f f0 f1 p0);
   XO p0 -> f0 p0 (positive_rect f f0 f1 p0);
   XH -> f1}

positive_rec :: (Positive -> a1 -> a1) -> (Positive -> a1 -> a1) -> a1 ->
                Positive -> a1
positive_rec =
  positive_rect

data N =
   N0
 | Npos Positive

n_rect :: a1 -> (Positive -> a1) -> N -> a1
n_rect f f0 n =
  case n of {
   N0 -> f;
   Npos x -> f0 x}

n_rec :: a1 -> (Positive -> a1) -> N -> a1
n_rec =
  n_rect

data Z =
   Z0
 | Zpos Positive
 | Zneg Positive

z_rect :: a1 -> (Positive -> a1) -> (Positive -> a1) -> Z -> a1
z_rect f f0 f1 z =
  case z of {
   Z0 -> f;
   Zpos x -> f0 x;
   Zneg x -> f1 x}

z_rec :: a1 -> (Positive -> a1) -> (Positive -> a1) -> Z -> a1
z_rec =
  z_rect

compose :: (a2 -> a3) -> (a1 -> a2) -> a1 -> a3
compose g f x =
  g (f x)

data Reflect =
   ReflectT
 | ReflectF

iff_reflect :: Bool -> Reflect
iff_reflect b =
  case b of {
   True -> and_rec (\_ _ -> ReflectT);
   False -> and_rec (\_ _ -> ReflectF)}

type T = Positive

succ :: Positive -> Positive
succ x =
  case x of {
   XI p -> XO (succ p);
   XO p -> XI p;
   XH -> XO XH}

add :: Positive -> Positive -> Positive
add x y =
  case x of {
   XI p ->
    case y of {
     XI q -> XO (add_carry p q);
     XO q -> XI (add p q);
     XH -> XO (succ p)};
   XO p ->
    case y of {
     XI q -> XI (add p q);
     XO q -> XO (add p q);
     XH -> XI p};
   XH ->
    case y of {
     XI q -> XO (succ q);
     XO q -> XI q;
     XH -> XO XH}}

add_carry :: Positive -> Positive -> Positive
add_carry x y =
  case x of {
   XI p ->
    case y of {
     XI q -> XI (add_carry p q);
     XO q -> XO (add_carry p q);
     XH -> XI (succ p)};
   XO p ->
    case y of {
     XI q -> XO (add_carry p q);
     XO q -> XI (add p q);
     XH -> XO (succ p)};
   XH ->
    case y of {
     XI q -> XI (succ q);
     XO q -> XO (succ q);
     XH -> XI XH}}

pred_double :: Positive -> Positive
pred_double x =
  case x of {
   XI p -> XI (XO p);
   XO p -> XI (pred_double p);
   XH -> XH}

pred0 :: Positive -> Positive
pred0 x =
  case x of {
   XI p -> XO p;
   XO p -> pred_double p;
   XH -> XH}

pred_N :: Positive -> N
pred_N x =
  case x of {
   XI p -> Npos (XO p);
   XO p -> Npos (pred_double p);
   XH -> N0}

data Mask =
   IsNul
 | IsPos Positive
 | IsNeg

mask_rect :: a1 -> (Positive -> a1) -> a1 -> Mask -> a1
mask_rect f f0 f1 m0 =
  case m0 of {
   IsNul -> f;
   IsPos x -> f0 x;
   IsNeg -> f1}

mask_rec :: a1 -> (Positive -> a1) -> a1 -> Mask -> a1
mask_rec =
  mask_rect

succ_double_mask :: Mask -> Mask
succ_double_mask x =
  case x of {
   IsNul -> IsPos XH;
   IsPos p -> IsPos (XI p);
   IsNeg -> IsNeg}

double_mask :: Mask -> Mask
double_mask x =
  case x of {
   IsPos p -> IsPos (XO p);
   x0 -> x0}

double_pred_mask :: Positive -> Mask
double_pred_mask x =
  case x of {
   XI p -> IsPos (XO (XO p));
   XO p -> IsPos (XO (pred_double p));
   XH -> IsNul}

pred_mask :: Mask -> Mask
pred_mask p =
  case p of {
   IsPos q ->
    case q of {
     XH -> IsNul;
     _ -> IsPos (pred0 q)};
   _ -> IsNeg}

sub_mask :: Positive -> Positive -> Mask
sub_mask x y =
  case x of {
   XI p ->
    case y of {
     XI q -> double_mask (sub_mask p q);
     XO q -> succ_double_mask (sub_mask p q);
     XH -> IsPos (XO p)};
   XO p ->
    case y of {
     XI q -> succ_double_mask (sub_mask_carry p q);
     XO q -> double_mask (sub_mask p q);
     XH -> IsPos (pred_double p)};
   XH ->
    case y of {
     XH -> IsNul;
     _ -> IsNeg}}

sub_mask_carry :: Positive -> Positive -> Mask
sub_mask_carry x y =
  case x of {
   XI p ->
    case y of {
     XI q -> succ_double_mask (sub_mask_carry p q);
     XO q -> double_mask (sub_mask p q);
     XH -> IsPos (pred_double p)};
   XO p ->
    case y of {
     XI q -> double_mask (sub_mask_carry p q);
     XO q -> succ_double_mask (sub_mask_carry p q);
     XH -> double_pred_mask p};
   XH -> IsNeg}

sub :: Positive -> Positive -> Positive
sub x y =
  case sub_mask x y of {
   IsPos z -> z;
   _ -> XH}

mul :: Positive -> Positive -> Positive
mul x y =
  case x of {
   XI p -> add y (XO (mul p y));
   XO p -> XO (mul p y);
   XH -> y}

iter :: Positive -> (a1 -> a1) -> a1 -> a1
iter n f x =
  case n of {
   XI n' -> f (iter n' f (iter n' f x));
   XO n' -> iter n' f (iter n' f x);
   XH -> f x}

pow :: Positive -> Positive -> Positive
pow x y =
  iter y (mul x) XH

square :: Positive -> Positive
square p =
  case p of {
   XI p0 -> XI (XO (add (square p0) p0));
   XO p0 -> XO (XO (square p0));
   XH -> XH}

div2 :: Positive -> Positive
div2 p =
  case p of {
   XI p0 -> p0;
   XO p0 -> p0;
   XH -> XH}

div2_up :: Positive -> Positive
div2_up p =
  case p of {
   XI p0 -> succ p0;
   XO p0 -> p0;
   XH -> XH}

size_nat :: Positive -> Nat
size_nat p =
  case p of {
   XI p0 -> S (size_nat p0);
   XO p0 -> S (size_nat p0);
   XH -> S O}

size :: Positive -> Positive
size p =
  case p of {
   XI p0 -> succ (size p0);
   XO p0 -> succ (size p0);
   XH -> XH}

compare_cont :: Positive -> Positive -> Comparison -> Comparison
compare_cont x y r =
  case x of {
   XI p ->
    case y of {
     XI q -> compare_cont p q r;
     XO q -> compare_cont p q Gt;
     XH -> Gt};
   XO p ->
    case y of {
     XI q -> compare_cont p q Lt;
     XO q -> compare_cont p q r;
     XH -> Gt};
   XH ->
    case y of {
     XH -> r;
     _ -> Lt}}

compare :: Positive -> Positive -> Comparison
compare x y =
  compare_cont x y Eq

min0 :: Positive -> Positive -> Positive
min0 p p' =
  case compare p p' of {
   Gt -> p';
   _ -> p}

max0 :: Positive -> Positive -> Positive
max0 p p' =
  case compare p p' of {
   Gt -> p;
   _ -> p'}

eqb :: Positive -> Positive -> Bool
eqb p q =
  case p of {
   XI p0 ->
    case q of {
     XI q0 -> eqb p0 q0;
     _ -> False};
   XO p0 ->
    case q of {
     XO q0 -> eqb p0 q0;
     _ -> False};
   XH ->
    case q of {
     XH -> True;
     _ -> False}}

leb :: Positive -> Positive -> Bool
leb x y =
  case compare x y of {
   Gt -> False;
   _ -> True}

ltb :: Positive -> Positive -> Bool
ltb x y =
  case compare x y of {
   Lt -> True;
   _ -> False}

sqrtrem_step :: (Positive -> Positive) -> (Positive -> Positive) -> (Prod
                Positive Mask) -> Prod Positive Mask
sqrtrem_step f g p =
  case p of {
   Pair s y ->
    case y of {
     IsPos r ->
      let {s' = XI (XO s)} in
      let {r' = g (f r)} in
      case leb s' r' of {
       True -> Pair (XI s) (sub_mask r' s');
       False -> Pair (XO s) (IsPos r')};
     _ -> Pair (XO s) (sub_mask (g (f XH)) (XO (XO XH)))}}

sqrtrem :: Positive -> Prod Positive Mask
sqrtrem p =
  case p of {
   XI p0 ->
    case p0 of {
     XI p1 -> sqrtrem_step (\x -> XI x) (\x -> XI x) (sqrtrem p1);
     XO p1 -> sqrtrem_step (\x -> XO x) (\x -> XI x) (sqrtrem p1);
     XH -> Pair XH (IsPos (XO XH))};
   XO p0 ->
    case p0 of {
     XI p1 -> sqrtrem_step (\x -> XI x) (\x -> XO x) (sqrtrem p1);
     XO p1 -> sqrtrem_step (\x -> XO x) (\x -> XO x) (sqrtrem p1);
     XH -> Pair XH (IsPos XH)};
   XH -> Pair XH IsNul}

sqrt :: Positive -> Positive
sqrt p =
  fst (sqrtrem p)

gcdn :: Nat -> Positive -> Positive -> Positive
gcdn n a b =
  case n of {
   O -> XH;
   S n0 ->
    case a of {
     XI a' ->
      case b of {
       XI b' ->
        case compare a' b' of {
         Eq -> a;
         Lt -> gcdn n0 (sub b' a') a;
         Gt -> gcdn n0 (sub a' b') b};
       XO b0 -> gcdn n0 a b0;
       XH -> XH};
     XO a0 ->
      case b of {
       XI p -> gcdn n0 a0 b;
       XO b0 -> XO (gcdn n0 a0 b0);
       XH -> XH};
     XH -> XH}}

gcd :: Positive -> Positive -> Positive
gcd a b =
  gcdn (plus (size_nat a) (size_nat b)) a b

ggcdn :: Nat -> Positive -> Positive -> Prod Positive
         (Prod Positive Positive)
ggcdn n a b =
  case n of {
   O -> Pair XH (Pair a b);
   S n0 ->
    case a of {
     XI a' ->
      case b of {
       XI b' ->
        case compare a' b' of {
         Eq -> Pair a (Pair XH XH);
         Lt ->
          case ggcdn n0 (sub b' a') a of {
           Pair g p ->
            case p of {
             Pair ba aa -> Pair g (Pair aa (add aa (XO ba)))}};
         Gt ->
          case ggcdn n0 (sub a' b') b of {
           Pair g p ->
            case p of {
             Pair ab bb -> Pair g (Pair (add bb (XO ab)) bb)}}};
       XO b0 ->
        case ggcdn n0 a b0 of {
         Pair g p ->
          case p of {
           Pair aa bb -> Pair g (Pair aa (XO bb))}};
       XH -> Pair XH (Pair a XH)};
     XO a0 ->
      case b of {
       XI p ->
        case ggcdn n0 a0 b of {
         Pair g p0 ->
          case p0 of {
           Pair aa bb -> Pair g (Pair (XO aa) bb)}};
       XO b0 ->
        case ggcdn n0 a0 b0 of {
         Pair g p -> Pair (XO g) p};
       XH -> Pair XH (Pair a XH)};
     XH -> Pair XH (Pair XH b)}}

ggcd :: Positive -> Positive -> Prod Positive (Prod Positive Positive)
ggcd a b =
  ggcdn (plus (size_nat a) (size_nat b)) a b

nsucc_double :: N -> N
nsucc_double x =
  case x of {
   N0 -> Npos XH;
   Npos p -> Npos (XI p)}

ndouble :: N -> N
ndouble n =
  case n of {
   N0 -> N0;
   Npos p -> Npos (XO p)}

lor :: Positive -> Positive -> Positive
lor p q =
  case p of {
   XI p0 ->
    case q of {
     XI q0 -> XI (lor p0 q0);
     XO q0 -> XI (lor p0 q0);
     XH -> p};
   XO p0 ->
    case q of {
     XI q0 -> XI (lor p0 q0);
     XO q0 -> XO (lor p0 q0);
     XH -> XI p0};
   XH ->
    case q of {
     XO q0 -> XI q0;
     _ -> q}}

land :: Positive -> Positive -> N
land p q =
  case p of {
   XI p0 ->
    case q of {
     XI q0 -> nsucc_double (land p0 q0);
     XO q0 -> ndouble (land p0 q0);
     XH -> Npos XH};
   XO p0 ->
    case q of {
     XI q0 -> ndouble (land p0 q0);
     XO q0 -> ndouble (land p0 q0);
     XH -> N0};
   XH ->
    case q of {
     XO q0 -> N0;
     _ -> Npos XH}}

ldiff :: Positive -> Positive -> N
ldiff p q =
  case p of {
   XI p0 ->
    case q of {
     XI q0 -> ndouble (ldiff p0 q0);
     XO q0 -> nsucc_double (ldiff p0 q0);
     XH -> Npos (XO p0)};
   XO p0 ->
    case q of {
     XI q0 -> ndouble (ldiff p0 q0);
     XO q0 -> ndouble (ldiff p0 q0);
     XH -> Npos p};
   XH ->
    case q of {
     XO q0 -> Npos XH;
     _ -> N0}}

lxor :: Positive -> Positive -> N
lxor p q =
  case p of {
   XI p0 ->
    case q of {
     XI q0 -> ndouble (lxor p0 q0);
     XO q0 -> nsucc_double (lxor p0 q0);
     XH -> Npos (XO p0)};
   XO p0 ->
    case q of {
     XI q0 -> nsucc_double (lxor p0 q0);
     XO q0 -> ndouble (lxor p0 q0);
     XH -> Npos (XI p0)};
   XH ->
    case q of {
     XI q0 -> Npos (XO q0);
     XO q0 -> Npos (XI q0);
     XH -> N0}}

shiftl_nat :: Positive -> Nat -> Positive
shiftl_nat p n =
  nat_iter n (\x -> XO x) p

shiftr_nat :: Positive -> Nat -> Positive
shiftr_nat p n =
  nat_iter n div2 p

shiftl :: Positive -> N -> Positive
shiftl p n =
  case n of {
   N0 -> p;
   Npos n0 -> iter n0 (\x -> XO x) p}

shiftr :: Positive -> N -> Positive
shiftr p n =
  case n of {
   N0 -> p;
   Npos n0 -> iter n0 div2 p}

testbit_nat :: Positive -> Nat -> Bool
testbit_nat p n =
  case p of {
   XI p0 ->
    case n of {
     O -> True;
     S n' -> testbit_nat p0 n'};
   XO p0 ->
    case n of {
     O -> False;
     S n' -> testbit_nat p0 n'};
   XH ->
    case n of {
     O -> True;
     S n0 -> False}}

testbit :: Positive -> N -> Bool
testbit p n =
  case p of {
   XI p0 ->
    case n of {
     N0 -> True;
     Npos n0 -> testbit p0 (pred_N n0)};
   XO p0 ->
    case n of {
     N0 -> False;
     Npos n0 -> testbit p0 (pred_N n0)};
   XH ->
    case n of {
     N0 -> True;
     Npos p0 -> False}}

iter_op :: (a1 -> a1 -> a1) -> Positive -> a1 -> a1
iter_op op p a =
  case p of {
   XI p0 -> op a (iter_op op p0 (op a a));
   XO p0 -> iter_op op p0 (op a a);
   XH -> a}

to_nat :: Positive -> Nat
to_nat x =
  iter_op plus x (S O)

of_nat :: Nat -> Positive
of_nat n =
  case n of {
   O -> XH;
   S x ->
    case x of {
     O -> XH;
     S n0 -> succ (of_nat x)}}

of_succ_nat :: Nat -> Positive
of_succ_nat n =
  case n of {
   O -> XH;
   S x -> succ (of_succ_nat x)}

eq_dec :: Positive -> Positive -> Sumbool
eq_dec x y =
  positive_rec (\p h y0 ->
    case y0 of {
     XI p0 -> sumbool_rec (\_ -> eq_rec_r p0 Left p) (\_ -> Right) (h p0);
     _ -> Right}) (\p h y0 ->
    case y0 of {
     XO p0 -> sumbool_rec (\_ -> eq_rec_r p0 Left p) (\_ -> Right) (h p0);
     _ -> Right}) (\y0 ->
    case y0 of {
     XH -> Left;
     _ -> Right}) x y

peano_rect :: a1 -> (Positive -> a1 -> a1) -> Positive -> a1
peano_rect a f p =
  let {f2 = peano_rect (f XH a) (\p0 x -> f (succ (XO p0)) (f (XO p0) x))} in
  case p of {
   XI q -> f (XO q) (f2 q);
   XO q -> f2 q;
   XH -> a}

peano_rec :: a1 -> (Positive -> a1 -> a1) -> Positive -> a1
peano_rec =
  peano_rect

data PeanoView =
   PeanoOne
 | PeanoSucc Positive PeanoView

peanoView_rect :: a1 -> (Positive -> PeanoView -> a1 -> a1) -> Positive ->
                  PeanoView -> a1
peanoView_rect f f0 p p0 =
  case p0 of {
   PeanoOne -> f;
   PeanoSucc p1 p2 -> f0 p1 p2 (peanoView_rect f f0 p1 p2)}

peanoView_rec :: a1 -> (Positive -> PeanoView -> a1 -> a1) -> Positive ->
                 PeanoView -> a1
peanoView_rec =
  peanoView_rect

peanoView_xO :: Positive -> PeanoView -> PeanoView
peanoView_xO p q =
  case q of {
   PeanoOne -> PeanoSucc XH PeanoOne;
   PeanoSucc p0 q0 -> PeanoSucc (succ (XO p0)) (PeanoSucc (XO p0)
    (peanoView_xO p0 q0))}

peanoView_xI :: Positive -> PeanoView -> PeanoView
peanoView_xI p q =
  case q of {
   PeanoOne -> PeanoSucc (succ XH) (PeanoSucc XH PeanoOne);
   PeanoSucc p0 q0 -> PeanoSucc (succ (XI p0)) (PeanoSucc (XI p0)
    (peanoView_xI p0 q0))}

peanoView :: Positive -> PeanoView
peanoView p =
  case p of {
   XI p0 -> peanoView_xI p0 (peanoView p0);
   XO p0 -> peanoView_xO p0 (peanoView p0);
   XH -> PeanoOne}

peanoView_iter :: a1 -> (Positive -> a1 -> a1) -> Positive -> PeanoView -> a1
peanoView_iter a f p q =
  case q of {
   PeanoOne -> a;
   PeanoSucc p0 q0 -> f p0 (peanoView_iter a f p0 q0)}

eqb_spec :: Positive -> Positive -> Reflect
eqb_spec x y =
  iff_reflect (eqb x y)

switch_Eq :: Comparison -> Comparison -> Comparison
switch_Eq c c' =
  case c' of {
   Eq -> c;
   x -> x}

mask2cmp :: Mask -> Comparison
mask2cmp p =
  case p of {
   IsNul -> Eq;
   IsPos p0 -> Gt;
   IsNeg -> Lt}

leb_spec0 :: Positive -> Positive -> Reflect
leb_spec0 x y =
  iff_reflect (leb x y)

ltb_spec0 :: Positive -> Positive -> Reflect
ltb_spec0 x y =
  iff_reflect (ltb x y)

max_case_strong :: Positive -> Positive -> (Positive -> Positive -> () -> a1
                   -> a1) -> (() -> a1) -> (() -> a1) -> a1
max_case_strong n m0 compat hl hr =
  let {c = compSpec2Type n m0 (compare n m0)} in
  case c of {
   CompGtT -> compat n (max0 n m0) __ (hl __);
   _ -> compat m0 (max0 n m0) __ (hr __)}

max_case :: Positive -> Positive -> (Positive -> Positive -> () -> a1 -> a1)
            -> a1 -> a1 -> a1
max_case n m0 x x0 x1 =
  max_case_strong n m0 x (\_ -> x0) (\_ -> x1)

max_dec :: Positive -> Positive -> Sumbool
max_dec n m0 =
  max_case n m0 (\x y _ h0 -> h0) Left Right

min_case_strong :: Positive -> Positive -> (Positive -> Positive -> () -> a1
                   -> a1) -> (() -> a1) -> (() -> a1) -> a1
min_case_strong n m0 compat hl hr =
  let {c = compSpec2Type n m0 (compare n m0)} in
  case c of {
   CompGtT -> compat m0 (min0 n m0) __ (hr __);
   _ -> compat n (min0 n m0) __ (hl __)}

min_case :: Positive -> Positive -> (Positive -> Positive -> () -> a1 -> a1)
            -> a1 -> a1 -> a1
min_case n m0 x x0 x1 =
  min_case_strong n m0 x (\_ -> x0) (\_ -> x1)

min_dec :: Positive -> Positive -> Sumbool
min_dec n m0 =
  min_case n m0 (\x y _ h0 -> h0) Left Right

max_case_strong0 :: Positive -> Positive -> (() -> a1) -> (() -> a1) -> a1
max_case_strong0 n m0 x x0 =
  max_case_strong n m0 (\x1 y _ x2 -> eq_rect __ x2 __) x x0

max_case0 :: Positive -> Positive -> a1 -> a1 -> a1
max_case0 n m0 x x0 =
  max_case_strong0 n m0 (\_ -> x) (\_ -> x0)

max_dec0 :: Positive -> Positive -> Sumbool
max_dec0 =
  max_dec

min_case_strong0 :: Positive -> Positive -> (() -> a1) -> (() -> a1) -> a1
min_case_strong0 n m0 x x0 =
  min_case_strong n m0 (\x1 y _ x2 -> eq_rect __ x2 __) x x0

min_case0 :: Positive -> Positive -> a1 -> a1 -> a1
min_case0 n m0 x x0 =
  min_case_strong0 n m0 (\_ -> x) (\_ -> x0)

min_dec0 :: Positive -> Positive -> Sumbool
min_dec0 =
  min_dec

type T0 = N

zero :: N
zero =
  N0

one :: N
one =
  Npos XH

two :: N
two =
  Npos (XO XH)

succ_double :: N -> N
succ_double x =
  case x of {
   N0 -> Npos XH;
   Npos p -> Npos (XI p)}

double :: N -> N
double n =
  case n of {
   N0 -> N0;
   Npos p -> Npos (XO p)}

succ0 :: N -> N
succ0 n =
  case n of {
   N0 -> Npos XH;
   Npos p -> Npos (succ p)}

pred1 :: N -> N
pred1 n =
  case n of {
   N0 -> N0;
   Npos p -> pred_N p}

succ_pos :: N -> Positive
succ_pos n =
  case n of {
   N0 -> XH;
   Npos p -> succ p}

add0 :: N -> N -> N
add0 n m0 =
  case n of {
   N0 -> m0;
   Npos p ->
    case m0 of {
     N0 -> n;
     Npos q -> Npos (add p q)}}

sub0 :: N -> N -> N
sub0 n m0 =
  case n of {
   N0 -> N0;
   Npos n' ->
    case m0 of {
     N0 -> n;
     Npos m'0 ->
      case sub_mask n' m'0 of {
       IsPos p -> Npos p;
       _ -> N0}}}

mul0 :: N -> N -> N
mul0 n m0 =
  case n of {
   N0 -> N0;
   Npos p ->
    case m0 of {
     N0 -> N0;
     Npos q -> Npos (mul p q)}}

compare0 :: N -> N -> Comparison
compare0 n m0 =
  case n of {
   N0 ->
    case m0 of {
     N0 -> Eq;
     Npos m'0 -> Lt};
   Npos n' ->
    case m0 of {
     N0 -> Gt;
     Npos m'0 -> compare n' m'0}}

eqb0 :: N -> N -> Bool
eqb0 n m0 =
  case n of {
   N0 ->
    case m0 of {
     N0 -> True;
     Npos p -> False};
   Npos p ->
    case m0 of {
     N0 -> False;
     Npos q -> eqb p q}}

leb0 :: N -> N -> Bool
leb0 x y =
  case compare0 x y of {
   Gt -> False;
   _ -> True}

ltb0 :: N -> N -> Bool
ltb0 x y =
  case compare0 x y of {
   Lt -> True;
   _ -> False}

min1 :: N -> N -> N
min1 n n' =
  case compare0 n n' of {
   Gt -> n';
   _ -> n}

max1 :: N -> N -> N
max1 n n' =
  case compare0 n n' of {
   Gt -> n;
   _ -> n'}

div0 :: N -> N
div0 n =
  case n of {
   N0 -> N0;
   Npos p0 ->
    case p0 of {
     XI p -> Npos p;
     XO p -> Npos p;
     XH -> N0}}

even :: N -> Bool
even n =
  case n of {
   N0 -> True;
   Npos p ->
    case p of {
     XO p0 -> True;
     _ -> False}}

odd :: N -> Bool
odd n =
  negb (even n)

pow0 :: N -> N -> N
pow0 n p =
  case p of {
   N0 -> Npos XH;
   Npos p0 ->
    case n of {
     N0 -> N0;
     Npos q -> Npos (pow q p0)}}

square0 :: N -> N
square0 n =
  case n of {
   N0 -> N0;
   Npos p -> Npos (square p)}

log2 :: N -> N
log2 n =
  case n of {
   N0 -> N0;
   Npos p0 ->
    case p0 of {
     XI p -> Npos (size p);
     XO p -> Npos (size p);
     XH -> N0}}

size0 :: N -> N
size0 n =
  case n of {
   N0 -> N0;
   Npos p -> Npos (size p)}

size_nat0 :: N -> Nat
size_nat0 n =
  case n of {
   N0 -> O;
   Npos p -> size_nat p}

pos_div_eucl :: Positive -> N -> Prod N N
pos_div_eucl a b =
  case a of {
   XI a' ->
    case pos_div_eucl a' b of {
     Pair q r ->
      let {r' = succ_double r} in
      case leb0 b r' of {
       True -> Pair (succ_double q) (sub0 r' b);
       False -> Pair (double q) r'}};
   XO a' ->
    case pos_div_eucl a' b of {
     Pair q r ->
      let {r' = double r} in
      case leb0 b r' of {
       True -> Pair (succ_double q) (sub0 r' b);
       False -> Pair (double q) r'}};
   XH ->
    case b of {
     N0 -> Pair N0 (Npos XH);
     Npos p ->
      case p of {
       XH -> Pair (Npos XH) N0;
       _ -> Pair N0 (Npos XH)}}}

div_eucl :: N -> N -> Prod N N
div_eucl a b =
  case a of {
   N0 -> Pair N0 N0;
   Npos na ->
    case b of {
     N0 -> Pair N0 a;
     Npos p -> pos_div_eucl na b}}

div :: N -> N -> N
div a b =
  fst (div_eucl a b)

modulo :: N -> N -> N
modulo a b =
  snd (div_eucl a b)

gcd0 :: N -> N -> N
gcd0 a b =
  case a of {
   N0 -> b;
   Npos p ->
    case b of {
     N0 -> a;
     Npos q -> Npos (gcd p q)}}

ggcd0 :: N -> N -> Prod N (Prod N N)
ggcd0 a b =
  case a of {
   N0 -> Pair b (Pair N0 (Npos XH));
   Npos p ->
    case b of {
     N0 -> Pair a (Pair (Npos XH) N0);
     Npos q ->
      case ggcd p q of {
       Pair g p0 ->
        case p0 of {
         Pair aa bb -> Pair (Npos g) (Pair (Npos aa) (Npos bb))}}}}

sqrtrem0 :: N -> Prod N N
sqrtrem0 n =
  case n of {
   N0 -> Pair N0 N0;
   Npos p ->
    case sqrtrem p of {
     Pair s m0 ->
      case m0 of {
       IsPos r -> Pair (Npos s) (Npos r);
       _ -> Pair (Npos s) N0}}}

sqrt0 :: N -> N
sqrt0 n =
  case n of {
   N0 -> N0;
   Npos p -> Npos (sqrt p)}

lor0 :: N -> N -> N
lor0 n m0 =
  case n of {
   N0 -> m0;
   Npos p ->
    case m0 of {
     N0 -> n;
     Npos q -> Npos (lor p q)}}

land0 :: N -> N -> N
land0 n m0 =
  case n of {
   N0 -> N0;
   Npos p ->
    case m0 of {
     N0 -> N0;
     Npos q -> land p q}}

ldiff0 :: N -> N -> N
ldiff0 n m0 =
  case n of {
   N0 -> N0;
   Npos p ->
    case m0 of {
     N0 -> n;
     Npos q -> ldiff p q}}

lxor0 :: N -> N -> N
lxor0 n m0 =
  case n of {
   N0 -> m0;
   Npos p ->
    case m0 of {
     N0 -> n;
     Npos q -> lxor p q}}

shiftl_nat0 :: N -> Nat -> N
shiftl_nat0 a n =
  nat_iter n double a

shiftr_nat0 :: N -> Nat -> N
shiftr_nat0 a n =
  nat_iter n div0 a

shiftl0 :: N -> N -> N
shiftl0 a n =
  case a of {
   N0 -> N0;
   Npos a0 -> Npos (shiftl a0 n)}

shiftr0 :: N -> N -> N
shiftr0 a n =
  case n of {
   N0 -> a;
   Npos p -> iter p div0 a}

testbit_nat0 :: N -> Nat -> Bool
testbit_nat0 a =
  case a of {
   N0 -> (\x -> False);
   Npos p -> testbit_nat p}

testbit0 :: N -> N -> Bool
testbit0 a n =
  case a of {
   N0 -> False;
   Npos p -> testbit p n}

to_nat0 :: N -> Nat
to_nat0 a =
  case a of {
   N0 -> O;
   Npos p -> to_nat p}

of_nat0 :: Nat -> N
of_nat0 n =
  case n of {
   O -> N0;
   S n' -> Npos (of_succ_nat n')}

iter0 :: N -> (a1 -> a1) -> a1 -> a1
iter0 n f x =
  case n of {
   N0 -> x;
   Npos p -> iter p f x}

eq_dec0 :: N -> N -> Sumbool
eq_dec0 n m0 =
  n_rec (\m1 ->
    case m1 of {
     N0 -> Left;
     Npos p -> Right}) (\p m1 ->
    case m1 of {
     N0 -> Right;
     Npos p0 ->
      sumbool_rec (\_ -> eq_rec_r p0 Left p) (\_ -> Right) (eq_dec p p0)}) n
    m0

discr :: N -> Sumor Positive
discr n =
  case n of {
   N0 -> Inright;
   Npos p -> Inleft p}

binary_rect :: a1 -> (N -> a1 -> a1) -> (N -> a1 -> a1) -> N -> a1
binary_rect f0 f2 fS2 n =
  let {f2' = \p -> f2 (Npos p)} in
  let {fS2' = \p -> fS2 (Npos p)} in
  case n of {
   N0 -> f0;
   Npos p -> positive_rect fS2' f2' (fS2 N0 f0) p}

binary_rec :: a1 -> (N -> a1 -> a1) -> (N -> a1 -> a1) -> N -> a1
binary_rec =
  binary_rect

peano_rect0 :: a1 -> (N -> a1 -> a1) -> N -> a1
peano_rect0 f0 f n =
  let {f' = \p -> f (Npos p)} in
  case n of {
   N0 -> f0;
   Npos p -> peano_rect (f N0 f0) f' p}

peano_rec0 :: a1 -> (N -> a1 -> a1) -> N -> a1
peano_rec0 =
  peano_rect0

leb_spec1 :: N -> N -> Reflect
leb_spec1 x y =
  iff_reflect (leb0 x y)

ltb_spec1 :: N -> N -> Reflect
ltb_spec1 x y =
  iff_reflect (ltb0 x y)

recursion :: a1 -> (N -> a1 -> a1) -> N -> a1
recursion =
  peano_rect0

sqrt_up :: N -> N
sqrt_up a =
  case compare0 N0 a of {
   Lt -> succ0 (sqrt0 (pred1 a));
   _ -> N0}

log2_up :: N -> N
log2_up a =
  case compare0 (Npos XH) a of {
   Lt -> succ0 (log2 (pred1 a));
   _ -> N0}

lcm :: N -> N -> N
lcm a b =
  mul0 a (div b (gcd0 a b))

eqb_spec0 :: N -> N -> Reflect
eqb_spec0 x y =
  iff_reflect (eqb0 x y)

b2n :: Bool -> N
b2n b =
  case b of {
   True -> Npos XH;
   False -> N0}

setbit :: N -> N -> N
setbit a n =
  lor0 a (shiftl0 (Npos XH) n)

clearbit :: N -> N -> N
clearbit a n =
  ldiff0 a (shiftl0 (Npos XH) n)

ones :: N -> N
ones n =
  pred1 (shiftl0 (Npos XH) n)

lnot :: N -> N -> N
lnot a n =
  lxor0 a (ones n)

max_case_strong1 :: N -> N -> (N -> N -> () -> a1 -> a1) -> (() -> a1) -> (()
                    -> a1) -> a1
max_case_strong1 n m0 compat hl hr =
  let {c = compSpec2Type n m0 (compare0 n m0)} in
  case c of {
   CompGtT -> compat n (max1 n m0) __ (hl __);
   _ -> compat m0 (max1 n m0) __ (hr __)}

max_case1 :: N -> N -> (N -> N -> () -> a1 -> a1) -> a1 -> a1 -> a1
max_case1 n m0 x x0 x1 =
  max_case_strong1 n m0 x (\_ -> x0) (\_ -> x1)

max_dec1 :: N -> N -> Sumbool
max_dec1 n m0 =
  max_case1 n m0 (\x y _ h0 -> h0) Left Right

min_case_strong1 :: N -> N -> (N -> N -> () -> a1 -> a1) -> (() -> a1) -> (()
                    -> a1) -> a1
min_case_strong1 n m0 compat hl hr =
  let {c = compSpec2Type n m0 (compare0 n m0)} in
  case c of {
   CompGtT -> compat m0 (min1 n m0) __ (hr __);
   _ -> compat n (min1 n m0) __ (hl __)}

min_case1 :: N -> N -> (N -> N -> () -> a1 -> a1) -> a1 -> a1 -> a1
min_case1 n m0 x x0 x1 =
  min_case_strong1 n m0 x (\_ -> x0) (\_ -> x1)

min_dec1 :: N -> N -> Sumbool
min_dec1 n m0 =
  min_case1 n m0 (\x y _ h0 -> h0) Left Right

max_case_strong2 :: N -> N -> (() -> a1) -> (() -> a1) -> a1
max_case_strong2 n m0 x x0 =
  max_case_strong1 n m0 (\x1 y _ x2 -> eq_rect __ x2 __) x x0

max_case2 :: N -> N -> a1 -> a1 -> a1
max_case2 n m0 x x0 =
  max_case_strong2 n m0 (\_ -> x) (\_ -> x0)

max_dec2 :: N -> N -> Sumbool
max_dec2 =
  max_dec1

min_case_strong2 :: N -> N -> (() -> a1) -> (() -> a1) -> a1
min_case_strong2 n m0 x x0 =
  min_case_strong1 n m0 (\x1 y _ x2 -> eq_rect __ x2 __) x x0

min_case2 :: N -> N -> a1 -> a1 -> a1
min_case2 n m0 x x0 =
  min_case_strong2 n m0 (\_ -> x) (\_ -> x0)

min_dec2 :: N -> N -> Sumbool
min_dec2 =
  min_dec1

nat_compare :: Nat -> Nat -> Comparison
nat_compare n m0 =
  case n of {
   O ->
    case m0 of {
     O -> Eq;
     S n0 -> Lt};
   S n' ->
    case m0 of {
     O -> Gt;
     S m'0 -> nat_compare n' m'0}}

type T1 = Z

zero0 :: Z
zero0 =
  Z0

one0 :: Z
one0 =
  Zpos XH

two0 :: Z
two0 =
  Zpos (XO XH)

double0 :: Z -> Z
double0 x =
  case x of {
   Z0 -> Z0;
   Zpos p -> Zpos (XO p);
   Zneg p -> Zneg (XO p)}

succ_double0 :: Z -> Z
succ_double0 x =
  case x of {
   Z0 -> Zpos XH;
   Zpos p -> Zpos (XI p);
   Zneg p -> Zneg (pred_double p)}

pred_double0 :: Z -> Z
pred_double0 x =
  case x of {
   Z0 -> Zneg XH;
   Zpos p -> Zpos (pred_double p);
   Zneg p -> Zneg (XI p)}

pos_sub :: Positive -> Positive -> Z
pos_sub x y =
  case x of {
   XI p ->
    case y of {
     XI q -> double0 (pos_sub p q);
     XO q -> succ_double0 (pos_sub p q);
     XH -> Zpos (XO p)};
   XO p ->
    case y of {
     XI q -> pred_double0 (pos_sub p q);
     XO q -> double0 (pos_sub p q);
     XH -> Zpos (pred_double p)};
   XH ->
    case y of {
     XI q -> Zneg (XO q);
     XO q -> Zneg (pred_double q);
     XH -> Z0}}

add1 :: Z -> Z -> Z
add1 x y =
  case x of {
   Z0 -> y;
   Zpos x' ->
    case y of {
     Z0 -> x;
     Zpos y' -> Zpos (add x' y');
     Zneg y' -> pos_sub x' y'};
   Zneg x' ->
    case y of {
     Z0 -> x;
     Zpos y' -> pos_sub y' x';
     Zneg y' -> Zneg (add x' y')}}

opp :: Z -> Z
opp x =
  case x of {
   Z0 -> Z0;
   Zpos x0 -> Zneg x0;
   Zneg x0 -> Zpos x0}

succ1 :: Z -> Z
succ1 x =
  add1 x (Zpos XH)

pred2 :: Z -> Z
pred2 x =
  add1 x (Zneg XH)

sub1 :: Z -> Z -> Z
sub1 m0 n =
  add1 m0 (opp n)

mul1 :: Z -> Z -> Z
mul1 x y =
  case x of {
   Z0 -> Z0;
   Zpos x' ->
    case y of {
     Z0 -> Z0;
     Zpos y' -> Zpos (mul x' y');
     Zneg y' -> Zneg (mul x' y')};
   Zneg x' ->
    case y of {
     Z0 -> Z0;
     Zpos y' -> Zneg (mul x' y');
     Zneg y' -> Zpos (mul x' y')}}

pow_pos :: Z -> Positive -> Z
pow_pos z n =
  iter n (mul1 z) (Zpos XH)

pow1 :: Z -> Z -> Z
pow1 x y =
  case y of {
   Z0 -> Zpos XH;
   Zpos p -> pow_pos x p;
   Zneg p -> Z0}

square1 :: Z -> Z
square1 x =
  case x of {
   Z0 -> Z0;
   Zpos p -> Zpos (square p);
   Zneg p -> Zpos (square p)}

compare1 :: Z -> Z -> Comparison
compare1 x y =
  case x of {
   Z0 ->
    case y of {
     Z0 -> Eq;
     Zpos y' -> Lt;
     Zneg y' -> Gt};
   Zpos x' ->
    case y of {
     Zpos y' -> compare x' y';
     _ -> Gt};
   Zneg x' ->
    case y of {
     Zneg y' -> compOpp (compare x' y');
     _ -> Lt}}

sgn :: Z -> Z
sgn z =
  case z of {
   Z0 -> Z0;
   Zpos p -> Zpos XH;
   Zneg p -> Zneg XH}

leb1 :: Z -> Z -> Bool
leb1 x y =
  case compare1 x y of {
   Gt -> False;
   _ -> True}

ltb1 :: Z -> Z -> Bool
ltb1 x y =
  case compare1 x y of {
   Lt -> True;
   _ -> False}

geb :: Z -> Z -> Bool
geb x y =
  case compare1 x y of {
   Lt -> False;
   _ -> True}

gtb :: Z -> Z -> Bool
gtb x y =
  case compare1 x y of {
   Gt -> True;
   _ -> False}

eqb1 :: Z -> Z -> Bool
eqb1 x y =
  case x of {
   Z0 ->
    case y of {
     Z0 -> True;
     _ -> False};
   Zpos p ->
    case y of {
     Zpos q -> eqb p q;
     _ -> False};
   Zneg p ->
    case y of {
     Zneg q -> eqb p q;
     _ -> False}}

max2 :: Z -> Z -> Z
max2 n m0 =
  case compare1 n m0 of {
   Lt -> m0;
   _ -> n}

min2 :: Z -> Z -> Z
min2 n m0 =
  case compare1 n m0 of {
   Gt -> m0;
   _ -> n}

abs :: Z -> Z
abs z =
  case z of {
   Zneg p -> Zpos p;
   x -> x}

abs_nat :: Z -> Nat
abs_nat z =
  case z of {
   Z0 -> O;
   Zpos p -> to_nat p;
   Zneg p -> to_nat p}

abs_N :: Z -> N
abs_N z =
  case z of {
   Z0 -> N0;
   Zpos p -> Npos p;
   Zneg p -> Npos p}

to_nat1 :: Z -> Nat
to_nat1 z =
  case z of {
   Zpos p -> to_nat p;
   _ -> O}

to_N :: Z -> N
to_N z =
  case z of {
   Zpos p -> Npos p;
   _ -> N0}

of_nat1 :: Nat -> Z
of_nat1 n =
  case n of {
   O -> Z0;
   S n0 -> Zpos (of_succ_nat n0)}

of_N :: N -> Z
of_N n =
  case n of {
   N0 -> Z0;
   Npos p -> Zpos p}

to_pos :: Z -> Positive
to_pos z =
  case z of {
   Zpos p -> p;
   _ -> XH}

iter1 :: Z -> (a1 -> a1) -> a1 -> a1
iter1 n f x =
  case n of {
   Zpos p -> iter p f x;
   _ -> x}

pos_div_eucl0 :: Positive -> Z -> Prod Z Z
pos_div_eucl0 a b =
  case a of {
   XI a' ->
    case pos_div_eucl0 a' b of {
     Pair q r ->
      let {r' = add1 (mul1 (Zpos (XO XH)) r) (Zpos XH)} in
      case ltb1 r' b of {
       True -> Pair (mul1 (Zpos (XO XH)) q) r';
       False -> Pair (add1 (mul1 (Zpos (XO XH)) q) (Zpos XH)) (sub1 r' b)}};
   XO a' ->
    case pos_div_eucl0 a' b of {
     Pair q r ->
      let {r' = mul1 (Zpos (XO XH)) r} in
      case ltb1 r' b of {
       True -> Pair (mul1 (Zpos (XO XH)) q) r';
       False -> Pair (add1 (mul1 (Zpos (XO XH)) q) (Zpos XH)) (sub1 r' b)}};
   XH ->
    case leb1 (Zpos (XO XH)) b of {
     True -> Pair Z0 (Zpos XH);
     False -> Pair (Zpos XH) Z0}}

div_eucl0 :: Z -> Z -> Prod Z Z
div_eucl0 a b =
  case a of {
   Z0 -> Pair Z0 Z0;
   Zpos a' ->
    case b of {
     Z0 -> Pair Z0 Z0;
     Zpos p -> pos_div_eucl0 a' b;
     Zneg b' ->
      case pos_div_eucl0 a' (Zpos b') of {
       Pair q r ->
        case r of {
         Z0 -> Pair (opp q) Z0;
         _ -> Pair (opp (add1 q (Zpos XH))) (add1 b r)}}};
   Zneg a' ->
    case b of {
     Z0 -> Pair Z0 Z0;
     Zpos p ->
      case pos_div_eucl0 a' b of {
       Pair q r ->
        case r of {
         Z0 -> Pair (opp q) Z0;
         _ -> Pair (opp (add1 q (Zpos XH))) (sub1 b r)}};
     Zneg b' ->
      case pos_div_eucl0 a' (Zpos b') of {
       Pair q r -> Pair q (opp r)}}}

div1 :: Z -> Z -> Z
div1 a b =
  case div_eucl0 a b of {
   Pair q x -> q}

modulo0 :: Z -> Z -> Z
modulo0 a b =
  case div_eucl0 a b of {
   Pair x r -> r}

quotrem :: Z -> Z -> Prod Z Z
quotrem a b =
  case a of {
   Z0 -> Pair Z0 Z0;
   Zpos a0 ->
    case b of {
     Z0 -> Pair Z0 a;
     Zpos b0 ->
      case pos_div_eucl a0 (Npos b0) of {
       Pair q r -> Pair (of_N q) (of_N r)};
     Zneg b0 ->
      case pos_div_eucl a0 (Npos b0) of {
       Pair q r -> Pair (opp (of_N q)) (of_N r)}};
   Zneg a0 ->
    case b of {
     Z0 -> Pair Z0 a;
     Zpos b0 ->
      case pos_div_eucl a0 (Npos b0) of {
       Pair q r -> Pair (opp (of_N q)) (opp (of_N r))};
     Zneg b0 ->
      case pos_div_eucl a0 (Npos b0) of {
       Pair q r -> Pair (of_N q) (opp (of_N r))}}}

quot :: Z -> Z -> Z
quot a b =
  fst (quotrem a b)

rem :: Z -> Z -> Z
rem a b =
  snd (quotrem a b)

even0 :: Z -> Bool
even0 z =
  case z of {
   Z0 -> True;
   Zpos p ->
    case p of {
     XO p0 -> True;
     _ -> False};
   Zneg p ->
    case p of {
     XO p0 -> True;
     _ -> False}}

odd0 :: Z -> Bool
odd0 z =
  case z of {
   Z0 -> False;
   Zpos p ->
    case p of {
     XO p0 -> False;
     _ -> True};
   Zneg p ->
    case p of {
     XO p0 -> False;
     _ -> True}}

div3 :: Z -> Z
div3 z =
  case z of {
   Z0 -> Z0;
   Zpos p ->
    case p of {
     XH -> Z0;
     _ -> Zpos (div2 p)};
   Zneg p -> Zneg (div2_up p)}

quot2 :: Z -> Z
quot2 z =
  case z of {
   Z0 -> Z0;
   Zpos p ->
    case p of {
     XH -> Z0;
     _ -> Zpos (div2 p)};
   Zneg p ->
    case p of {
     XH -> Z0;
     _ -> Zneg (div2 p)}}

log0 :: Z -> Z
log0 z =
  case z of {
   Zpos p0 ->
    case p0 of {
     XI p -> Zpos (size p);
     XO p -> Zpos (size p);
     XH -> Z0};
   _ -> Z0}

sqrtrem1 :: Z -> Prod Z Z
sqrtrem1 n =
  case n of {
   Zpos p ->
    case sqrtrem p of {
     Pair s m0 ->
      case m0 of {
       IsPos r -> Pair (Zpos s) (Zpos r);
       _ -> Pair (Zpos s) Z0}};
   _ -> Pair Z0 Z0}

sqrt1 :: Z -> Z
sqrt1 n =
  case n of {
   Zpos p -> Zpos (sqrt p);
   _ -> Z0}

gcd1 :: Z -> Z -> Z
gcd1 a b =
  case a of {
   Z0 -> abs b;
   Zpos a0 ->
    case b of {
     Z0 -> abs a;
     Zpos b0 -> Zpos (gcd a0 b0);
     Zneg b0 -> Zpos (gcd a0 b0)};
   Zneg a0 ->
    case b of {
     Z0 -> abs a;
     Zpos b0 -> Zpos (gcd a0 b0);
     Zneg b0 -> Zpos (gcd a0 b0)}}

ggcd1 :: Z -> Z -> Prod Z (Prod Z Z)
ggcd1 a b =
  case a of {
   Z0 -> Pair (abs b) (Pair Z0 (sgn b));
   Zpos a0 ->
    case b of {
     Z0 -> Pair (abs a) (Pair (sgn a) Z0);
     Zpos b0 ->
      case ggcd a0 b0 of {
       Pair g p ->
        case p of {
         Pair aa bb -> Pair (Zpos g) (Pair (Zpos aa) (Zpos bb))}};
     Zneg b0 ->
      case ggcd a0 b0 of {
       Pair g p ->
        case p of {
         Pair aa bb -> Pair (Zpos g) (Pair (Zpos aa) (Zneg bb))}}};
   Zneg a0 ->
    case b of {
     Z0 -> Pair (abs a) (Pair (sgn a) Z0);
     Zpos b0 ->
      case ggcd a0 b0 of {
       Pair g p ->
        case p of {
         Pair aa bb -> Pair (Zpos g) (Pair (Zneg aa) (Zpos bb))}};
     Zneg b0 ->
      case ggcd a0 b0 of {
       Pair g p ->
        case p of {
         Pair aa bb -> Pair (Zpos g) (Pair (Zneg aa) (Zneg bb))}}}}

testbit1 :: Z -> Z -> Bool
testbit1 a n =
  case n of {
   Z0 -> odd0 a;
   Zpos p ->
    case a of {
     Z0 -> False;
     Zpos a0 -> testbit a0 (Npos p);
     Zneg a0 -> negb (testbit0 (pred_N a0) (Npos p))};
   Zneg p -> False}

shiftl1 :: Z -> Z -> Z
shiftl1 a n =
  case n of {
   Z0 -> a;
   Zpos p -> iter p (mul1 (Zpos (XO XH))) a;
   Zneg p -> iter p div3 a}

shiftr1 :: Z -> Z -> Z
shiftr1 a n =
  shiftl1 a (opp n)

lor1 :: Z -> Z -> Z
lor1 a b =
  case a of {
   Z0 -> b;
   Zpos a0 ->
    case b of {
     Z0 -> a;
     Zpos b0 -> Zpos (lor a0 b0);
     Zneg b0 -> Zneg (succ_pos (ldiff0 (pred_N b0) (Npos a0)))};
   Zneg a0 ->
    case b of {
     Z0 -> a;
     Zpos b0 -> Zneg (succ_pos (ldiff0 (pred_N a0) (Npos b0)));
     Zneg b0 -> Zneg (succ_pos (land0 (pred_N a0) (pred_N b0)))}}

land1 :: Z -> Z -> Z
land1 a b =
  case a of {
   Z0 -> Z0;
   Zpos a0 ->
    case b of {
     Z0 -> Z0;
     Zpos b0 -> of_N (land a0 b0);
     Zneg b0 -> of_N (ldiff0 (Npos a0) (pred_N b0))};
   Zneg a0 ->
    case b of {
     Z0 -> Z0;
     Zpos b0 -> of_N (ldiff0 (Npos b0) (pred_N a0));
     Zneg b0 -> Zneg (succ_pos (lor0 (pred_N a0) (pred_N b0)))}}

ldiff1 :: Z -> Z -> Z
ldiff1 a b =
  case a of {
   Z0 -> Z0;
   Zpos a0 ->
    case b of {
     Z0 -> a;
     Zpos b0 -> of_N (ldiff a0 b0);
     Zneg b0 -> of_N (land0 (Npos a0) (pred_N b0))};
   Zneg a0 ->
    case b of {
     Z0 -> a;
     Zpos b0 -> Zneg (succ_pos (lor0 (pred_N a0) (Npos b0)));
     Zneg b0 -> of_N (ldiff0 (pred_N b0) (pred_N a0))}}

lxor1 :: Z -> Z -> Z
lxor1 a b =
  case a of {
   Z0 -> b;
   Zpos a0 ->
    case b of {
     Z0 -> a;
     Zpos b0 -> of_N (lxor a0 b0);
     Zneg b0 -> Zneg (succ_pos (lxor0 (Npos a0) (pred_N b0)))};
   Zneg a0 ->
    case b of {
     Z0 -> a;
     Zpos b0 -> Zneg (succ_pos (lxor0 (pred_N a0) (Npos b0)));
     Zneg b0 -> of_N (lxor0 (pred_N a0) (pred_N b0))}}

eq_dec1 :: Z -> Z -> Sumbool
eq_dec1 x y =
  z_rec (\y0 ->
    case y0 of {
     Z0 -> Left;
     _ -> Right}) (\p y0 ->
    case y0 of {
     Zpos p0 ->
      sumbool_rec (\_ -> eq_rec_r p0 Left p) (\_ -> Right) (eq_dec p p0);
     _ -> Right}) (\p y0 ->
    case y0 of {
     Zneg p0 ->
      sumbool_rec (\_ -> eq_rec_r p0 Left p) (\_ -> Right) (eq_dec p p0);
     _ -> Right}) x y

leb_spec2 :: Z -> Z -> Reflect
leb_spec2 x y =
  iff_reflect (leb1 x y)

ltb_spec2 :: Z -> Z -> Reflect
ltb_spec2 x y =
  iff_reflect (ltb1 x y)

sqrt_up0 :: Z -> Z
sqrt_up0 a =
  case compare1 Z0 a of {
   Lt -> succ1 (sqrt1 (pred2 a));
   _ -> Z0}

log2_up0 :: Z -> Z
log2_up0 a =
  case compare1 (Zpos XH) a of {
   Lt -> succ1 (log0 (pred2 a));
   _ -> Z0}

div4 :: Z -> Z -> Z
div4 =
  quot

modulo1 :: Z -> Z -> Z
modulo1 =
  rem

lcm0 :: Z -> Z -> Z
lcm0 a b =
  abs (mul1 a (div1 b (gcd1 a b)))

eqb_spec1 :: Z -> Z -> Reflect
eqb_spec1 x y =
  iff_reflect (eqb1 x y)

b2z :: Bool -> Z
b2z b =
  case b of {
   True -> Zpos XH;
   False -> Z0}

setbit0 :: Z -> Z -> Z
setbit0 a n =
  lor1 a (shiftl1 (Zpos XH) n)

clearbit0 :: Z -> Z -> Z
clearbit0 a n =
  ldiff1 a (shiftl1 (Zpos XH) n)

lnot0 :: Z -> Z
lnot0 a =
  pred2 (opp a)

ones0 :: Z -> Z
ones0 n =
  pred2 (shiftl1 (Zpos XH) n)

max_case_strong3 :: Z -> Z -> (Z -> Z -> () -> a1 -> a1) -> (() -> a1) -> (()
                    -> a1) -> a1
max_case_strong3 n m0 compat hl hr =
  let {c = compSpec2Type n m0 (compare1 n m0)} in
  case c of {
   CompGtT -> compat n (max2 n m0) __ (hl __);
   _ -> compat m0 (max2 n m0) __ (hr __)}

max_case3 :: Z -> Z -> (Z -> Z -> () -> a1 -> a1) -> a1 -> a1 -> a1
max_case3 n m0 x x0 x1 =
  max_case_strong3 n m0 x (\_ -> x0) (\_ -> x1)

max_dec3 :: Z -> Z -> Sumbool
max_dec3 n m0 =
  max_case3 n m0 (\x y _ h0 -> h0) Left Right

min_case_strong3 :: Z -> Z -> (Z -> Z -> () -> a1 -> a1) -> (() -> a1) -> (()
                    -> a1) -> a1
min_case_strong3 n m0 compat hl hr =
  let {c = compSpec2Type n m0 (compare1 n m0)} in
  case c of {
   CompGtT -> compat m0 (min2 n m0) __ (hr __);
   _ -> compat n (min2 n m0) __ (hl __)}

min_case3 :: Z -> Z -> (Z -> Z -> () -> a1 -> a1) -> a1 -> a1 -> a1
min_case3 n m0 x x0 x1 =
  min_case_strong3 n m0 x (\_ -> x0) (\_ -> x1)

min_dec3 :: Z -> Z -> Sumbool
min_dec3 n m0 =
  min_case3 n m0 (\x y _ h0 -> h0) Left Right

max_case_strong4 :: Z -> Z -> (() -> a1) -> (() -> a1) -> a1
max_case_strong4 n m0 x x0 =
  max_case_strong3 n m0 (\x1 y _ x2 -> eq_rect __ x2 __) x x0

max_case4 :: Z -> Z -> a1 -> a1 -> a1
max_case4 n m0 x x0 =
  max_case_strong4 n m0 (\_ -> x) (\_ -> x0)

max_dec4 :: Z -> Z -> Sumbool
max_dec4 =
  max_dec3

min_case_strong4 :: Z -> Z -> (() -> a1) -> (() -> a1) -> a1
min_case_strong4 n m0 x x0 =
  min_case_strong3 n m0 (\x1 y _ x2 -> eq_rect __ x2 __) x x0

min_case4 :: Z -> Z -> a1 -> a1 -> a1
min_case4 n m0 x x0 =
  min_case_strong4 n m0 (\_ -> x) (\_ -> x0)

min_dec4 :: Z -> Z -> Sumbool
min_dec4 =
  min_dec3

eq_nat_dec :: Nat -> Nat -> Sumbool
eq_nat_dec n =
  nat_rec (\m0 ->
    case m0 of {
     O -> Left;
     S m1 -> Right}) (\n0 iHn m0 ->
    case m0 of {
     O -> Right;
     S m1 -> sumbool_rec (\_ -> Left) (\_ -> Right) (iHn m1)}) n

beq_nat :: Nat -> Nat -> Bool
beq_nat n m0 =
  case n of {
   O ->
    case m0 of {
     O -> True;
     S n0 -> False};
   S n1 ->
    case m0 of {
     O -> False;
     S m1 -> beq_nat n1 m1}}

div5 :: Nat -> Nat
div5 n =
  case n of {
   O -> O;
   S n0 ->
    case n0 of {
     O -> O;
     S n' -> S (div5 n')}}

double1 :: Nat -> Nat
double1 n =
  plus n n

leb2 :: Nat -> Nat -> Bool
leb2 n m0 =
  case n of {
   O -> True;
   S n' ->
    case m0 of {
     O -> False;
     S m'0 -> leb2 n' m'0}}

ltb2 :: Nat -> Nat -> Bool
ltb2 n m0 =
  leb2 (S n) m0

pow2 :: Nat -> Nat -> Nat
pow2 n m0 =
  case m0 of {
   O -> S O;
   S m1 -> mult n (pow2 n m1)}

square2 :: Nat -> Nat
square2 n =
  mult n n

even1 :: Nat -> Bool
even1 n =
  case n of {
   O -> True;
   S n0 ->
    case n0 of {
     O -> False;
     S n' -> even1 n'}}

odd1 :: Nat -> Bool
odd1 n =
  negb (even1 n)

divmod :: Nat -> Nat -> Nat -> Nat -> Prod Nat Nat
divmod x y q u =
  case x of {
   O -> Pair q u;
   S x' ->
    case u of {
     O -> divmod x' y (S q) y;
     S u' -> divmod x' y q u'}}

div6 :: Nat -> Nat -> Nat
div6 x y =
  case y of {
   O -> y;
   S y' -> fst (divmod x y' O y')}

modulo2 :: Nat -> Nat -> Nat
modulo2 x y =
  case y of {
   O -> y;
   S y' -> minus y' (snd (divmod x y' O y'))}

sqrt_iter :: Nat -> Nat -> Nat -> Nat -> Nat
sqrt_iter k p q r =
  case k of {
   O -> p;
   S k' ->
    case r of {
     O -> sqrt_iter k' (S p) (S (S q)) (S (S q));
     S r' -> sqrt_iter k' p q r'}}

sqrt2 :: Nat -> Nat
sqrt2 n =
  sqrt_iter n O O O

log2_iter :: Nat -> Nat -> Nat -> Nat -> Nat
log2_iter k p q r =
  case k of {
   O -> p;
   S k' ->
    case r of {
     O -> log2_iter k' (S p) (S q) q;
     S r' -> log2_iter k' p (S q) r'}}

log1 :: Nat -> Nat
log1 n =
  log2_iter (pred n) O (S O) O

gcd2 :: Nat -> Nat -> Nat
gcd2 a b =
  case a of {
   O -> b;
   S a' -> gcd2 (modulo2 b (S a')) (S a')}

testbit2 :: Nat -> Nat -> Bool
testbit2 a n =
  case n of {
   O -> odd1 a;
   S n0 -> testbit2 (div5 a) n0}

shiftl2 :: Nat -> Nat -> Nat
shiftl2 a n =
  nat_iter n double1 a

shiftr2 :: Nat -> Nat -> Nat
shiftr2 a n =
  nat_iter n div5 a

bitwise :: (Bool -> Bool -> Bool) -> Nat -> Nat -> Nat -> Nat
bitwise op n a b =
  case n of {
   O -> O;
   S n' ->
    plus
      (case op (odd1 a) (odd1 b) of {
        True -> S O;
        False -> O}) (mult (S (S O)) (bitwise op n' (div5 a) (div5 b)))}

land2 :: Nat -> Nat -> Nat
land2 a b =
  bitwise andb a a b

lor2 :: Nat -> Nat -> Nat
lor2 a b =
  bitwise orb (max a b) a b

ldiff2 :: Nat -> Nat -> Nat
ldiff2 a b =
  bitwise (\b0 b' -> andb b0 (negb b')) a a b

lxor2 :: Nat -> Nat -> Nat
lxor2 a b =
  bitwise xorb (max a b) a b

recursion0 :: a1 -> (Nat -> a1 -> a1) -> Nat -> a1
recursion0 =
  nat_rect

type T2 = Nat

eqb2 :: Nat -> Nat -> Bool
eqb2 =
  beq_nat

compare2 :: Nat -> Nat -> Comparison
compare2 =
  nat_compare

zero1 :: Nat
zero1 =
  O

one1 :: Nat
one1 =
  S O

two1 :: Nat
two1 =
  S (S O)

succ2 :: Nat -> Nat
succ2 x =
  S x

pred3 :: Nat -> Nat
pred3 =
  pred

add2 :: Nat -> Nat -> Nat
add2 =
  plus

sub2 :: Nat -> Nat -> Nat
sub2 =
  minus

mul2 :: Nat -> Nat -> Nat
mul2 =
  mult

ltb3 :: Nat -> Nat -> Bool
ltb3 =
  ltb2

leb3 :: Nat -> Nat -> Bool
leb3 =
  leb2

min3 :: Nat -> Nat -> Nat
min3 =
  min

max3 :: Nat -> Nat -> Nat
max3 =
  max

eq_dec2 :: Nat -> Nat -> Sumbool
eq_dec2 =
  eq_nat_dec

even2 :: Nat -> Bool
even2 =
  even1

odd2 :: Nat -> Bool
odd2 =
  odd1

pow3 :: Nat -> Nat -> Nat
pow3 =
  pow2

square3 :: Nat -> Nat
square3 =
  square2

log3 :: Nat -> Nat
log3 =
  log1

sqrt3 :: Nat -> Nat
sqrt3 =
  sqrt2

div7 :: Nat -> Nat -> Nat
div7 =
  div6

modulo3 :: Nat -> Nat -> Nat
modulo3 =
  modulo2

gcd3 :: Nat -> Nat -> Nat
gcd3 =
  gcd2

testbit3 :: Nat -> Nat -> Bool
testbit3 =
  testbit2

shiftl3 :: Nat -> Nat -> Nat
shiftl3 =
  shiftl2

shiftr3 :: Nat -> Nat -> Nat
shiftr3 =
  shiftr2

lxor3 :: Nat -> Nat -> Nat
lxor3 =
  lxor2

land3 :: Nat -> Nat -> Nat
land3 =
  land2

lor3 :: Nat -> Nat -> Nat
lor3 =
  lor2

ldiff3 :: Nat -> Nat -> Nat
ldiff3 =
  ldiff2

div8 :: Nat -> Nat
div8 =
  div5

sqrt_up1 :: Nat -> Nat
sqrt_up1 a =
  case nat_compare O a of {
   Lt -> S (sqrt2 (pred a));
   _ -> O}

log2_up1 :: Nat -> Nat
log2_up1 a =
  case nat_compare (S O) a of {
   Lt -> S (log1 (pred a));
   _ -> O}

lcm1 :: Nat -> Nat -> Nat
lcm1 a b =
  mult a (div6 b (gcd2 a b))

eqb_spec2 :: Nat -> Nat -> Reflect
eqb_spec2 x y =
  iff_reflect (beq_nat x y)

b2n0 :: Bool -> Nat
b2n0 b =
  case b of {
   True -> S O;
   False -> O}

setbit1 :: Nat -> Nat -> Nat
setbit1 a n =
  lor2 a (shiftl2 (S O) n)

clearbit1 :: Nat -> Nat -> Nat
clearbit1 a n =
  ldiff2 a (shiftl2 (S O) n)

ones1 :: Nat -> Nat
ones1 n =
  pred (shiftl2 (S O) n)

lnot1 :: Nat -> Nat -> Nat
lnot1 a n =
  lxor2 a (ones1 n)

max_case_strong5 :: Nat -> Nat -> (Nat -> Nat -> () -> a1 -> a1) -> (() ->
                    a1) -> (() -> a1) -> a1
max_case_strong5 n m0 compat hl hr =
  let {c = compSpec2Type n m0 (nat_compare n m0)} in
  case c of {
   CompGtT -> compat n (max n m0) __ (hl __);
   _ -> compat m0 (max n m0) __ (hr __)}

max_case5 :: Nat -> Nat -> (Nat -> Nat -> () -> a1 -> a1) -> a1 -> a1 -> a1
max_case5 n m0 x x0 x1 =
  max_case_strong5 n m0 x (\_ -> x0) (\_ -> x1)

max_dec5 :: Nat -> Nat -> Sumbool
max_dec5 n m0 =
  max_case5 n m0 (\x y _ h0 -> h0) Left Right

min_case_strong5 :: Nat -> Nat -> (Nat -> Nat -> () -> a1 -> a1) -> (() ->
                    a1) -> (() -> a1) -> a1
min_case_strong5 n m0 compat hl hr =
  let {c = compSpec2Type n m0 (nat_compare n m0)} in
  case c of {
   CompGtT -> compat m0 (min n m0) __ (hr __);
   _ -> compat n (min n m0) __ (hl __)}

min_case5 :: Nat -> Nat -> (Nat -> Nat -> () -> a1 -> a1) -> a1 -> a1 -> a1
min_case5 n m0 x x0 x1 =
  min_case_strong5 n m0 x (\_ -> x0) (\_ -> x1)

min_dec5 :: Nat -> Nat -> Sumbool
min_dec5 n m0 =
  min_case5 n m0 (\x y _ h0 -> h0) Left Right

max_case_strong6 :: Nat -> Nat -> (() -> a1) -> (() -> a1) -> a1
max_case_strong6 n m0 x x0 =
  max_case_strong5 n m0 (\x1 y _ x2 -> eq_rect __ x2 __) x x0

max_case6 :: Nat -> Nat -> a1 -> a1 -> a1
max_case6 n m0 x x0 =
  max_case_strong6 n m0 (\_ -> x) (\_ -> x0)

max_dec6 :: Nat -> Nat -> Sumbool
max_dec6 =
  max_dec5

min_case_strong6 :: Nat -> Nat -> (() -> a1) -> (() -> a1) -> a1
min_case_strong6 n m0 x x0 =
  min_case_strong5 n m0 (\x1 y _ x2 -> eq_rect __ x2 __) x x0

min_case6 :: Nat -> Nat -> a1 -> a1 -> a1
min_case6 n m0 x x0 =
  min_case_strong6 n m0 (\_ -> x) (\_ -> x0)

min_dec6 :: Nat -> Nat -> Sumbool
min_dec6 =
  min_dec5

zeq_bool :: Z -> Z -> Bool
zeq_bool x y =
  case compare1 x y of {
   Eq -> True;
   _ -> False}

pow_pos0 :: (a1 -> a1 -> a1) -> a1 -> Positive -> a1
pow_pos0 rmul x i =
  case i of {
   XI i0 -> let {p = pow_pos0 rmul x i0} in rmul x (rmul p p);
   XO i0 -> let {p = pow_pos0 rmul x i0} in rmul p p;
   XH -> x}

rev :: (([]) a1) -> ([]) a1
rev l =
  case l of {
   [] -> [];
   (:) x l' -> app (rev l') ((:) x [])}

map :: (a1 -> a2) -> (([]) a1) -> ([]) a2
map f l =
  case l of {
   [] -> [];
   (:) a t -> (:) (f a) (map f t)}

fold_right :: (a2 -> a1 -> a1) -> a1 -> (([]) a2) -> a1
fold_right f a0 l =
  case l of {
   [] -> a0;
   (:) b t -> f b (fold_right f a0 t)}

split :: (([]) (Prod a1 a2)) -> Prod (([]) a1) (([]) a2)
split l =
  case l of {
   [] -> Pair [] [];
   (:) p tl ->
    case p of {
     Pair x y ->
      case split tl of {
       Pair g d -> Pair ((:) x g) ((:) y d)}}}

combine :: (([]) a1) -> (([]) a2) -> ([]) (Prod a1 a2)
combine l l' =
  case l of {
   [] -> [];
   (:) x tl ->
    case l' of {
     [] -> [];
     (:) y tl' -> (:) (Pair x y) (combine tl tl')}}

list_prod :: (([]) a1) -> (([]) a2) -> ([]) (Prod a1 a2)
list_prod l l' =
  case l of {
   [] -> [];
   (:) x t -> app (map (\y -> Pair x y) l') (list_prod t l')}

data Q =
   Qmake Z Positive

qnum :: Q -> Z
qnum q =
  case q of {
   Qmake qnum0 qden0 -> qnum0}

qden :: Q -> Positive
qden q =
  case q of {
   Qmake qnum0 qden0 -> qden0}

qcompare :: Q -> Q -> Comparison
qcompare p q =
  compare1 (mul1 (qnum p) (Zpos (qden q))) (mul1 (qnum q) (Zpos (qden p)))

qeq_bool :: Q -> Q -> Bool
qeq_bool x y =
  zeq_bool (mul1 (qnum x) (Zpos (qden y))) (mul1 (qnum y) (Zpos (qden x)))

qplus :: Q -> Q -> Q
qplus x y =
  Qmake
    (add1 (mul1 (qnum x) (Zpos (qden y))) (mul1 (qnum y) (Zpos (qden x))))
    (mul (qden x) (qden y))

qmult :: Q -> Q -> Q
qmult x y =
  Qmake (mul1 (qnum x) (qnum y)) (mul (qden x) (qden y))

qopp :: Q -> Q
qopp x =
  Qmake (opp (qnum x)) (qden x)

qminus :: Q -> Q -> Q
qminus x y =
  qplus x (qopp y)

qinv :: Q -> Q
qinv x =
  case qnum x of {
   Z0 -> Qmake Z0 XH;
   Zpos p -> Qmake (Zpos (qden x)) p;
   Zneg p -> Qmake (Zneg (qden x)) p}

qdiv :: Q -> Q -> Q
qdiv x y =
  qmult x (qinv y)

qpower_positive :: Q -> Positive -> Q
qpower_positive q p =
  pow_pos0 qmult q p

qpower :: Q -> Z -> Q
qpower q z =
  case z of {
   Z0 -> Qmake (Zpos XH) XH;
   Zpos p -> qpower_positive q p;
   Zneg p -> qinv (qpower_positive q p)}

qred :: Q -> Q
qred q =
  case q of {
   Qmake q1 q2 ->
    case snd (ggcd1 q1 (Zpos q2)) of {
     Pair r1 r2 -> Qmake r1 (to_pos r2)}}

qfloor :: Q -> Z
qfloor x =
  case x of {
   Qmake n d -> div1 n (Zpos d)}

qceiling :: Q -> Z
qceiling x =
  opp (qfloor (qopp x))

data Monad m =
   Build_Monad (() -> () -> m) (() -> () -> m -> (() -> m) -> m)

ret :: (Monad a1) -> a2 -> a1
ret monad x =
  case monad of {
   Build_Monad ret0 bind0 -> unsafeCoerce ret0 __ x}

bind :: (Monad a1) -> a1 -> (a2 -> a1) -> a1
bind monad x x0 =
  case monad of {
   Build_Monad ret0 bind0 -> unsafeCoerce bind0 __ __ x x0}

data PMonad m =
   Build_PMonad (() -> () -> () -> m) (() -> () -> () -> m -> (() -> m) -> m)

type MonP m x = ()

pbind :: (PMonad a1) -> (MonP a1 a3) -> a1 -> (a2 -> a1) -> a1
pbind pMonad pu x x0 =
  case pMonad of {
   Build_PMonad pret pbind0 -> unsafeCoerce pbind0 __ __ pu x x0}

pMonad_Monad :: (Monad a1) -> PMonad a1
pMonad_Monad m0 =
  Build_PMonad (\_ _ x -> ret m0 x) (\_ _ _ c f -> bind m0 c f)

monad_option :: Monad (Option ())
monad_option =
  Build_Monad (unsafeCoerce (\_ x -> Some x)) (\_ _ c1 c2 ->
    case c1 of {
     Some v -> c2 v;
     None -> None})

z_lg2' :: Q -> Z -> Nat -> Option Z
z_lg2' p n fuel =
  case fuel of {
   O -> None;
   S fuel' ->
    case eqb1 (qfloor p) Z0 of {
     True ->
      case qcompare (qpower (Qmake (Zpos (XO XH)) XH) n) p of {
       Eq -> Some (add1 n (Zpos XH));
       Lt -> Some n;
       Gt -> z_lg2' p (sub1 n (Zpos XH)) fuel'};
     False ->
      case compare1 (pow1 (Zpos (XO XH)) n) (qfloor p) of {
       Eq -> Some n;
       Lt -> z_lg2' p (add1 n (Zpos XH)) fuel';
       Gt -> Some (sub1 n (Zpos XH))}}}

z_lg2 :: Q -> Option Z
z_lg2 q =
  z_lg2' q Z0 (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S
    O))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

y_0' :: Z -> Q -> Q
y_0' n x =
  qmult (qpower (Qmake (Zpos (XO XH)) XH) (opp n)) x

y_0 :: Q -> Option (Prod Z Q)
y_0 x =
  pbind (pMonad_Monad (unsafeCoerce monad_option)) __
    (unsafeCoerce (z_lg2 x)) (\l2 ->
    ret (unsafeCoerce monad_option) (Pair l2 (y_0' l2 x)))

qge_bool :: Q -> Q -> Bool
qge_bool x y =
  geb (mul1 (qnum x) (Zpos (qden y))) (mul1 (qnum y) (Zpos (qden x)))

card_P :: Positive -> Positive
card_P p =
  case p of {
   XI p' -> add (card_P p') XH;
   XO p' -> add (card_P p') XH;
   XH -> XH}

truncate_P_by :: Positive -> Positive -> Positive
truncate_P_by =
  peano_rec (\d -> d) (\p r d ->
    case d of {
     XI d' -> r d';
     XO d' -> r d';
     XH -> XH})

truncate_Q_by :: Q -> Positive -> Q
truncate_Q_by q toRemove =
  Qmake
    (case qnum q of {
      Z0 -> Z0;
      Zpos p -> Zpos (truncate_P_by toRemove p);
      Zneg p -> Zneg (truncate_P_by toRemove p)})
    (truncate_P_by toRemove (qden q))

divide_Q :: Q -> Positive -> Q
divide_Q q plength =
  let {
   numpos = case qnum q of {
             Z0 -> XH;
             Zpos nump -> nump;
             Zneg nump -> nump}}
  in
  let {numlength = card_P numpos} in
  let {denlength = card_P (qden q)} in
  let {toRemove = sub (max0 numlength denlength) plength} in
  truncate_Q_by q toRemove

m' :: Q -> Z -> Nat -> Prod Z Q
m' y mr fuel =
  case fuel of {
   O -> Pair mr y;
   S fuel' ->
    case qeq_bool y (Qmake (Zpos XH) XH) of {
     True -> Pair Z0 y;
     False ->
      case qge_bool y (Qmake (Zpos (XO XH)) XH) of {
       True -> Pair mr y;
       False ->
        m'
          (divide_Q (qred (qpower y (Zpos (XO XH)))) (XO (XO (XO (XO (XO (XO
            XH))))))) (add1 mr (Zpos XH)) fuel'}}}

m :: Q -> Prod Z Q
m y =
  m' y Z0 (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S
    O))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

populate_ms :: Q -> Nat -> ([]) Z
populate_ms y n =
  case n of {
   O -> [];
   S n' ->
    case m y of {
     Pair m_n y' -> (:) m_n
      (populate_ms (qred (qdiv y' (Qmake (Zpos (XO XH)) XH))) n')}}

log2' :: Nat -> (([]) Z) -> Z -> Q
log2' n ms msum =
  case n of {
   O -> Qmake Z0 XH;
   S n' ->
    case ms of {
     [] -> Qmake Z0 XH;
     (:) m0 ms' ->
      case m0 of {
       Zpos p ->
        case msum of {
         Z0 ->
          qplus (qred (Qmake (Zpos XH) (pow (XO XH) p)))
            (qred (log2' n' ms' (Zpos p)));
         Zpos msumpos ->
          let {msum' = add p msumpos} in
          qplus (qred (Qmake (Zpos XH) (pow (XO XH) msum')))
            (qred (log2' n' ms' (Zpos msum')));
         Zneg p0 -> Qmake Z0 XH};
       _ -> Qmake Z0 XH}}}

log4 :: Q -> Nat -> Option Q
log4 q c =
  pbind (pMonad_Monad (unsafeCoerce monad_option)) __ (unsafeCoerce (y_0 q))
    (\y_0r ->
    case y_0r of {
     Pair n y0 ->
      let {ms = populate_ms y0 c} in
      ret (unsafeCoerce monad_option)
        (qplus (qred (log2' c ms Z0)) (Qmake n XH))})

lg2e :: Q
lg2e =
  Qmake (Zpos (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO
    (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO
    (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO
    (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO
    (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO
    (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO
    (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XO (XI (XO (XI (XI (XO
    (XI (XI (XO (XO (XI (XO (XO (XO (XO (XO (XO (XO (XI (XO (XO (XO (XO (XO
    (XI (XI (XO (XI (XO (XO (XI (XO (XO (XI (XI (XO (XI (XO (XI (XI (XO (XO
    (XI (XO (XI (XI (XI (XO (XI (XI (XI (XI (XI (XO (XO (XI (XO (XO (XI (XI
    (XO (XI (XO (XI (XO (XI (XO (XO (XO (XI (XO (XO (XO (XI (XO (XI (XO (XI
    (XO (XI (XO (XO (XI (XI (XO (XO (XO (XO (XI (XI (XO (XO (XO (XO (XO (XI
    (XO (XI (XO (XO (XO (XO (XO (XO (XO (XO (XI (XI (XI (XI (XI (XO (XO (XI
    (XO (XI (XI (XI (XI (XO (XI (XO (XO (XO (XI (XO (XI (XO (XI (XO (XI (XO
    (XO (XI (XO (XO (XO (XO (XO (XO (XO (XO (XI (XI (XO (XO (XI (XI (XO (XO
    (XO (XI (XO (XI (XI (XI (XI (XI (XO (XI (XO (XI (XI (XO (XO (XI (XI (XO
    (XI (XI (XI (XO (XI (XI (XO (XO (XI (XI (XI (XO (XI (XI (XI (XO (XI (XI
    (XI (XI (XI (XI (XI (XI (XI (XO (XO (XI (XI (XI (XI (XI (XI (XO (XO (XI
    (XI (XO (XO (XO (XO (XI (XO (XI (XO (XO (XO (XI (XI (XI (XI (XI (XI (XO
    (XO (XI (XO (XI (XI (XO (XI (XI (XI (XO (XI (XI (XO (XO (XI (XO (XI (XO
    (XO (XO (XO (XO (XO (XO (XO (XO (XI (XO (XO (XO (XI (XI (XI (XO (XO (XO
    (XI (XO (XO (XO (XI (XI (XI (XI (XI (XO (XO (XI (XO (XI (XI (XI (XI (XO
    (XO
    XH))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    (XI (XO (XO (XI (XI (XO (XO (XO (XI (XO (XO (XI (XI (XI (XI (XI (XO (XO
    (XO (XO (XO (XO (XI (XO (XO (XO (XO (XI (XO (XI (XO (XI (XO (XO (XI (XI
    (XO (XI (XI (XI (XI (XO (XI (XO (XI (XO (XI (XO (XI (XO (XI (XO (XO (XI
    (XO (XI (XO (XO (XI (XI (XO (XO (XI (XO (XO (XO (XO (XO (XI (XI (XI (XI
    (XO (XI (XI (XI (XI (XO (XO (XI (XO (XO (XO (XI (XI (XO (XI (XI (XI (XO
    (XO (XI (XO (XI (XI (XI (XO (XI (XI (XO (XI (XI (XO (XO (XI (XI (XI (XO
    (XI (XO (XO (XO (XO (XO (XO (XI (XI (XI (XI (XI (XI (XI (XI (XO (XO (XO
    (XO (XI (XO (XO (XO (XO (XI (XO (XI (XO (XO (XO (XO (XO (XI (XO (XI (XI
    (XO (XI (XI (XO (XO (XO (XO (XO (XO (XI (XO (XI (XI (XI (XO (XI (XI (XI
    (XO (XO (XO (XO (XI (XI (XO (XI (XI (XO (XI (XI (XO (XI (XI (XO (XO (XO
    (XO (XO (XI (XI (XI (XO (XI (XI (XI (XO (XI (XI (XI (XO (XI (XO (XO (XO
    (XI (XI (XO (XO (XI (XO (XI (XI (XO (XI (XI (XI (XO (XI (XI (XO (XO (XI
    (XI (XO (XI (XO (XO (XI (XI (XI (XI (XO (XI (XI (XO (XI (XI (XO (XI (XI
    (XI (XO (XI (XO (XO (XI (XI (XO (XI (XI (XO (XO (XO (XI (XI (XO (XO (XO
    (XO (XO (XO (XI (XO (XO (XO (XO (XI (XO (XI (XI (XI (XI (XO (XO (XO (XO
    (XO (XI (XI (XO (XI (XO (XI (XO (XI (XO (XO (XI (XI (XO (XO (XO (XO (XI
    (XO (XI (XO (XI (XO (XO (XI (XI (XO (XO (XI (XO (XI (XI (XO (XO (XO (XO
    (XI (XO (XO (XO (XO (XI (XI (XI (XI (XO (XI (XO (XO (XO (XO (XO (XI (XO
    (XI (XI (XO (XO (XI (XO (XI (XI (XO (XI (XO (XI (XO (XO (XO (XI (XO (XI
    (XI (XI (XO (XI (XO (XI (XI (XO (XI (XO (XI (XI (XI (XO (XI (XO (XI (XI
    (XO (XI (XI (XI (XI (XO (XI (XI (XI (XO (XO (XO (XO (XI (XI (XI (XI (XO
    (XO (XI (XI (XI (XI (XO (XI (XO (XI (XI (XO (XO (XI (XO (XO (XI (XI
    XH)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

lnc :: Q -> Nat -> Positive -> Option Q
lnc q c digs2 =
  pbind (pMonad_Monad (unsafeCoerce monad_option)) __ (log4 (qred q) c)
    (\r ->
    ret (unsafeCoerce monad_option) (divide_Q (qred (qmult lg2e r)) digs2))

ln :: Q -> Option Q
ln q =
  lnc q (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    O)))))))))))))))))))) (XO (XO (XO (XO (XO (XO XH))))))

n_to_Q :: N -> Q
n_to_Q n =
  Qmake (of_N n) XH

findASN' :: Q -> Q -> Q -> Q -> Option
            (Prod (Prod (Prod (Prod (Prod (Prod Q Q) Q) Q) Q) Q) Z)
findASN' ballots vw vl risk =
  case qge_bool vw vl of {
   True ->
    let {sw = qdiv vw (qplus vw vl)} in
    pbind (pMonad_Monad (unsafeCoerce monad_option)) __
      (unsafeCoerce (ln (qmult (Qmake (Zpos (XO XH)) XH) sw))) (\zw ->
      pbind (pMonad_Monad (unsafeCoerce monad_option)) __
        (unsafeCoerce
          (ln
            (qmult (Qmake (Zpos (XO XH)) XH)
              (qminus (Qmake (Zpos XH) XH) sw)))) (\zl ->
        let {pw = qdiv vw ballots} in
        let {pl = qdiv vl ballots} in
        pbind (pMonad_Monad (unsafeCoerce monad_option)) __
          (unsafeCoerce (ln (qdiv (Qmake (Zpos XH) XH) risk))) (\lr ->
          ret (unsafeCoerce monad_option) (Pair (Pair (Pair (Pair (Pair (Pair
            sw zw) zl) pw) pl) lr)
            (qceiling
              (qdiv (qplus lr (qdiv zw (Qmake (Zpos (XO XH)) XH)))
                (qmult (qdiv (qplus vw vl) ballots)
                  (qplus (qmult pw zw) (qmult pl zl)))))))));
   False -> None}

findASN :: Q -> N -> N -> Q -> Option Z
findASN ballots vw vl risk =
  pbind (pMonad_Monad (unsafeCoerce monad_option)) __
    (unsafeCoerce (findASN' ballots (n_to_Q vw) (n_to_Q vl) risk)) (\res ->
    ret (unsafeCoerce monad_option) (snd res))

ble_nat :: Nat -> Nat -> Bool
ble_nat a b =
  case nat_compare a b of {
   Gt -> False;
   _ -> True}

votes_contest :: (([]) N) -> N
votes_contest contest =
  fold_right add0 N0 contest

contests_ok :: (([]) (([]) N)) -> (([]) Nat) -> N -> Bool
contests_ok contests no_winners ballots =
  case contests of {
   [] ->
    case no_winners of {
     [] -> True;
     (:) n l -> False};
   (:) contest contests' ->
    case no_winners of {
     [] -> False;
     (:) no_winner no_winners' ->
      case andb (ble_nat no_winner (length contest))
             (leb0 (votes_contest contest)
               (mul0 (of_nat0 no_winner) ballots)) of {
       True -> contests_ok contests' no_winners' ballots;
       False -> False}}}

merge :: (([]) N) -> (([]) N) -> ([]) N
merge l1 l2 =
  let {
   merge_aux l3 =
     case l1 of {
      [] -> l3;
      (:) a1 l1' ->
       case l3 of {
        [] -> l1;
        (:) a2 l2' ->
         case leb0 a1 a2 of {
          True -> (:) a1 (merge l1' l3);
          False -> (:) a2 (merge_aux l2')}}}}
  in merge_aux l2

merge_list_to_stack :: (([]) (Option (([]) N))) -> (([]) N) -> ([])
                       (Option (([]) N))
merge_list_to_stack stack l =
  case stack of {
   [] -> (:) (Some l) [];
   (:) y stack' ->
    case y of {
     Some l' -> (:) None (merge_list_to_stack stack' (merge l' l));
     None -> (:) (Some l) stack'}}

merge_stack :: (([]) (Option (([]) N))) -> ([]) N
merge_stack stack =
  case stack of {
   [] -> [];
   (:) y stack' ->
    case y of {
     Some l -> merge l (merge_stack stack');
     None -> merge_stack stack'}}

iter_merge :: (([]) (Option (([]) N))) -> (([]) N) -> ([]) N
iter_merge stack l =
  case l of {
   [] -> merge_stack stack;
   (:) a l' -> iter_merge (merge_list_to_stack stack ((:) a [])) l'}

sort :: (([]) N) -> ([]) N
sort =
  iter_merge []

flatten_stack :: (([]) (Option (([]) N))) -> ([]) N
flatten_stack stack =
  case stack of {
   [] -> [];
   (:) o stack' ->
    case o of {
     Some l -> app l (flatten_stack stack');
     None -> flatten_stack stack'}}

findMargin' :: (([]) N) -> Nat -> N
findMargin' contest index =
  case index of {
   O ->
    case contest of {
     [] -> N0;
     (:) lastwin l ->
      case l of {
       [] -> lastwin;
       (:) firstlose l0 -> sub0 lastwin firstlose}};
   S n' -> findMargin' contest n'}

findMargin :: (Prod (([]) N) Nat) -> N
findMargin contest_no_winners =
  case contest_no_winners of {
   Pair contest no_winners -> findMargin' contest (pred3 no_winners)}

split_after_index'' :: (([]) a1) -> Nat -> Prod (([]) a1) (([]) a1)
split_after_index'' l n =
  case n of {
   O -> Pair [] l;
   S n' ->
    case l of {
     [] -> Pair [] [];
     (:) h t ->
      let {res = split_after_index'' t n'} in
      Pair ((:) h (fst res)) (snd res)}}

split_after_index :: (([]) a1) -> Nat -> Prod (([]) a1) (([]) a1)
split_after_index l n =
  split_after_index'' l n

split_after_index_pair :: (Prod (([]) a1) Nat) -> Prod (([]) a1) (([]) a1)
split_after_index_pair i =
  case i of {
   Pair l n -> split_after_index l n}

findASN_pair :: Q -> Q -> (Prod N N) -> Option Z
findASN_pair ballots risk v =
  case v of {
   Pair wv lv -> findASN ballots wv lv risk}

maxASN_contest :: Q -> Q -> (Prod (([]) N) (([]) N)) -> Option Z
maxASN_contest ballots risk wl =
  case wl of {
   Pair winners losers ->
    fold_right (\x y ->
      pbind (pMonad_Monad (unsafeCoerce monad_option)) __ x (\r ->
        pbind (pMonad_Monad (unsafeCoerce monad_option)) __ y (\r2 ->
          ret (unsafeCoerce monad_option) (max2 r r2)))) (Some Z0)
      (map (findASN_pair ballots risk) (list_prod winners losers))}

maxASN_all_contests :: (([]) (([]) N)) -> (([]) (([]) N)) -> Q -> Q -> Option
                       Z
maxASN_all_contests all_winners all_losers ballots risk =
  fold_right (\x y ->
    pbind (pMonad_Monad (unsafeCoerce monad_option)) __ x (\r ->
      pbind (pMonad_Monad (unsafeCoerce monad_option)) __ y (\r2 ->
        ret (unsafeCoerce monad_option) (max2 r r2)))) (Some Z0)
    (map (maxASN_contest ballots risk) (combine all_winners all_losers))

findMinMargin :: Positive -> (([]) (([]) N)) -> Q -> (([]) Nat) -> Prod
                 (Prod (Prod (([]) (([]) N)) N) Q) (Option Z)
findMinMargin ballots contests risk no_winners =
  case negb (contests_ok contests no_winners (Npos ballots)) of {
   True -> Pair (Pair (Pair [] N0) (Qmake Z0 XH)) None;
   False ->
    let {sorted_contests = map (compose rev sort) contests} in
    let {margins = map findMargin (combine sorted_contests no_winners)} in
    let {min_margin = fold_right min1 (Npos ballots) margins} in
    let {diluted_margin = Qmake (of_N min_margin) ballots} in
    case split
           (map split_after_index_pair (combine sorted_contests no_winners)) of {
     Pair winners losers -> Pair (Pair (Pair sorted_contests min_margin)
      diluted_margin)
      (maxASN_all_contests winners losers (Qmake (Zpos ballots) XH) risk)}}

