# Copilot Kind

Highly automated proof techniques are a necessary step for the widespread adoption of formal methods in the software industry. Moreover, it could provide a partial answer to one of its main issue which is scalability.

* A solver implementing basic k-induction model checking, useful for proving basic inductive properties and for pedagogical purposes.

* A solver producing native inputs for the *Kind2* model checker, developed at University of Iowa. The latter uses both a *k-induction* algorithm extended with *path compression* and *structural abstraction* [ref] and the IC3 algorithm with counterexample generalization based on approximate quantifier elimination [ref].


## A Tutorial

### Installation instructions

*copilot-kind* needs the following dependencies to run :

* The *copilot-core* and *copilot-language* Haskell libraries
* The *Yices2* SMT-solver : `yices-smt2` must be in your `$PATH`
* The *Kind2* model checker : `kind2` must be in your `$PATH`

To build it, just clone this repository and use `cabal install`.

### First steps

*copilot-kind* is aimed at checking **safety properties** on Copilot programs. Intuitively, a safety property is a property which express the idea that *nothing bad can happen*. In particular, any invalid safety property can be disproved by a finite execution trace of the program called a *counterexample*. Safety properties are often opposed to **liveness** properties, which express the idea that *something good will eventually happen*. The latters are out of the scope of *copilot-kind*.

Safety properties are simply expressed with standard boolean streams. In addition to triggers and observers declarations, it is possible to bind a boolean stream to a property name with the `prop` construct in the specification.

For instance, here is a straightforward specification declaring one property :


```haskell
spec :: Spec
spec = do
  prop "gt0" (x > 0)
  where
    x = [1] ++ (1 + x)
```

Let's say you want to prove that 


Some simple examples are in the *examples* folder. 

In the `Driver.hs`



Any installation problems can be signaled to


## Technical details

# Recommended use


What kind of properties can I prove with *copilot-kind* ?




## Limitations of copilot-kind

### Limitations related to Copilot implementation

### Limitations related to Copilot implementation



II. Technical details
---------------------





III. Future work
----------------

A. Little convenience enhancements of copilot-kind

+ Implement a cleaner way to deal with arbitrary streams and arbitrary constants by


+ If inductive proving of a property fails, giving the user a concrete CTI (Counterexample To The Inductiveness)



















<step>  (set-logic QF_UFLIA)
<step>  (declare-fun n () Int)
<step>  (declare-fun s1 (Int) Int)
<step>  (declare-fun s3 (Int) Int)
<step>  (declare-fun s2 (Int) Int)
<step>  (declare-fun s4 (Int) Int)
<step>  (declare-fun s6 (Int) Int)
<step>  (declare-fun ext_in (Int) Int)
<step>  (declare-fun l7 (Int) Int)
<step>  (declare-fun l8 (Int) Int)
<step>  (assert (= (s1 (+ n 1)) (ite (= (ext_in (+ n 0)) 1) (+ (s1 (+ n 0)) 1) (s1 (+ n 0)))))
<step>  (assert (= (s3 (+ n 1)) (ite (or (= (s2 (+ n 0)) (ext_in (+ n 0))) (<= (s3 (+ n 0)) 0)) (+ (s3 (+ n 0)) 1) (- (s3 (+ n 0)) 1))))
<step>  (assert (= (s2 (+ n 1)) (ite (<= (s3 (+ n 0)) 0) (ext_in (+ n 0)) (s2 (+ n 0)))))
<step>  (assert (= (s4 (+ n 1)) (+ 1 (s4 (+ n 0)))))
<step>  (assert (= (s6 (+ n 1)) (ite (= (ext_in (+ n 0)) 2) (+ (s6 (+ n 0)) 1) (s6 (+ n 0)))))
<step>  (assert (= (l8 (+ n 0)) (s6 (+ n 0))))
<step>  (assert (= (l7 (+ n 0)) (s1 (+ n 0))))
<step>  (assert (or (= (ext_in (+ n 0)) 1) (= (ext_in (+ n 0)) 2)))
<step>  (assert (= (s1 (+ n 2)) (ite (= (ext_in (+ n 1)) 1) (+ (s1 (+ n 1)) 1) (s1 (+ n 1)))))
<step>  (assert (= (s3 (+ n 2)) (ite (or (= (s2 (+ n 1)) (ext_in (+ n 1))) (<= (s3 (+ n 1)) 0)) (+ (s3 (+ n 1)) 1) (- (s3 (+ n 1)) 1))))
<step>  (assert (= (s2 (+ n 2)) (ite (<= (s3 (+ n 1)) 0) (ext_in (+ n 1)) (s2 (+ n 1)))))
<step>  (assert (= (s4 (+ n 2)) (+ 1 (s4 (+ n 1)))))
<step>  (assert (= (s6 (+ n 2)) (ite (= (ext_in (+ n 1)) 2) (+ (s6 (+ n 1)) 1) (s6 (+ n 1)))))
<step>  (assert (= (l8 (+ n 1)) (s6 (+ n 1))))
<step>  (assert (= (l7 (+ n 1)) (s1 (+ n 1))))
<step>  (assert (or (= (ext_in (+ n 1)) 1) (= (ext_in (+ n 1)) 2)))
<step>  (assert (and (and (or (not (not (= 1 (s2 (+ n 0))))) (<= (+ (s3 (+ n 0)) (* 2 (l7 (+ n 0)))) (s4 (+ n 0)))) (or (not (= 1 (s2 (+ n 0)))) (<= (* 2 (l7 (+ n 0))) (+ (s3 (+ n 0)) (s4 (+ n 0)))))) (and (or (not (not (= 2 (s2 (+ n 0))))) (<= (+ (s3 (+ n 0)) (* 2 (l8 (+ n 0)))) (s4 (+ n 0)))) (or (not (= 2 (s2 (+ n 0)))) (<= (* 2 (l8 (+ n 0))) (+ (s3 (+ n 0)) (s4 (+ n 0))))))))
<step>  (push 1)
<step>  (assert (or false (not (and (and (or (not (not (= 1 (s2 (+ n 1))))) (<= (+ (s3 (+ n 1)) (* 2 (l7 (+ n 1)))) (s4 (+ n 1)))) (or (not (= 1 (s2 (+ n 1)))) (<= (* 2 (l7 (+ n 1))) (+ (s3 (+ n 1)) (s4 (+ n 1)))))) (and (or (not (not (= 2 (s2 (+ n 1))))) (<= (+ (s3 (+ n 1)) (* 2 (l8 (+ n 1)))) (s4 (+ n 1)))) (or (not (= 2 (s2 (+ n 1)))) (<= (* 2 (l8 (+ n 1))) (+ (s3 (+ n 1)) (s4 (+ n 1))))))))))
<step>  (check-sat)



