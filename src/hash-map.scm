;;; File: hash-map.scm
;;
;;; License:
;; Copyright © 2015 Remy Goldschmidt <taktoa@gmail.com>
;;
;; This file is part of ThomasEngine2.
;;
;; ThomasEngine2 is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; ThomasEngine2 is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with ThomasEngine. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Author:     Remy Goldschmidt <taktoa@gmail.com>
;;; Maintainer: Remy Goldschmidt <taktoa@gmail.com>
;;; Created:    August 25th, 2015
;;
;;; Homepage:   https://github.com/taktoa/ThomasEngine2
;;
;;; Commentary:
;; The <hash-map> class is a GOOPS wrapper over the hash tables provided
;; by the (ice-9 hash-table) module included in Guile.
;;
;;; Code:

(define-module (thomas utils hash-map)
  #:use-module (ice-9  hash-table)
  #:use-module (ice-9  format)
  #:use-module (ice-9  match)
  #:use-module (srfi   srfi-1)
  #:use-module (srfi   srfi-11)
  #:use-module (srfi   srfi-26)
  #:use-module (oop    goops)
  #:export     (<hash-map>
                ;; FIXME: add the rest
                ))

;;; Classes
(define-class <hash-map> ()
  (ht #:init-form (make-hash-table)
      #:getter    get-hash-table))

;;; Public methods
;; Get the property at the given position
(define-method (example-method (hash <hash-map>)) '())

;;; Public functions
;; Make a hash map
(define (make-hash-map) (make <hash-map>))

;;; Private functions
;; Increments a number by 1
(define (incr1 i) (+ i 1))


;;;;----------------------------------------------------------------------------
;;;;----------------------------------------------------------------------------
;;;;----------------------------------------------------------------------------
;;;;----------------------------------------------------------------------------
;;;;----------------------------------------------------------------------------
;;;;----------------------------------------------------------------------------

(define* (printf fmt #:rest args)
  (display (apply format (cons #f (cons fmt args)))))

(define* (printfln fmt #:rest args)
  (apply format (cons #t (cons (string-append fmt "~%") args))))

(define* (signature name type)
  (printfln "")
  (printfln "Signature:")
  (printfln "    Name: ~s" name)
  (printfln "    Type: ~s" type)
  (printfln ""))

(define* (def-container-func name formals doc body)
  (printfln "")
  (printfln "Container Function:")
  (printfln "    Name:      ~s" name)
  (printfln "    Formals:   ~s" formals)
  (printfln "    Body:      ~s" body)
  (printfln "    Docstring: ~a" doc)
  (printfln ""))

(define (process-type input)
  (let ([is-fat-arrow (λ (x) (or (eq? '⇒ x) (eq? '=> x)))])
    (if (any is-fat-arrow input)
        (let-values ([(pfx sfx) (break is-fat-arrow input)]) (cons pfx sfx))
        (cons '() input))))

(define-syntax do-nothing (λ (stx) #''()))

(define-syntax define-container-function
  (syntax-rules ()
    [(define-container-function (name formals ...) doc body ...)
     (def-container-func 'name '(formals ...) doc '(body ...))]))

(define-syntax sig
  (syntax-rules ()
    [(sig name ∷ type ...) (signature 'name (process-type '(type ...)))]))

(when (defined? 'setlocale)
  (catch 'system-error
    (λ [] (setlocale LC_ALL ""))
    (lambda* [#:rest args] (printf "failed to install locale: ~a"
                                   (strerror (system-error-errno args))))))

;;;;----------------------------------------------------------------------------

(sig adjust ∷ Ord k
            ⇒ (a → a)
            → k
            → Map k a
            → Map k a)
(define-container-function (adjust f k m)
  "O(log n). Update a value at a specific key with the result of the provided function.
When the key is not
a member of the map, the original map is returned.

> adjust (\"new \" ++) 5 (from-list [(5,\"a\"), (3,\"b\")]) == from-list [(3, \"b\"), (5, \"new a\")]
> adjust (\"new \" ++) 7 (from-list [(5,\"a\"), (3,\"b\")]) == from-list [(3, \"b\"), (5, \"a\")]
> adjust (\"new \" ++) 7 empty == empty"
  '())

(sig adjust-with-key ∷ Ord k
                     ⇒ (k → a → a)
                     → k
                     → Map k a
                     → Map k a)
(define-container-function (adjust-with-key f k m)
  "O(log n). Adjust a value at a specific key. When the key is not
a member of the map, the original map is returned."
  '())

(sig alter ∷ Ord k
           ⇒ (Maybe a → Maybe a)
           → k
           → Map k a
           → Map k a)
(define-container-function (alter f k m)
  "O(log n). The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
'alter' can be used to insert, delete, or update a value in a 'Map'.
In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.

> let f _ = Nothing
> alter f 7 (from-list [(5,\"a\"), (3,\"b\")]) == from-list [(3, \"b\"), (5, \"a\")]
> alter f 5 (from-list [(5,\"a\"), (3,\"b\")]) == singleton 3 \"b\"
>
> let f _ = Just \"c\"
> alter f 7 (from-list [(5,\"a\"), (3,\"b\")]) == from-list [(3, \"b\"), (5, \"a\"), (7, \"c\")]
> alter f 5 (from-list [(5,\"a\"), (3,\"b\")]) == from-list [(3, \"b\"), (5, \"c\")]"
  '())

(sig difference-with ∷ Ord k
                     ⇒ (a → b → Maybe a)
                     → Map k a
                     → Map k b
                     → Map k a)
(define-container-function (difference-with f m1 m2)
  "O(n+m). Difference with a combining function.
When two equal keys are
encountered, the combining function is applied to the values of these keys.
If it returns 'Nothing', the element is discarded (proper set difference). If
it returns (@'Just' y@), the element is updated with a new value @y@.
The implementation uses an efficient hedge algorithm comparable with hedge-union.

> let f al ar = if al == \"b\" then Just (al ++ \":\" ++ ar) else Nothing
> differenceWith f (from-list [(5, \"a\"), (3, \"b\")]) (from-list [(5, \"A\"), (3, \"B\"), (7, \"C\")]) == singleton 3 \"b:B\""
  '())

(sig difference-with-key ∷ Ord k
                         ⇒ (k → a → b → Maybe a)
                         → Map k a
                         → Map k b
                         → Map k a)
(define-container-function (difference-with-key f m1 m2)
  "O(n+m). Difference with a combining function. When two equal keys are
encountered, the combining function is applied to the key and both values.
If it returns 'Nothing', the element is discarded (proper set difference). If
it returns (@'Just' y@), the element is updated with a new value @y@.
The implementation uses an efficient hedge algorithm comparable with hedge-union.

> let f k al ar = if al == \"b\" then Just ((show k) ++ \":\" ++ al ++ \"|\" ++ ar) else Nothing
> differenceWithKey f (from-list [(5, \"a\"), (3, \"b\")]) (from-list [(5, \"A\"), (3, \"B\"), (10, \"C\")]) == singleton 3 \"3:b|B\""
  '())

(sig find-with-default ∷ Ord k
                       ⇒ a
                       → k
                       → Map k a
                       → a)
(do-nothing (find-with-default d k m)
  "O(log n). The expression @('find-with-default' def k map)@ returns the
value at key @k@ or returns default value @def@ when the key is not in the map.

> (find-with-default #\\x 1 (from-list (list (tup 5 #\\a) (tup 3 #\\b)))])) == #\\x
> (find-with-default #\\x 5 (from-list (list (tup 5 #\\a) (tup 3 #\\b)))) == #\\a"
  '())

(sig from-asc-list ∷ Eq k
                   ⇒ [(k, a)]
                   → Map k a)
(define-container-function (from-asc-list xs)
  "O(n). Build a map from an ascending list in linear time.
The precondition (input list is ascending) is not checked.

> fromAscList [(3,\"b\"), (5,\"a\")] == from-list [(3, \"b\"), (5, \"a\")]
> fromAscList [(3,\"b\"), (5,\"a\"), (5,\"b\")] == from-list [(3, \"b\"), (5, \"b\")]
> valid (fromAscList [(3,\"b\"), (5,\"a\"), (5,\"b\")]) == True
> valid (fromAscList [(5,\"a\"), (3,\"b\"), (5,\"b\")]) == False"
  '())

(sig from-asc-list-with ∷ Eq k
                        ⇒ (a → a → a)
                        → [(k, a)]
                        → Map k a)
(define-container-function (from-asc-list-with f xs)
  "O(n). Build a map from an ascending list in linear time with a combining function for equal keys.
The precondition (input list is ascending) is not checked.

> fromAscListWith (++) [(3,\"b\"), (5,\"a\"), (5,\"b\")] == from-list [(3, \"b\"), (5, \"ba\")]
> valid (fromAscListWith (++) [(3,\"b\"), (5,\"a\"), (5,\"b\")]) == True
> valid (fromAscListWith (++) [(5,\"a\"), (3,\"b\"), (5,\"b\")]) == False"
  '())

(sig from-asc-list-with-key ∷ Eq k
                            ⇒ (k → a → a → a)
                            → [(k, a)]
                            → Map k a)
(define-container-function (from-asc-list-with-key f xs)
  "O(n). Build a map from an ascending list in linear time with a
combining function for equal keys.
The precondition (input list is ascending) is not checked.

> let f k a1 a2 = (show k) ++ \":\" ++ a1 ++ a2
> fromAscListWithKey f [(3,\"b\"), (5,\"a\"), (5,\"b\"), (5,\"b\")] == from-list [(3, \"b\"), (5, \"5:b5:ba\")]
> valid (fromAscListWithKey f [(3,\"b\"), (5,\"a\"), (5,\"b\"), (5,\"b\")]) == True
> valid (fromAscListWithKey f [(5,\"a\"), (3,\"b\"), (5,\"b\"), (5,\"b\")]) == False"
  '())

(sig from-distinct-asc-list ∷ [(k, a)]
                            → Map k a)
(define-container-function (from-distinct-asc-list xs)
  "O(n). Build a map from an ascending list of distinct elements in linear time.
The precondition is not checked.

> fromDistinctAscList [(3,\"b\"), (5,\"a\")] == from-list [(3, \"b\"), (5, \"a\")]
> valid (fromDistinctAscList [(3,\"b\"), (5,\"a\")]) == True
> valid (fromDistinctAscList [(3,\"b\"), (5,\"a\"), (5,\"b\")]) == False

For some reason, when 'singleton' is used in fromDistinctAscList or in
create, it is not inlined, so we inline it manually."
  '())

(sig from-list ∷ Ord k
               ⇒ [(k, a)]
               → Map k a)
(define-container-function (from-list xs)
  "O(n*log n). Build a map from a list of key/value pairs. See also 'fromAscList'.
If the list contains more than one value for the same key, the last value
for the key is retained.

If the keys of the list are ordered, linear-time implementation is used,
with the performance equal to 'fromDistinctAscList'.

> from-list [] == empty
> from-list [(5,\"a\"), (3,\"b\"), (5, \"c\")] == from-list [(5,\"c\"), (3,\"b\")]
> from-list [(5,\"c\"), (3,\"b\"), (5, \"a\")] == from-list [(5,\"a\"), (3,\"b\")]

For some reason, when 'singleton' is used in from-list or in
create, it is not inlined, so we inline it manually."
  '())

(sig from-list-with ∷ Ord k
                    ⇒ (a → a → a)
                    → [(k, a)]
                    → Map k a)
(define-container-function (from-list-with f xs)
  "O(n*log n). Build a map from a list of key/value pairs with a combining function. See also 'fromAscListWith'.

> from-listWith (++) [(5,\"a\"), (5,\"b\"), (3,\"b\"), (3,\"a\"), (5,\"a\")] == from-list [(3, \"ab\"), (5, \"aba\")]
> from-listWith (++) [] == empty"
  '())

(sig from-list-with-key ∷ Ord k
                        ⇒ (k → a → a → a)
                        → [(k, a)]
                        → Map k a)
(define-container-function (from-list-with-key f xs)
  "O(n*log n). Build a map from a list of key/value pairs with a combining function. See also 'fromAscListWithKey'.

> let f k a1 a2 = (show k) ++ a1 ++ a2
> from-listWithKey f [(5,\"a\"), (5,\"b\"), (3,\"b\"), (3,\"a\"), (5,\"a\")] == from-list [(3, \"3ab\"), (5, \"5a5ba\")]
> from-listWithKey f [] == empty"
  '())

(sig from-set ∷ (k → a)
              → Set k
              → Map k a)
(define-container-function (from-set f set)
  "O(n). Build a map from a set of keys and a function which for each key
computes its value.

> fromSet (\\k -> replicate k 'a') (Data.Set.from-list [3, 5]) == from-list [(5,\"aaaaa\"), (3,\"aaa\")]
> fromSet undefined Data.Set.empty == empty"
  '())

(sig insert ∷ Ord k
            ⇒ k
            → a
            → Map k a
            → Map k a)
(do-nothing (insert k v m)
  "O(log n). Insert a new key and value in the map.
If the key is already present in the map, the associated value is replaced with
 a supplied value. 'insert' is equivalent to @(partial 'insert-with' 'const')@.

> (insert 5 #\\x (from-list (list (tup 5 #\\a), (tup 3 #\\b)])) == (from-list (list (tup 3 #\\b) (tup 5 #\\x)))
> (insert 7 #\\x (from-list (list (tup 5 #\\a), (tup 3 #\\b)])) == (from-list (list (tup 3 #\\b) (tup 5 #\\a) (tup 7 #\\x)))
> (insert 5 #\\x empty) == singleton 5 #\\x"
  '())

(sig insert-lookup-with-key ∷ Ord k
                            ⇒ (k → a → a → a)
                            → k
                            → a
                            → Map k a)
(define-container-function (insert-lookup-with-key f k v m)
  "O(log n). Combines insert operation with old value retrieval.
The expression (@'insertLookupWithKey' f k x map@)
is a pair where the first element is equal to (@'lookup' k map@)
and the second element equal to (@'insertWithKey' f k x map@).

> let f key new_value old_value = (show key) ++ \":\" ++ new_value ++ \"|\" ++ old_value
> insertLookupWithKey f 5 \"xxx\" (from-list [(5,\"a\"), (3,\"b\")]) == (Just \"a\", from-list [(3, \"b\"), (5, \"5:xxx|a\")])
> insertLookupWithKey f 7 \"xxx\" (from-list [(5,\"a\"), (3,\"b\")]) == (Nothing,  from-list [(3, \"b\"), (5, \"a\"), (7, \"xxx\")])
> insertLookupWithKey f 5 \"xxx\" empty                         == (Nothing,  singleton 5 \"xxx\")

This is how to define @insertLookup@ using @insertLookupWithKey@:

> let insertLookup kx x t = insertLookupWithKey (\\_ a _ -> a) kx x t
> insertLookup 5 \"x\" (from-list [(5,\"a\"), (3,\"b\")]) == (Just \"a\", from-list [(3, \"b\"), (5, \"x\")])
> insertLookup 7 \"x\" (from-list [(5,\"a\"), (3,\"b\")]) == (Nothing,  from-list [(3, \"b\"), (5, \"a\"), (7, \"x\")])"
  '())

(sig insert-with ∷ Ord k
                 ⇒ (a → a → a)
                 → k
                 → a
                 → Map k a
                 → Map k a)
(do-nothing (insert-with f k v m)
  "O(log n). Insert with a function, combining new value and old value.
@('insert-with' f key value m)@ will insert the pair @(tup key value)@ into @m@
if @key@ does not exist in the map. If the key does exist, the function will
insert the pair @(tup key (f new-value old-value))@.

> (insert-with ++ 5 \"xxx\" (from-list (list (tup 5 \"a\") (tup 3 \"b\")))) == (from-list (list (tup 3 \"b\") (tup 5 \"xxxa\")))
> (insert-with ++ 7 \"xxx\" (from-list (list (tup 5 \"a\") (tup 3 \"b\")))) == (from-list (list (tup 3 \"b\") (tup 5 \"a\") (tup 7 \"xxx\")))
> (insert-with ++ 5 \"xxx\" empty) == singleton 5 \"xxx\""
  '())

(sig insert-with-key ∷ Ord k
                     ⇒ (k → a → a → a)
                     → k
                     → a
                     → Map k a
                     → Map k a)
(define-container-function (insert-with-key f k v m)
  "O(log n). Insert with a function, combining key, new value and old value.
@'insertWithKey' f key value mp@
will insert the pair (key, value) into @mp@ if key does
not exist in the map. If the key does exist, the function will
insert the pair @(key,f key new_value old_value)@.
Note that the key passed to f is the same key passed to 'insertWithKey'.

> let f key new_value old_value = (show key) ++ \":\" ++ new_value ++ \"|\" ++ old_value
> insertWithKey f 5 \"xxx\" (from-list [(5,\"a\"), (3,\"b\")]) == from-list [(3, \"b\"), (5, \"5:xxx|a\")]
> insertWithKey f 7 \"xxx\" (from-list [(5,\"a\"), (3,\"b\")]) == from-list [(3, \"b\"), (5, \"a\"), (7, \"xxx\")]
> insertWithKey f 5 \"xxx\" empty                         == singleton 5 \"xxx\""
  '())

(sig intersection-with ∷ Ord k
                       ⇒ (a → b → c)
                       → Map k a
                       → Map k b
                       → Map k c)
(define-container-function (intersection-with f m1 m2)
  "O(n+m). Intersection with a combining function.  The implementation uses
an efficient hedge algorithm comparable with hedge-union.

> intersectionWith (++) (from-list [(5, \"a\"), (3, \"b\")]) (from-list [(5, \"A\"), (7, \"C\")]) == singleton 5 \"aA\""
  '())

(sig intersection-with-key ∷ Ord k
                           ⇒ (k → a → b → c)
                           → Map k a
                           → Map k b
                           → Map k c)
(define-container-function (intersection-with-key f m1 m2)
  "O(n+m). Intersection with a combining function.  The implementation uses
an efficient hedge algorithm comparable with hedge-union.

> let f k al ar = (show k) ++ \":\" ++ al ++ \"|\" ++ ar
> intersectionWithKey f (from-list [(5, \"a\"), (3, \"b\")]) (from-list [(5, \"A\"), (7, \"C\")]) == singleton 5 \"5:a|A\""
  '())

(sig map ∷ (a → b)
         → Map k a
         → Map k b)
(define-container-function (map f m)
  "O(n). Map a function over all values in the map.

> map (++ \"x\") (from-list [(5,\"a\"), (3,\"b\")]) == from-list [(3, \"bx\"), (5, \"ax\")]"
  '())

(sig map-accum ∷ (a → b → (a, c))
               → a
               → Map k b
               → (a, Map k c))
(define-container-function (map-accum f i m)
  "O(n). The function 'mapAccum' threads an accumulating
argument through the map in ascending order of keys.

> let f a b = (a ++ b, b ++ \"X\")
> mapAccum f \"Everything: \" (from-list [(5,\"a\"), (3,\"b\")]) == (\"Everything: ba\", from-list [(3, \"bX\"), (5, \"aX\")])"
  '())

(sig map-accum-l ∷ (a → k → b → (a, c))
                 → a
                 → Map k b → (a, Map k c))
(define-container-function (map-accum-l)
  "O(n). The function 'mapAccumL' threads an accumulating
argument through the map in ascending order of keys."
  '())

(sig map-accum-r ∷ (a → k → b → (a, c))
                 → a
                 → Map k b
                 → (a, Map k c))
(define-container-function (map-accum-r)
  "O(n). The function 'mapAccumR' threads an accumulating
argument through the map in descending order of keys."
  '())

(sig map-accum-with-key ∷ (a → k → b → (a, c))
                        → a
                        → Map k b
                        → (a, Map k c))
(define-container-function (map-accum-with-key)
  "O(n). The function 'mapAccumWithKey' threads an accumulating
argument through the map in ascending order of keys.

> let f a k b = (a ++ \" \" ++ (show k) ++ \"-\" ++ b, b ++ \"X\")
> mapAccumWithKey f \"Everything:\" (from-list [(5,\"a\"), (3,\"b\")]) == (\"Everything: 3-b 5-a\", from-list [(3, \"bX\"), (5, \"aX\")])"
  '())

(sig map-either ∷ (a → Either b c)
                → Map k a
                → (Map k b, Map k c))
(define-container-function (map-either)
  "O(n). Map values and separate the 'Left' and 'Right' results.

> let f a = if a < \"c\" then Left a else Right a
> mapEither f (from-list [(5,\"a\"), (3,\"b\"), (1,\"x\"), (7,\"z\")])
>     == (from-list [(3,\"b\"), (5,\"a\")], from-list [(1,\"x\"), (7,\"z\")])
>
> mapEither (\\ a -> Right a) (from-list [(5,\"a\"), (3,\"b\"), (1,\"x\"), (7,\"z\")])
>     == (empty, from-list [(5,\"a\"), (3,\"b\"), (1,\"x\"), (7,\"z\")])"
  '())

(sig map-either-with-key ∷ (k → a → Either b c)
                         → Map k a
                         → (Map k b, Map k c))
(define-container-function (map-either-with-key)
  "O(n). Map keys/values and separate the 'Left' and 'Right' results.

> let f k a = if k < 5 then Left (k * 2) else Right (a ++ a)
> mapEitherWithKey f (from-list [(5,\"a\"), (3,\"b\"), (1,\"x\"), (7,\"z\")])
>     == (from-list [(1,2), (3,6)], from-list [(5,\"aa\"), (7,\"zz\")])
>
> mapEitherWithKey (\\_ a -> Right a) (from-list [(5,\"a\"), (3,\"b\"), (1,\"x\"), (7,\"z\")])
>     == (empty, from-list [(1,\"x\"), (3,\"b\"), (5,\"a\"), (7,\"z\")])"
  '())

(sig map-keys-with ∷ Ord k'
                   ⇒ (a → a → a)
                   → (k → k')
                   → Map k a
                   → Map k' a)
(define-container-function (map-keys-with)
  "O(n*log n).
@'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.

The size of the result may be smaller if @f@ maps two or more distinct
keys to the same new key.  In this case the associated values will be
combined using @c@.

> mapKeysWith (++) (\\ _ -> 1) (from-list [(1,\"b\"), (2,\"a\"), (3,\"d\"), (4,\"c\")]) == singleton 1 \"cdab\"
> mapKeysWith (++) (\\ _ -> 3) (from-list [(1,\"b\"), (2,\"a\"), (3,\"d\"), (4,\"c\")]) == singleton 3 \"cdab\""
  '())

(sig map-maybe ∷ (a → Maybe b)
               → Map k a
               → Map k b)
(define-container-function (map-maybe)
  "O(n). Map values and collect the 'Just' results.

> let f x = if x == \"a\" then Just \"new a\" else Nothing
> mapMaybe f (from-list [(5,\"a\"), (3,\"b\")]) == singleton 5 \"new a\""
  '())

(sig map-maybe-with-key ∷ (k → a → Maybe b)
                        → Map k a
                        → Map k b)
(define-container-function (map-maybe-with-key)
  "O(n). Map keys/values and collect the 'Just' results.

> let f k _ = if k < 5 then Just (\"key : \" ++ (show k)) else Nothing
> mapMaybeWithKey f (from-list [(5,\"a\"), (3,\"b\")]) == singleton 3 \"key : 3\""
  '())

(sig map-with-key ∷ (k → a → b)
                  → Map k a
                  → Map k b)
(define-container-function (map-with-key)
  "O(n). Map a function over all values in the map.

> let f key x = (show key) ++ \":\" ++ x
> mapWithKey f (from-list [(5,\"a\"), (3,\"b\")]) == from-list [(3, \"3:b\"), (5, \"5:a\")]"
  '())

(sig merge-with-key ∷ Ord k
                    ⇒ (k → a → b → Maybe c)
                    → (Map k a → Map k c)
                    → (Map k b → Map k c))
(define-container-function (merge-with-key f g1 g2 m1 m2)
  "O(n+m). A high-performance universal combining function. This function
is used to define 'unionWith', 'unionWithKey', 'differenceWith',
'differenceWithKey', 'intersectionWith', 'intersectionWithKey' and can be
used to define other custom combine functions.

Please make sure you know what is going on when using 'mergeWithKey',
otherwise you can be surprised by unexpected code growth or even
corruption of the data structure.

When 'mergeWithKey' is given three arguments, it is inlined to the call
site. You should therefore use 'mergeWithKey' only to define your custom
combining functions. For example, you could define 'unionWithKey',
'differenceWithKey' and 'intersectionWithKey' as

> myUnionWithKey f m1 m2 = mergeWithKey (\\k x1 x2 -> Just (f k x1 x2)) id id m1 m2
> myDifferenceWithKey f m1 m2 = mergeWithKey f id (const empty) m1 m2
> myIntersectionWithKey f m1 m2 = mergeWithKey (\\k x1 x2 -> Just (f k x1 x2)) (const empty) (const empty) m1 m2

When calling @'mergeWithKey' combine only1 only2@, a function combining two
'IntMap's is created, such that

* if a key is present in both maps, it is passed with both corresponding
  values to the @combine@ function. Depending on the result, the key is either
  present in the result with specified value, or is left out;

* a nonempty subtree present only in the first map is passed to @only1@ and
  the output is added to the result;

* a nonempty subtree present only in the second map is passed to @only2@ and
  the output is added to the result.

The @only1@ and @only2@ methods must return a map with a subset (possibly empty) of the keys of the given map.
The values can be modified arbitrarily. Most common variants of @only1@ and
@only2@ are 'id' and @'const' 'empty'@, but for example @'map' f@ or
@'filterWithKey' f@ could be used for any @f@."
  '())

(sig singleton ∷ k → a → Map k a)
(do-nothing (singleton k v)
  "O(1). A map with a single element.

> (singleton 1 #\\a) == (from-list (list (tup 1 #\\a)))
> (size (singleton 1 #\\a)) == 1"
  '())

(sig union-with ∷ Ord k
                ⇒ (a → a → a)
                → Map k a
                → Map k a
                → Map k a)
(define-container-function (union-with f m1 m2)
  "O(n+m). Union with a combining function. The implementation uses the efficient hedge-union algorithm.

> unionWith (++) (from-list [(5, \"a\"), (3, \"b\")]) (from-list [(5, \"A\"), (7, \"C\")]) == from-list [(3, \"b\"), (5, \"aA\"), (7, \"C\")]"
  '())

(sig union-with-key ∷ Ord k
                    ⇒ (k → a → a → a)
                    → Map k a
                    → Map k a
                    → Map k a)
(define-container-function (union-with-key f m1 m2)
  "O(n+m).
Union with a combining function. The implementation uses the efficient hedge-union algorithm.

> let f key left_value right_value = (show key) ++ \":\" ++ left_value ++ \"|\" ++ right_value
> unionWithKey f (from-list [(5, \"a\"), (3, \"b\")]) (from-list [(5, \"A\"), (7, \"C\")]) == from-list [(3, \"b\"), (5, \"5:a|A\"), (7, \"C\")]"
  '())

(sig unions-with ∷ Ord k
                 ⇒ (a → a → a)
                 → [Map k a]
                 → Map k a)
(define-container-function (unions-with f ms)
  "The union of a list of maps, with a combining operation:
  (@'unionsWith' f == 'Prelude.foldl' ('unionWith' f) 'empty'@).

> unionsWith (++) [(from-list [(5, \"a\"), (3, \"b\")]), (from-list [(5, \"A\"), (7, \"C\")]), (from-list [(5, \"A3\"), (3, \"B3\")])]
>     == from-list [(3, \"bB3\"), (5, \"aAA3\"), (7, \"C\")]"
  '())

(sig update ∷ Ord k
            ⇒ (a → Maybe a)
            → k
            → Map k a
            → Map k a)
(define-container-function (update f k m)
  "O(log n). The expression (@'update' f k map@) updates the value @x@
at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.

> let f x = if x == \"a\" then Just \"new a\" else Nothing
> update f 5 (from-list [(5,\"a\"), (3,\"b\")]) == from-list [(3, \"b\"), (5, \"new a\")]
> update f 7 (from-list [(5,\"a\"), (3,\"b\")]) == from-list [(3, \"b\"), (5, \"a\")]
> update f 3 (from-list [(5,\"a\"), (3,\"b\")]) == singleton 5 \"a\""
  '())

(sig update-at ∷ (k → a → Maybe a)
               → Int
               → Map k a
               → Map k a)
(define-container-function (update-at f idx m)
  "O(log n). Update the element at index. Calls 'error' when an
invalid index is used.

> updateAt (\\ _ _ -> Just \"x\") 0    (from-list [(5,\"a\"), (3,\"b\")]) == from-list [(3, \"x\"), (5, \"a\")]
> updateAt (\\ _ _ -> Just \"x\") 1    (from-list [(5,\"a\"), (3,\"b\")]) == from-list [(3, \"b\"), (5, \"x\")]
> updateAt (\\ _ _ -> Just \"x\") 2    (from-list [(5,\"a\"), (3,\"b\")])    Error: index out of range
> updateAt (\\ _ _ -> Just \"x\") (-1) (from-list [(5,\"a\"), (3,\"b\")])    Error: index out of range
> updateAt (\\_ _  -> Nothing)  0    (from-list [(5,\"a\"), (3,\"b\")]) == singleton 5 \"a\"
> updateAt (\\_ _  -> Nothing)  1    (from-list [(5,\"a\"), (3,\"b\")]) == singleton 3 \"b\"
> updateAt (\\_ _  -> Nothing)  2    (from-list [(5,\"a\"), (3,\"b\")])    Error: index out of range
> updateAt (\\_ _  -> Nothing)  (-1) (from-list [(5,\"a\"), (3,\"b\")])    Error: index out of range"
  '())

(sig update-lookup-with-key ∷ Ord k
                            ⇒ (k → a → Maybe a)
                            → k
                            → Map k a
                            → (Maybe a, Map k a))
(define-container-function (update-lookup-with-key f k m)
  "O(log n). Lookup and update. See also 'updateWithKey'.
The function returns changed value, if it is updated.
Returns the original key value if the map entry is deleted.

> let f k x = if x == \"a\" then Just ((show k) ++ \":new a\") else Nothing
> updateLookupWithKey f 5 (from-list [(5,\"a\"), (3,\"b\")]) == (Just \"5:new a\", from-list [(3, \"b\"), (5, \"5:new a\")])
> updateLookupWithKey f 7 (from-list [(5,\"a\"), (3,\"b\")]) == (Nothing,  from-list [(3, \"b\"), (5, \"a\")])
> updateLookupWithKey f 3 (from-list [(5,\"a\"), (3,\"b\")]) == (Just \"b\", singleton 5 \"a\")"
  '())

(sig update-max ∷ (a → Maybe a)
                → Map k a
                → Map k a)
(define-container-function (update-max f m)
  "O(log n). Update the value at the maximal key.

> updateMax (\\ a -> Just (\"X\" ++ a)) (from-list [(5,\"a\"), (3,\"b\")]) == from-list [(3, \"b\"), (5, \"Xa\")]
> updateMax (\\ _ -> Nothing)         (from-list [(5,\"a\"), (3,\"b\")]) == singleton 3 \"b\""
  '())

(sig update-max-with-key ∷ (k → a → Maybe a)
                         → Map k a
                         → Map k a)
(define-container-function (update-max-with-key f m)
  "O(log n). Update the value at the maximal key.

> updateMaxWithKey (\\ k a -> Just ((show k) ++ \":\" ++ a)) (from-list [(5,\"a\"), (3,\"b\")]) == from-list [(3,\"b\"), (5,\"5:a\")]
> updateMaxWithKey (\\ _ _ -> Nothing)                     (from-list [(5,\"a\"), (3,\"b\")]) == singleton 3 \"b\""
  '())

(sig update-min ∷ (a → Maybe a)
                → Map k a
                → Map k a)
(define-container-function (update-min f m)
  "O(log n). Update the value at the minimal key.

> updateMin (\\ a -> Just (\"X\" ++ a)) (from-list [(5,\"a\"), (3,\"b\")]) == from-list [(3, \"Xb\"), (5, \"a\")]
> updateMin (\\ _ -> Nothing)         (from-list [(5,\"a\"), (3,\"b\")]) == singleton 5 \"a\""
  '())

(sig update-min-with-key ∷ (k → a → Maybe a)
                         → Map k a
                         → Map k a)
(define-container-function (update-min-with-key f m)
  "O(log n). Update the value at the minimal key.

> updateMinWithKey (\\ k a -> Just ((show k) ++ \":\" ++ a)) (from-list [(5,\"a\"), (3,\"b\")]) == from-list [(3,\"3:b\"), (5,\"a\")]
> updateMinWithKey (\\ _ _ -> Nothing) (from-list [(5,\"a\"), (3,\"b\")]) == singleton 5 \"a\""
  '())

(sig update-with-key ∷ Ord k
                     ⇒ (k → a → Maybe a)
                     → k
                     → Map k a
                     → Map k a)
(define-container-function (update-with-key f k m)
  "O(log n). The expression (@'updateWithKey' f k map@) updates the
value @x@ at @k@ (if it is in the map). If (@f k x@) is 'Nothing',
the element is deleted. If it is (@'Just' y@), the key @k@ is bound
to the new value @y@.

> let f k x = if x == \"a\" then Just ((show k) ++ \":new a\") else Nothing
> updateWithKey f 5 (from-list [(5,\"a\"), (3,\"b\")]) == from-list [(3, \"b\"), (5, \"5:new a\")]
> updateWithKey f 7 (from-list [(5,\"a\"), (3,\"b\")]) == from-list [(3, \"b\"), (5, \"a\")]
> updateWithKey f 3 (from-list [(5,\"a\"), (3,\"b\")]) == singleton 5 \"a\""
  '())
