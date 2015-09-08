;;; File: graph.scm
;;
;;; License:
;; Copyright © 2015 Remy Goldschmidt <taktoa@gmail.com>
;; Copyright © 2005 Guillaume Marceau <gmarceau@cs.brown.edu>
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
;; along with ThomasEngine2. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Author:     Remy Goldschmidt <taktoa@gmail.com>
;;; Maintainer: Remy Goldschmidt <taktoa@gmail.com>
;;; Created:    August 25th, 2015
;;
;;; Homepage:   https://github.com/taktoa/ThomasEngine2
;;
;;; Commentary:
;; This is a graph library for Guile.
;;
;;; Code:

(define-module (thomas utils graph)
  #:version    (0 0 1)
;;  #:use-module (thomas utils misc)
  #:use-module (scheme documentation)
  #:use-module (ice-9  hash-table)
  #:use-module (ice-9  format)
  #:use-module (ice-9  match)
  #:use-module (ice-9  pretty-print)
  #:use-module (srfi   srfi-1)
  #:use-module (srfi   srfi-11)
  #:use-module (srfi   srfi-26)
  #:use-module (oop    goops)
  #:export     (make-graph
                ;; --- Constructors :
                graph?
                graph-directed?
                graph-make-similar
                graph-copy
                graph-add-all!
                ;; --- Functions on nodes:
                graph-nodes
                graph-nodes-size
                graph-make-node!
                graph-node-add!
                graph-node-mem?
                graph-node-set!
                graph-node-remove!
                graph-node-collapse!
                graph-node-has-label?
                graph-node-label
                graph-for-each-node
                graph-fold-nodes
                ;; --- Functions on neighbors:
                graph-succs
                graph-preds
                graph-adjs
                graph-for-each-adjs
                ;; --- Functions on edges:
                graph-edges
                graph-edges-size
                graph-edge-add!
                graph-edge-mem?
                graph-edge-set!
                graph-edge-remove!
                graph-edge-has-label?
                graph-edge-label
                graph-for-each-edge
                graph-fold-edges
                ;; --- Simple graph algorithms:
                graph-dfs-from-node
                graph-dfs-all
                graph-components
                graph-strongly-connected-components
                graph-topological-sort
                ;; --- Debugging:
                graph-to-list
                graph-to-string
                graph-test))

(define-class <state> ()
  (flags        #:init-value   0
                #:getter       get-flags)
  (n-nodes      #:init-value   #:n-nodes
                #:getter       get-n-nodes
                #:setter       set-n-nodes)
  (n-edges      #:init-keyword #:n-edges
                #:getter       get-n-nodes
                #:setter       set-n-edges)
  (nodes        #:init-keyword #:nodes
                #:getter       get-nodes)
  (successors   #:init-keyword #:successors
                #:getter       get-successors)
  (predecessors #:init-keyword #:predecessors
                #:getter       get-predecessors)
  (type         #:init-keyword #:type
                #:getter       get-type))

(define (state? val) (is-a? val <state>))

(define (make-state flags n-nodes n-edges nodes successors predecessors)
  (make (<state>)
    #:flags        flags
    #:n-nodes      n-nodes
    #:n-edges      n-edges
    #:nodes        nodes
    #:successors   successors
    #:predecessors predecessors))


;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------

;;  (require "base-gm.ss")


(is-a? a <>)
(define-struct t (flags n-nodes n-edges nodes successors predecessors))

;; Flags can be: 'equal 'directed 'unique-node 'unique-edge 'nodes-must-exists 'safe
;; 'safe is a short for '(unique-node unique-edge nodes-must-exists)
(define (make-graph . flags)
  (let ((flag-hash (make-hash)))
    (set! flags (expands-safe-flag flags))
    (for-each-f flags (λ (flag) (hash-put! flag-hash flag true)))
    (if (member 'equal flags)
        (make-state flag-hash 0 0 (make-hash 'equal) (make-hash 'equal) (make-hash 'equal))
        (make-state flag-hash 0 0 (make-hash) (make-hash) (make-hash)))))

(define (graph? graph) (state? graph))

(define no-value (box empty))

;; Makes a hash with the same 'equal as the graph
(define (graph-make-hash graph)
  (if (graph-has-flag? graph 'equal)
      (make-hash 'equal)
      (make-hash)))


(define (expands-safe-flag flags)
  (let loop ((cur flags) (acc empty))
    (cond [(empty? cur)            acc]
          [(eq? (first cur) 'safe) (loop (rest cur)
                                         (append '(unique-node
                                                   unique-edge
                                                   nodes-must-exists)
                                                 flags))]
          [true (loop (rest cur)   (cons (first cur) acc))])))

;; Make a graph with mostly the same flags as another graph
(define (graph-make-similar graph
                            plus-flags
                            minus-flags)
  (set! plus-flags  (expands-safe-flag plus-flags))
  (set! minus-flags (expands-safe-flag minus-flags))
  (apply make-graph
         (append plus-flags
                 (filter (λ (i) (not (member i minus-flags)))
                         (hash-keys (get-flags graph))))))

(define (graph-copy graph)
  (let* ((rtn-nodes        (graph-make-hash graph))
         (rtn-successors   (graph-make-hash graph))
         (rtn-predecessors (graph-make-hash graph))
         (rtn              (make-g (get-flags   graph)
                                   (get-n-nodes graph)
                                   (get-n-edges graph)
                                   rtn-nodes
                                   rtn-successors
                                   rtn-predecessors)))
    (hash-add-all! rtn-nodes        (get-nodes        graph))
    (hash-add-all! rtn-successors   (get-successors   graph))
    (hash-add-all! rtn-predecessors (get-predecessors graph))
    rtn))

(define (graph-add-all! dest-graph src-graph)
  (graph-for-each-node
   src-graph
   (λ (node)
     (if (graph-node-has-label? src-graph node)
         (graph-node-add! dest-graph node (graph-node-label src-graph node))
         (graph-node-add! dest-graph node))))
  (graph-for-each-edge
   src-graph
   (λ (from to)
     (if (graph-edge-has-label? src-graph from to)
         (graph-edge-add! dest-graph from to (graph-edge-label src-graph from to))
         (graph-edge-add! dest-graph from to)))))

(define (graph-has-flag? graph flag)
  (hash-mem? (get-flags graph) flag))

(define (graph-directed? graph)
  (hash-mem? (get-flags graph) 'directed))

;;; =====================================================================
;;; Nodes

(define (graph-nodes graph) (hash-keys (get-nodes graph)))

(define (graph-nodes-size graph) (get-n-nodes graph))

(define graph-make-node!
  (case-lambda
    [(graph) (graph-make-node! graph no-value)]
    [(graph val)
     (let ((sym (string->symbol (string-append "node" (number->string (get-n-nodes graph))))))
       (graph-node-add! graph sym val)
       sym)]))

;; Add a node to the graph. If the node already exists,
;; sets its label, unless the graph has the 'unique-node property,
;; in which case this will assert.
(define graph-node-add!
  (case-lambda
    [(graph node) (graph-node-add! graph node no-value)]
    [(graph node val)
     (if (hash-mem? (get-nodes graph) node)
         (assert (not (graph-has-flag? graph 'unique-node)))
         (begin
           (set-n-nodes! graph (+ 1 (get-n-nodes graph)))
           (hash-put! (get-successors graph) node (graph-make-hash graph))
           (if (graph-directed? graph)
               (hash-put! (get-predecessors graph) node (graph-make-hash graph)))))
     (hash-put! (get-nodes graph) node val)]))

(define (graph-node-mem? graph node)
  (hash-mem? (get-nodes graph) node))

(define (graph-node-set! graph node val)
  (assert (hash-mem? (get-nodes graph) node))
  (hash-put! (get-nodes graph) node val))

(define (graph-node-remove! graph node)
  (assert (graph-node-mem? graph node))
  (for-each-f (hash-get (get-successors graph) node)
              (λ (i) (graph-edge-remove! graph node i)))

  (if (graph-directed? graph)
      (for-each-f (hash-get (get-predecessors graph) node)
                  (λ (i) (graph-edge-remove! graph i node))))

  (hash-remove! (get-nodes graph) node)
  (hash-remove! (get-successors graph) node)
  (if (graph-directed? graph)
      (hash-remove! (get-predecessors graph) node))
  (set-n-nodes! graph (- (get-n-nodes graph) 1)))

(define graph-node-collapse!
  (case-lambda
    [(graph node with-self-loop) (graph-node-collapse! graph node with-self-loop (λ (pred-label succ-label) no-value))]
    [(graph node with-self-loop label-fn)
     (let ((is-directed (graph-directed? graph)))
       (for-each-f

        (if is-directed
            (hash-get (get-predecessors graph) node)
            (hash-get (get-successors graph) node))

        (λ (pred)
          (for-each-f
           (hash-get (get-successors graph) node)
           (λ (succ)
             (unless (or (and (not is-directed) (eq? pred succ))
                         (graph-edge-mem? graph pred succ))
               (let* ((label-pred (hash-get (hash-get (get-successors graph) pred) node))
                      (label-succ (hash-get (hash-get (get-successors graph) node) succ))
                      (new-label (label-fn (if (eq? label-pred no-value) false label-pred)
                                           (if (eq? label-succ no-value) false label-succ))))
                 (when (or with-self-loop (not (eq? pred succ)))
                   (hash-put! (hash-get (get-successors graph) pred) succ new-label)
                   (if is-directed
                       (hash-put! (hash-get (get-predecessors graph) succ) pred new-label)
                       (hash-put! (hash-get (get-successors graph) succ) pred new-label))))))))))
     (graph-node-remove! graph node)]))

(define (graph-node-has-label? graph node)
  (not (eq? (hash-get (get-nodes graph) node) no-value)))

(define (graph-node-label graph node)
  (let ((r (hash-get (get-nodes graph) node)))
    (if (eq? r no-value) (error "graph-node-label: no value for node" node)
        r)))

(define (graph-succs graph node)
  (assert (graph-directed? graph))
  (hash-keys (hash-get (get-successors graph) node)))

(define (graph-preds graph node)
  (assert (graph-directed? graph))
  (hash-keys (hash-get (get-predecessors graph) node)))

(define (graph-adjs graph node)
  (if (graph-directed? graph)
      (append (hash-keys (hash-get (get-successors graph) node))
              (hash-keys (hash-get (get-predecessors graph) node)))
      (hash-keys (hash-get (get-successors graph) node))))

(define (graph-for-each-adjs graph node fn)
  (for-each (λ (succ) (fn node succ))
            (hash-get (get-successors graph) node))
  (when (graph-directed? graph)
    (for-each (λ (pred) (fn pred node))
              (hash-get (get-predecessors graph) node))))

(define (graph-for-each-node graph fn)
  (for-each-f (get-nodes graph) fn))

(define (graph-fold-nodes graph init fn)
  (let ((acc init))
    (graph-for-each-node
     graph
     (λ (node) (set! acc (fn node acc))))
    acc))

;;; =====================================================================
;;; Edges

(define (graph-edges graph)
  (let ((rtn empty))
    (graph-for-each-edge graph (λ (from to) (set! rtn (cons from to))))
    rtn))

(define (graph-edges-size graph) (get-n-edges graph))

;; Add an edge to the graph. If the edge already exists,
;; sets its label, unless the graph has the 'unique-edge property,
;; in which case this will assert.
(define graph-edge-add!
  (case-lambda
    [(graph from to) (graph-edge-add! graph from to no-value)]
    [(graph from to val)

     (if (graph-edge-mem? graph from to)
         (assert (not (graph-has-flag? graph 'unique-edge)))
         (set-n-edges! graph (+ (get-n-edges graph) 1)))

     (if (graph-has-flag? graph 'nodes-must-exists)
         (assert (and (graph-node-mem? graph from) (graph-node-mem? graph to)))
         (begin (if (not (graph-node-mem? graph from)) (graph-node-add! graph from))
                (if (not (graph-node-mem? graph to)) (graph-node-add! graph to))))

     (hash-put! (hash-get (get-successors graph) from) to val)

     (if (graph-directed? graph)
         (hash-put! (hash-get (get-predecessors graph) to) from val)
         (hash-put! (hash-get (get-successors graph) to) from val))]))

(define (graph-edge-mem? graph from to)
  (if (graph-has-flag? graph 'nodes-must-exists)
      (assert (and (graph-node-mem? graph from)
                   (graph-node-mem? graph to))))

  (and (hash-mem? (get-successors graph) from)
       (hash-mem? (hash-get (get-successors graph) from) to)))

(define (graph-edge-set! graph from to val)
  (assert (graph-edge-mem? graph from to))
  (hash-put! (hash-get (get-successors graph) from) to val)

  (if (graph-directed? graph)
      (hash-put! (hash-get (get-predecessors graph) to) from val)
      (hash-put! (hash-get (get-successors graph) to) from val)))

(define (graph-edge-remove! graph from to)
  (assert (graph-edge-mem? graph from to))
  (hash-remove! (hash-get (get-successors graph) from) to)

  (if (graph-directed? graph)
      (hash-remove! (hash-get (get-predecessors graph) to) from)
      (hash-remove! (hash-get (get-successors graph) to) from)))

(define (graph-edge-has-label? graph from to)
  (not (eq? (hash-get (hash-get (get-successors graph) from) to) no-value)))

(define (graph-edge-label graph from to)
  (let ((r (hash-get (hash-get (get-successors graph) from) to)))
    (if (eq? r no-value) (error "graph-edge-label: no value for edge" (cons from to)))
    r))

(define (graph-for-each-edge graph fn)
  (graph-for-each-node
   graph
   (λ (from)
     (for-each-f (hash-get (get-successors graph) from)
                 (λ (to) (fn from to))))))

(define (graph-fold-edges graph init fn)
  (let ((acc init))
    (graph-for-each-edge
     graph
     (λ (from to) (set! acc (fn from to acc))))
    acc))

;;; =====================================================================
;;; Algos

(define (graph-dfs-from-node-with-log graph node dealt-with pre-fn post-fn backward)
  (assert (or (not backward) (graph-directed? graph)))
  (if (not (hash-mem? dealt-with node))
      (begin (hash-put! dealt-with node true)
             (pre-fn node)
             (for-each-f (if backward
                             (hash-get (get-predecessors graph) node)
                             (hash-get (get-successors graph) node))
                         (λ (n) (graph-dfs-from-node-with-log graph n dealt-with pre-fn post-fn backward)))
             (post-fn node))))


(define graph-dfs-from-node
  (case-lambda
    [(graph node pre-fn) (graph-dfs-from-node graph node pre-fn (λ (i) i))]
    [(graph node pre-fn post-fn)
     (graph-dfs-from-node-with-log graph node (graph-make-hash graph) pre-fn post-fn false)]))

(define graph-dfs-all
  (case-lambda
    [(graph pre-fn) (graph-dfs-all graph pre-fn (λ (i) i))]
    [(graph pre-fn post-fn)
     (let ((dealt-with (graph-make-hash graph)))
       (graph-for-each-node graph (λ (n) (if (not (hash-mem? dealt-with n))
                                             (graph-dfs-from-node-with-log graph n dealt-with pre-fn post-fn false)))))]))


(define (graph-components graph)
  (let ([dealt-with (graph-make-hash graph)])
    (graph-fold-nodes graph
                      empty
                      (λ (node acc)
                        (if (hash-mem? dealt-with node) acc
                            (let ((cur-component
                                   (let loop ((cur node) (acc empty))
                                     (if (hash-mem? dealt-with cur) acc
                                         (begin (hash-put! dealt-with cur true)
                                                (foldl (λ (adj acc) (loop adj acc)) (cons cur acc)
                                                       (graph-adjs graph cur)))))))
                              (cons cur-component acc)))))))

(define (graph-strongly-connected-components graph)
  (assert (graph-directed? graph))
  (let ((finish-times empty)
        (dealt-with (graph-make-hash graph)))

    (graph-for-each-node
     graph
     (λ (n) (graph-dfs-from-node-with-log
             graph n dealt-with
             (λ (i) i)
             (λ (i) (set! finish-times (cons i finish-times)))
             false)))

    (set! dealt-with (graph-make-hash graph))

    (let ((component-graph (graph-make-similar graph empty '(safe equal)))
          (node2supernode (make-hash)))

      (for-each-f
       finish-times
       (λ (n)
         (if (not (hash-mem? dealt-with n))
             (let ((super-node (graph-make-node! component-graph empty)))
               (graph-dfs-from-node-with-log
                graph n dealt-with
                (λ (i)
                  (graph-node-set! component-graph super-node (cons i (graph-node-label component-graph super-node)))
                  (hash-put! node2supernode i super-node))
                (λ (i) i)
                true)))))
      (graph-for-each-edge graph
                           (λ (from to)
                             (graph-edge-add! component-graph
                                              (hash-get node2supernode from)
                                              (hash-get node2supernode to))))
      (cons component-graph node2supernode))))

(define (graph-topological-sort graph)
  (assert (graph-directed? graph))
  (let ((rtn empty))
    (graph-dfs-all graph (λ (i) i) (λ (node) (set! rtn (cons node rtn))))
    rtn))


;;; =====================================================================
;;; Utils

(define graph-to-list
  (case-lambda
    [(graph) (graph-to-list graph false)]
    [(graph with-labels)
     (hash-map (get-nodes graph)
               (λ (node node-val)
                 (let ((node-rep (if (and with-labels (graph-node-has-label? graph node))
                                     (cons node (graph-node-label graph node))
                                     node)))
                   (cons node-rep
                         (hash-fold (hash-get (get-successors graph) node) empty
                                    (λ (succ edge-val acc)
                                      (if (and with-labels (graph-edge-has-label? graph node succ))
                                          (cons (cons succ (graph-edge-label graph node succ)) acc)
                                          (cons succ acc))))))))]))

(define (graph-to-string-prv graph with-labels to-string)
  (let ([the-to-string (or to-string
                           (λ (item) (format #f "~a" item)))])
    (string-append (if (graph-directed? graph) "[di-graph: " "[undirected-graph:")
                   (the-to-string (map (λ (n)
                                         (cons (first n) (cons '--> (rest n))))
                                       (graph-to-list graph with-labels)))
                   "]")))

(define (graph-to-string graph . to-string)
  (graph-to-string-prv graph false (if (empty? to-string) false (first to-string))))

(define (graph-to-string-with-labels graph . to-string)
  (graph-to-string-prv graph true (if (empty? to-string) true (first to-string))))

(define to-string-f (make-to-string `((,state? ,graph-to-string))))
(define debug-f (make-debug to-string-f))
(define for-each-f (make-for-each))

;;; =====================================================================
;;; Tests

(define (graph-test)
  (define graph (make-graph 'safe 'directed))

  (graph-node-add! graph 'a)
  (graph-node-add! graph 'b 2)
  (graph-node-add! graph 'c 3)
  (graph-node-add! graph 'd)

  (graph-edge-add! graph 'a 'c)
  (graph-edge-add! graph 'a 'd "asd")
  (graph-edge-add! graph 'b 'c "dfg")
  (graph-edge-add! graph 'b 'd)
  (graph-edge-add! graph 'd 'a)

  (display (graph-node-mem? graph 'a))
  (display (graph-edge-mem? graph 'a 'c))
  (newline)
  (display (graph-node-mem? graph 'v))
  (display (graph-edge-mem? graph 'c 'a))
  (display (graph-edge-mem? graph 'a 'b))
  (newline)

  (debug-f (graph-to-list graph true))
  (graph-for-each-edge graph (λ (a b) (debug-f "A " a b)))

  (graph-dfs-from-node graph 'a (λ (i) (display i)))
  (newline)
  (graph-dfs-from-node graph 'b (λ (i) (display i)))
  (newline)
  (graph-dfs-from-node graph 'c (λ (i) (display i)))
  (newline)
  (graph-dfs-from-node graph 'd (λ (i) (display i)))
  (newline)

  (let ((star (make-graph 'directed)))
    (graph-edge-add! star 1 'x)
    (graph-edge-add! star 'x 1)
    (graph-edge-add! star 2 'x)
    (graph-edge-add! star 'x 3)
    (graph-edge-add! star 'x 4)
    (graph-edge-add! star 'x 5)
    (graph-node-collapse! star 'x false)
    (debug-f "collapsed:" (graph-to-list star)))

  (let ((strong-graph (make-graph 'directed)))

    (graph-edge-add! strong-graph 'e 'a)
    (graph-edge-add! strong-graph 'a 'b)
    (graph-edge-add! strong-graph 'b 'e)
    (graph-edge-add! strong-graph 'e 'f)
    (graph-edge-add! strong-graph 'b 'f)
    (graph-edge-add! strong-graph 'b 'c)
    (graph-edge-add! strong-graph 'f 'g)
    (graph-edge-add! strong-graph 'g 'f)
    (graph-edge-add! strong-graph 'c 'g)
    (graph-edge-add! strong-graph 'c 'd)
    (graph-edge-add! strong-graph 'd 'c)
    (graph-edge-add! strong-graph 'g 'h)
    (graph-edge-add! strong-graph 'd 'h)
    (graph-edge-add! strong-graph 'h 'h)

    (graph-edge-add! strong-graph 'xa 'xb)
    (graph-edge-add! strong-graph 'xb 'xc)
    (graph-edge-add! strong-graph 'xc 'xa)

    (debug-f "strong-graph" strong-graph)
    (debug-f "component" (graph-components strong-graph))
    (let ((components (graph-strongly-connected-components strong-graph)))
      (debug-f "strong-components" components)
      (debug-f "toposort" (graph-topological-sort (first components)))))

  (let ((u-graph (make-graph)))
    (graph-edge-add! u-graph 'a 'b)
    (graph-edge-add! u-graph 'b 'c)
    (graph-edge-add! u-graph 'c 'd)
    (graph-edge-add! u-graph 'd 'a)
    (graph-edge-add! u-graph 'd 'e)
    (graph-edge-add! u-graph 'e 'c)

    (graph-edge-add! u-graph 'xa 'xb)
    (graph-edge-add! u-graph 'xa 'xc)
    (graph-edge-add! u-graph 'xb 'xd)
    (newline)
    (debug-f "u-graph" u-graph)
    (graph-edge-remove! u-graph 'b 'a)
    (graph-node-remove! u-graph 'd)
    (debug-f "u-graph" u-graph)
    (debug-f "component" (graph-components u-graph))))
