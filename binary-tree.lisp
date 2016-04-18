;;;; A bianry tree base on lists with three elements and its recursive traversals.
;;;; eg. (A B C) , (A (B D E) (C F G)).

;;; The binary tree.
(defun the-entry (btree)
      `,(car btree))

(defun left-subtree (btree)
      `,(cadr btree))

(defun right-subtree (btree)
  (if (eql (cddr btree) nil)
      nil
      `,(caddr btree)))

;;; Traverse the tree.
;;; Helper functions & macros.
(defmacro if-not-cons-return (funcnm)
  `(unless (typep btree 'cons)
     (if (not (typep btree 'null))
	 (format t "~a " btree))
      (return-from ,funcnm nil)))

;; Pre-order traverse.
(defun pre-traverse (btree)
  (if-not-cons-return pre-traverse)
  (format t "~a " (the-entry btree))
  (pre-traverse (left-subtree btree))
  (pre-traverse (right-subtree btree)))

;; In-order traverse.
(defun in-traverse (btree)
  (if-not-cons-return in-traverse)
  (in-traverse (left-subtree btree))
  (format t "~a "  (the-entry btree))
  (in-traverse (right-subtree btree)))

;; Post-order traverse.
(defun post-traverse (btree)
  (if-not-cons-return post-traverse)
  (post-traverse (left-subtree btree))
  (post-traverse (right-subtree btree))
  (format t "~a " (the-entry btree)))

;;; Tests
(in-traverse '(A B C))
;; => B A C

(pre-traverse '(A (B D E) (C F G)))
;; => A B D E C F G

(in-traverse '(A (B D E) (C F G)))
;; => D B E A F C G

(post-traverse '(A (B D E) (C F G)))
;; => D E B F G C A 

(post-traverse '(A (B D) (C F G)))
;; => D B F G C A 

(pre-traverse '(A (B nil E) (C F G)))
;; => A B E C F G 

(in-traverse '(A nil (C F G)))
;; => A F C G

(post-traverse '(A (B (D G) (E H I)) (C nil F)))
;; => G D H I E B F C A 
