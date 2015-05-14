;;; Copyright 2015 Amyas Chew, Lynn Chua, Yongquan Lu
;;; 
;;; This file is part of math-scm.
;;; 
;;; math-scm is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; 
;;; math-scm is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with math-scm.  If not, see <http://www.gnu.org/licenses/>.

;;;; ############################################################################################
;;;; ############################################################################################
;;;; Load files

;;; #############################################################################################
;;; Scheme layer

;;; Non-deterministic search with amb
(load "amb/funco")
(load "amb/stack-queue")
(load "amb/ambsch")

;;; Generic procedures
(load "ghelper.scm")

;;; Testing procedures
(load "test.scm")

;;; Utility procedures
(load "util.scm")

;;; Generic comparators
(load "comparators.scm")

;;; Math object definitions
(load "math-object.scm")

;;; #############################################################################################
;;; Math-layer

;;; Sets ----------------------------------------------------------------------------------------
(load "set.scm")

;;; Group-like ----------------------------------------------------------------------------------
(load "group-like.scm")

;;; Magma
(load "magma.scm")

;;; Semigroups
(load "semigroup.scm")

;;; Monoids
(load "monoid.scm")

;;; Groups
(load "group.scm")

;;; Ring-like -----------------------------------------------------------------------------------
(load "ring-like.scm")

;;; Rings
(load "ring.scm")

;;; Semirings
(load "semiring.scm")

;;; Fields
(load "field.scm")

;;; Graphs --------------------------------------------------------------------------------------
(load "graph.scm")

;;; #############################################################################################
;;; Miscellaneous

;;; Generic arithmetic (for dealing with square roots)
(load "arith/compatibility.scm")
(load "arith/utils.scm")
(load "arith/package.scm")
(load "arith/operators.scm")
(load "arith/arith.scm")
(load "arith/generics.scm")
(load "arith/matrix.scm")
(load "arith/root-list.scm")
