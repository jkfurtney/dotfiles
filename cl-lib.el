;;; cl-lib.el --- Properly prefixed CL functions and macros  -*- coding: utf-8 -*-

;; Copyright (C) 2012  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; vcomment: Emacs-24.3's version is 1.0 so this has to stay below.
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a forward compatibility package, which provides (a subset of) the
;; features of the cl-lib package introduced in Emacs-24.3, for use on
;; previous emacsen.

;; Make sure this is installed *late* in your `load-path`, i.e. after Emacs's
;; built-in .../lisp/emacs-lisp directory, so that if/when you upgrade to
;; Emacs-24.3, the built-in version of the file will take precedence, otherwise
;; you'll quickly get recursive-load errors.

;; This code is largely copied from Emacs-24.3's cl.el, with the alias bindings
;; simply reversed.

;;; Code:

(require 'cl)

;; Some of Emacs-24.3's cl.el definition are not just aliases, because either
;; the feature was dropped from cl-lib.el or because the cl-lib version is
;; not fully compatible.
;; Let's just not include them here, since it is very important that if code
;; works with this cl-lib.el it should also work with Emacs-24.3's cl-lib.el,
;; whereas the reverse is much less important.

(dolist (var '(
               ;; loop-result-var
               ;; loop-result
               ;; loop-initially
               ;; loop-finally
               ;; loop-bindings
               ;; loop-args
               ;; bind-inits
               ;; bind-block
               ;; lambda-list-keywords
               float-negative-epsilon
               float-epsilon
               least-negative-normalized-float
               least-positive-normalized-float
               least-negative-float
               least-positive-float
               most-negative-float
               most-positive-float
               ;; custom-print-functions
               ))
  (defvaralias (intern (format "cl-%s" var)) var))

(dolist (fun '(
               (get* . cl-get)
               (random* . cl-random)
               (rem* . cl-rem)
               (mod* . cl-mod)
               (round* . cl-round)
               (truncate* . cl-truncate)
               (ceiling* . cl-ceiling)
               (floor* . cl-floor)
               (rassoc* . cl-rassoc)
               (assoc* . cl-assoc)
               (member* . cl-member)
               (delete* . cl-delete)
               (remove* . cl-remove)
               (defsubst* . cl-defsubst)
               (sort* . cl-sort)
               (function* . cl-function)
               (defmacro* . cl-defmacro)
               (defun* . cl-defun)
               (mapcar* . cl-mapcar)

               remprop
               getf
               tailp
               list-length
               nreconc
               revappend
               concatenate
               subseq
               random-state-p
               make-random-state
               signum
               isqrt
               lcm
               gcd
               notevery
               notany
               every
               some
               mapcon
               mapcan
               mapl
               maplist
               map
               equalp
               coerce
               tree-equal
               nsublis
               sublis
               nsubst-if-not
               nsubst-if
               nsubst
               subst-if-not
               subst-if
               subsetp
               nset-exclusive-or
               set-exclusive-or
               nset-difference
               set-difference
               nintersection
               intersection
               nunion
               union
               rassoc-if-not
               rassoc-if
               assoc-if-not
               assoc-if
               member-if-not
               member-if
               merge
               stable-sort
               search
               mismatch
               count-if-not
               count-if
               count
               position-if-not
               position-if
               position
               find-if-not
               find-if
               find
               nsubstitute-if-not
               nsubstitute-if
               nsubstitute
               substitute-if-not
               substitute-if
               substitute
               delete-duplicates
               remove-duplicates
               delete-if-not
               delete-if
               remove-if-not
               remove-if
               replace
               fill
               reduce
               compiler-macroexpand
               define-compiler-macro
               assert
               check-type
               typep
               deftype
               defstruct
               callf2
               callf
               letf*
               ;; letf
               rotatef
               shiftf
               remf
               psetf
               (define-setf-method . define-setf-expander)
               declare
               the
               locally
               multiple-value-setq
               multiple-value-bind
               symbol-macrolet
               macrolet
               progv
               psetq
               do-all-symbols
               do-symbols
               dotimes
               dolist
               do*
               do
               loop
               return-from
               return
               block
               etypecase
               typecase
               ecase
               case
               load-time-value
               eval-when
               destructuring-bind
               gentemp
               gensym
               pairlis
               acons
               subst
               adjoin
               copy-list
               ldiff
               list*
               cddddr
               cdddar
               cddadr
               cddaar
               cdaddr
               cdadar
               cdaadr
               cdaaar
               cadddr
               caddar
               cadadr
               cadaar
               caaddr
               caadar
               caaadr
               caaaar
               cdddr
               cddar
               cdadr
               cdaar
               caddr
               cadar
               caadr
               caaar
               tenth
               ninth
               eighth
               seventh
               sixth
               fifth
               fourth
               third
               endp
               rest
               second
               first
               svref
               copy-seq
               evenp
               oddp
               minusp
               plusp
               floatp-safe
               declaim
               proclaim
               nth-value
               multiple-value-call
               multiple-value-apply
               multiple-value-list
               values-list
               values
               pushnew
               decf
               incf

               dolist
               dotimes
               ))
  (let ((new (if (consp fun) (prog1 (cdr fun) (setq fun (car fun)))
               (intern (format "cl-%s" fun)))))
    (defalias new fun)))

(provide 'cl-lib)
;;; cl-lib.el ends here
