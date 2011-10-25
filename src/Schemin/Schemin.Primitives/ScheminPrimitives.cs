/* 
 * Copyright (c) 2011 Alex Fort 
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

namespace Schemin.Primitives
{
	public static class ScheminPrimitives
	{
		public static string Cadr = "(define (cadr list) (car (cdr list)))";
		public static string Cddr = "(define (cddr list) (cdr (cdr list)))";
		public static string Caddr = "(define (caddr list) (car (cdr (cdr list))))";

		public static string Foldl = @"(define (foldl func accum lst)
						 (if (null? lst)
						   accum
						   (foldl func (func accum (car lst)) (cdr lst))))";

		public static string Foldr = @"(define (foldr func end lst)
						 (if (null? lst)
						   end
						   (func (car lst) (foldr func end (cdr lst)))))";

		public static string Filter = @"(define (filter pred lst) (foldr (lambda (x y) (if (pred x) (cons x y) y)) (quote ()) lst))";

		public static string Map = @"(define (map func lst) (foldr (lambda (x y) (cons (func x) y)) (quote ()) lst))";

		public static string Not = "(define (not x) (if x #f #t))";

		public static string Id = "(define (id obj) obj)";

		public static string Flip = "(define (flip func) (lambda (arg1 arg2) (func arg2 arg1)))";

		public static string Fold = "(define fold foldl)";

		public static string Reduce = "(define reduce fold)";

		public static string Unfold = @"(define (unfold func init pred)
						  (if (pred init)
						    (cons init '())
						    (cons init (unfold func (func init) pred))))";

		public static string Reverse = "(define (reverse lst) (fold (flip cons) '() lst))";

		public static string Curry = "(define (curry func arg1) (lambda (arg) (apply func (cons arg1 (list arg)))))";

		public static string Compose = "(define (compose f g) (lambda (arg) (f (apply g arg))))";

		public static string Zero = "(define zero? (curry = 0))";

		public static string Positive = "(define positive? (curry < 0))";

		public static string Negative = "(define negative? (curry > 0))";

		public static string Odd = "(define (odd? num) (= (mod num 2) 1))";

		public static string Even = "(define (even? num) (= (mod num 2) 0))";

		public static string CallWithCC = "(define call-with-current-continuation call/cc)";

		public static string Error = "(define error display)";

		public static string Sum = "(define (sum . lst) (fold + 0 lst))";

		public static string Product = "(define (product . lst) (fold * 1 lst))";

		public static string Max = "(define (max first . num-list) (fold (lambda (old new) (if (> old new) old new)) first num-list))";

		public static string Min = "(define (min first . num-list) (fold (lambda (old new) (if (< old new) old new)) first num-list))";

		public static string MemHelper = "(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))";

		public static string Memq = "(define (memq obj lst) (fold (mem-helper (curry eq? obj) id) #f lst))";

		public static string Memv = "(define (memv obj lst) (fold (mem-helper (curry eqv? obj) id) #f lst))";

		public static string Member = "(define (member obj lst) (fold (mem-helper (curry equal? obj) id) #f lst))";

		public static string Assq = "(define (assq obj alist) (fold (mem-helper (curry eq? obj) car) #f alist))";

		public static string Assv = "(define (assv obj alist) (fold (mem-helper (curry eqv? obj) car) #f alist))";

		public static string Assoc = "(define (assoc obj alist) (fold (mem-helper (curry equal? obj) car) #f alist))";

		public static string DefineMacro = @"(define-rewriter define-macro
						      (lambda args
						        `(define-rewriter ,(car (car args)) (lambda ,(cdr (car args)) ,(car (cdr args))))))";
	}
}
