
namespace Schemin.Evaluate.Primitives
{
	public static class ScheminPrimitives
	{
		public static string Foldl = @"(define (foldl func accum lst)
					  (if (null? lst)
					    accum
					    (foldl func (func accum (car lst)) (cdr lst))))";

		public static string Foldr = @"(define (foldr func end lst)
					  (if (null? lst)
					    end
					    (func (car lst) (foldr func end (cdr lst)))))";

		public static string Filter = @"(define (filter pred lst) (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))";

		public static string Map = @"(define (map func lst) (foldr (lambda (x y) (cons (func x) y)) '() lst))";

		public static string Not = "(define (not x) (if x #f #t))";

		public static string Id = "(define (id obj) obj)";

		public static string Flip = "(define (flip func) (lambda (arg1 arg2) (func arg2 arg1)))";

		public static string Fold = "(define fold foldl)";

		public static string Reduce = "(define reduce fold)";

		public static string Unfold = @"(define (unfold func init pred)
						  (if (pred init)
						    (cons init '())
						    (cons init (unfold func (func init) pred))))";
	}
}

