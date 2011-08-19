namespace Schemin.Evaluate.Primitives
{
	using System;
	using System.Text;
	using System.Collections.Generic;
	using System.Linq;
	using Schemin.AST;
	using Schemin.Evaluate;
	using Environment = Schemin.Evaluate.Environment;

	public static class ListOperations
	{
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Car;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Cdr;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Cadr;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Cddr;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Cons;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Length;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> List;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Append;

		public static string Map;
		public static string Filter;
        public static string Foldl; 
        public static string Foldr;

		static ListOperations()
		{

			Length = (list, env, eval) => {
				ScheminList listArg = (ScheminList) list.Car();
				return new ScheminInteger(listArg.Length);
			};

			Cons = (list, env, eval) => {
				IScheminType head = list.Car();
				IScheminType rest = list.Cdr().Car();

				if (rest.GetType() == typeof(ScheminList))
				{
					ScheminList temp = (ScheminList) rest;
					return new ScheminList(head, temp);
				}

				var append = new ScheminList(head);
				append.Append(rest);
				return append; 
			};

			Car = (list, env, eval) => {
				ScheminList listArg = (ScheminList) list.Car();
				return listArg.Car();
			};

			Cdr = (list, env, eval) => {
				ScheminList listArg = (ScheminList) list.Car();
				return listArg.Cdr();
			};

			Cadr = (list, env, eval) => {
				ScheminList listArg = (ScheminList) list.Car();
				return listArg.Cdr().Car();
			};

			Cddr = (list, env, eval) => {
				ScheminList listArg = (ScheminList) list.Car();
				return listArg.Cdr().Cdr();
			};

			List = (list, env, eval) => {
				IScheminType args = list;
				ScheminList ret = new ScheminList();

				if (args.GetType() == typeof(ScheminList))
				{
					ScheminList temp = (ScheminList) args;
					foreach (IScheminType type in temp)
					{
						ret.Append(type);
					}
				}
				else
				{
					ret.Append(args);
				}

				return ret;
			};

			Append = (list, env, eval) => {
				ScheminList appended = new ScheminList();

				foreach (IScheminType type in list)
				{
					if (type.GetType() == typeof(ScheminList))
					{
						ScheminList temp = (ScheminList) type;

						if (temp.Empty)
						{
							continue;
						}

						foreach (IScheminType subType in temp)
						{
							appended.Append(subType);
						}
					}
					else
					{
						throw new InvalidOperationException(string.Format("Non-list argument given to append: {0}", type));
					}
				}

				return appended;
			};


			Foldl = @"(define (foldl func accum lst)
                      (if (null? lst)
                        accum
                        (foldl func (func accum (car lst)) (cdr lst))))";

            Foldr = @"(define (foldr func end lst)
                        (if (null? lst)
                            end
                            (func (car lst) (foldr func end (cdr lst)))))";

            Filter = @"(define (filter pred lst) (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))";

            Map = @"(define (map func lst) (foldr (lambda (x y) (cons (func x) y)) '() lst))";
		}

	}
}
