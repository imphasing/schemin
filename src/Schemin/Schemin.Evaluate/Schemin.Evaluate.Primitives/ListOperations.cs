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

		public static Func<ScheminList, Environment, Evaluator, IScheminType> Map;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Filter;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Foldl;

		static ListOperations()
		{

			Length = (list, env, eval) => {
				ScheminList listArg = (ScheminList) list.Car();
				return new ScheminInteger(0);
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


			Foldl = (list, env, eval) => {
				IScheminType func = (IScheminType) list.Car();
				IScheminType init = list.Cdr().Car();
				ScheminList toFold = (ScheminList) list.Cdr().Cdr().Car();


				IScheminType result;

				if (func.GetType() == typeof(ScheminPrimitive))
				{

					ScheminPrimitive proc = (ScheminPrimitive) func;
					result = toFold.Aggregate(init, (total, next) => {
							ScheminList args = new ScheminList(next);
							args.Append(total);

							return proc.Evaluate(args, env, eval);
							});
				}
				else
				{
					ScheminLambda lam = (ScheminLambda) func;

					result = toFold.Aggregate(init, (total, next) => {
							ScheminList args = new ScheminList(next);
							args.Append(total);

							return lam.Evaluate(args, eval, env);
							}); 
				}

				return result;
			};

			Filter = (list, env, eval) => {
				IScheminType toApply = (IScheminType) list.Car();
				ScheminList toFilter = (ScheminList) list.Cdr().Car();

				ScheminLambda lam = (ScheminLambda) toApply;

				if (toFilter.Empty)
				{
					return toFilter;
				}

				var filtered = toFilter.Where(element => {
						var args = new ScheminList(element);
						ScheminBool predResult = (ScheminBool) lam.Evaluate(args, eval, env);
						return predResult.Value;
						});

				if (filtered.Count() < 1)
				{
					return new ScheminList();
				}

				ScheminList temp = new ScheminList();
				foreach (IScheminType type in filtered)
				{
					temp.Append(type);
				}

				return temp;
			};

			Map = (list, env, eval) => {
				IScheminType toApply = (IScheminType) list.Car();
				ScheminList toMap = (ScheminList) list.Cdr().Car();

				if (toMap.Empty)
				{
					return toMap;
				}

				ScheminLambda lam = (ScheminLambda) toApply;

				var mapped = toMap.Select(element => {
						var args = new ScheminList(element);
						return lam.Evaluate(args, eval, env);
						});

				if (mapped.Count() < 1)
				{
					return new ScheminList();
				}

				ScheminList temp = new ScheminList();
				foreach (IScheminType type in mapped)
				{
					temp.Append(type);
				}

				return temp;
			};
		}

	}
}
