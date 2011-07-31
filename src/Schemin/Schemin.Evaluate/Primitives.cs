
namespace Schemin.Evaluate
{
	using System;
	using System.Text;
	using System.Collections.Generic;
	using System.Numerics;
	using System.Linq;
	using Cadenza.Collections;
	using Schemin.AST;

	public static class Primitives
	{
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Add;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Subtract;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Multiply;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Define;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> DumpEnv;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Quote;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Car;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Cdr;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Equal;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> If;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Cons;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Map;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> GreaterThan;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> LessThan;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Let;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Begin;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> SetBang;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Display;


		static Primitives()
		{
			Display = (list, env, eval) => {
				IScheminType toDisplay = list;
				Console.WriteLine(eval.Evaluate(toDisplay, env, false, false).ToString());

				return new ScheminList();
			};

			SetBang = (list, env, eval) => {
				ScheminAtom symbol = (ScheminAtom) list.Car();
				IScheminType definition = list.Cdr();

				// If there's less than 2 elements in the list, treat the define as if it's binding to a non-list.
				ScheminList temp = (ScheminList) definition;
				if (temp.List.Count() < 2)
				{
					definition = temp.Car();
				}
				
				if (env.HasValue(symbol))
				{
					env.RemoveBinding(symbol);
					env.AddBinding(symbol, definition);
				}
				else
				{
					throw new Exception("Unbound atom referenced: " + symbol.Name);
				}

				return new ScheminList();
			};

			Begin = (list, env, eval) => {
				IScheminType last = new ScheminList();
				foreach (IScheminType type in list.List)
				{
					last = eval.Evaluate(type, env, false, false);
				}

				return last;
			};

			Let = (list, env, eval) => {
				ScheminList bindings = (ScheminList) list.Car();
				IScheminType expression = list.Cdr().Car();

				Environment temporary = new Environment();
				temporary.parent = env;
				
				foreach (IScheminType type in bindings.List)
				{
					ScheminList binding = (ScheminList) type;
					ScheminAtom symbol = (ScheminAtom) binding.Car();
					IScheminType val = binding.Cdr().Car();

					temporary.AddBinding(symbol, eval.Evaluate(val, env, false, false));
				}

				return eval.Evaluate(expression, temporary, false, false);
			};

			Map = (list, env, eval) => {
				IScheminType toApply = (IScheminType) list.Car();
				ScheminList toMap = (ScheminList) list.Cdr();

				ScheminLambda lam;

				if (toApply.GetType() == typeof(ScheminAtom))
				{
					// we suspended symbol lookup and function eval so the lambda wouldn't get called on the list, so lookup the lambda now
					lam = (ScheminLambda) eval.Evaluate(toApply, env, false, false);
				}
				else
				{
					// we were passed an explicit lambda instead of a symbol to lookup
					lam = (ScheminLambda) toApply;
				}


				var mapped = toMap.List.Select(element => {
					var args = new ScheminList(element);
					return lam.Evaluate(args, eval, env);
				});

				return new ScheminList(new CachedSequence<IScheminType>(mapped));
			};

			If = (list, env, eval) => {
				IScheminType condition = list.Car();
				IScheminType then = list.Cdr().Car();
				IScheminType otherwise = list.Cdr().Cdr().Car();

				ScheminBool conditionResults = (ScheminBool) eval.Evaluate(condition, env, false, false);
				if (conditionResults.Value)
				{
					return eval.Evaluate(then, env, false, false);
				}
				else
				{
					return eval.Evaluate(otherwise, env, false, false);
				}
			};
				
			Equal = (list, env, eval) => {
				IScheminType last = list.Car();

				bool result = false;
				
				foreach (IScheminType type in list.Cdr().List)
				{
					if (last.Equals(type))
					{
						result = true;
					}
					else
					{
						result = false;
					}
				}

				return new ScheminBool(result);
			};

			Cons = (list, env, eval) => {
				IScheminType head = list.Car();
				IScheminType rest = list.Cdr().Car();

				var temp = new ScheminList(head);
				return temp.Append(rest);
			};

			Car = (list, env, eval) => {
				return list.Car();
			};

			Cdr = (list, env, eval) => {
				if (list.Cdr().List.Count() < 2)
				{
					return list.Cdr().Car();
				}
				return list.Cdr();
			};

			Quote = (list, env, eval) => {
				if (list.List.Count() > 1)
				{
					return list;
				}
				else
				{
					return list.Car();
				}
			};

			DumpEnv = (args, env, eval) => {
				StringBuilder builder = new StringBuilder();

				foreach (KeyValuePair<string, IScheminType> kvp in env.bindings)
				{
					builder.Append(string.Format("({0} => {1}), ", kvp.Key, kvp.Value));
				}

				return new ScheminString(builder.ToString());
			};

			Define = (args, env, eval) => {
				ScheminAtom symbol = (ScheminAtom) args.Car();
				IScheminType definition = args.Cdr();

				// If there's less than 2 elements in the list, treat the define as if it's binding to a non-list.
				ScheminList temp = (ScheminList) definition;
				if (temp.List.Count() < 2)
				{
					definition = temp.Car();
				}
				
				if (env.HasValue(symbol))
				{
					env.RemoveBinding(symbol);
					env.AddBinding(symbol, definition);
				}
				else
				{
					env.AddBinding(symbol, definition);
				}

				return new ScheminList();
			};

			GreaterThan = (args, env, eval) => {
				ScheminInteger first = (ScheminInteger) args.Car();
				ScheminInteger second = (ScheminInteger) args.Cdr().Car();

				if (first.Value > second.Value)
				{
					return new ScheminBool(true);
				}

				return new ScheminBool(false);
			};

			LessThan = (args, env, eval) => {
				ScheminInteger first = (ScheminInteger) args.Car();
				ScheminInteger second = (ScheminInteger) args.Cdr().Car();

				if (first.Value < second.Value)
				{
					return new ScheminBool(true);
				}

				return new ScheminBool(false);
			};

			Add = (args, env, eval) => {
				BigInteger result = new BigInteger(0);

				if (args.List.Count() < 2)
				{
					var first = (ScheminInteger) args.Car();
					return new ScheminInteger(first.Value * 1);
				}

				foreach (IScheminType type in args.List)
				{
					if (type.GetType() != typeof(ScheminList))
					{
						var temp = (ScheminInteger) type;
						result += temp.Value;
					}
				}

				return new ScheminInteger(result);
			};

			Subtract = (args, env, eval) => {
				var first = (ScheminInteger) args.Car();
				BigInteger result = first.Value;

				if (args.List.Count() < 2)
				{
					return new ScheminInteger(result * -1);
				}

				foreach (IScheminType type in args.Cdr().List)
				{
					if (type.GetType() != typeof(ScheminList))
					{
						var temp = (ScheminInteger) type;
						result -= temp.Value;
					}
				}

				return new ScheminInteger(result);
			};

			Multiply = (args, env, eval) => {
				BigInteger result = new BigInteger(1);

				foreach (IScheminType type in args.List)
				{
					if (type.GetType() != typeof(ScheminList))
					{
						var temp = (ScheminInteger) type;
						result = temp.Value * result;
					}
				}

				return new ScheminInteger(result);
			};
		}
	}
}

