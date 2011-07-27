
namespace Schemin.Evaluate
{
	using System;
	using System.Text;
	using System.Collections.Generic;
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


		static Primitives()
		{
			If = (list, env, eval) => {
				ScheminList condition = (ScheminList) list.Car();
				ScheminList then = (ScheminList) list.Cdr().Car();
				ScheminList otherwise = (ScheminList) list.Cdr().Cdr().Car();

				ScheminBool conditionResults = (ScheminBool) eval.Evaluate(condition, env, false);
				if (conditionResults.Value)
				{
					return eval.Evaluate(then, env, false);
				}
				else
				{
					return eval.Evaluate(otherwise, env, false);
				}
			};
				
			Equal = (list, env, eval) => {
				ScheminInteger last = (ScheminInteger) list.Car();

				bool result = false;
				
				foreach (IScheminType type in list.Cdr().List)
				{
					var temp = (ScheminInteger) type;

					if (last.Value == temp.Value)
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

			Car = (list, env, eval) => {
				return list.Car();
			};

			Cdr = (list, env, eval) => {
				return list.Cdr();
			};

			Quote = (list, env, eval) => {
				return list;
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

			Add = (args, env, eval) => {
				int result = 0;

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
				int result = first.Value;

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
				int result = 1;

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

