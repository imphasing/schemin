
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
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Lambda;

		public static Func<ScheminList, Environment, Evaluator, IScheminType> Add;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Subtract;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Multiply;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> GreaterThan;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> LessThan;

		public static Func<ScheminList, Environment, Evaluator, IScheminType> Car;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Cdr;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Cadr;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Cddr;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Cons;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Length;

		public static Func<ScheminList, Environment, Evaluator, IScheminType> Equal;

		public static Func<ScheminList, Environment, Evaluator, IScheminType> If;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Map;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Let;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Begin;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> SetBang;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Display;

		public static Func<ScheminList, Environment, Evaluator, IScheminType> Define;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> DumpEnv;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Quote;

		static Primitives()
		{
			Lambda = (list, env, eval) => {
				ScheminLambda lam = new ScheminLambda(list);
				eval.EvalState = EvaluatorState.Normal;
				return lam;
			};

			Display = (list, env, eval) => {
				IScheminType toDisplay = list;
				Console.WriteLine(toDisplay.ToString());

				return new ScheminList();
			};

			SetBang = (list, env, eval) => {
				ScheminAtom symbol = (ScheminAtom) list.Car();
				IScheminType definition = list.Cdr().Car();

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
					last = eval.EvaluateInternal(type, env);
				}

				return last;
			};

			Let = (list, env, eval) => {
				eval.EvalState = EvaluatorState.Normal;
				bool isNamed = false;
				IScheminType first = list.Car();
				if (first.GetType() == typeof(ScheminAtom))
				{
					isNamed = true;
				}
					
				ScheminList bindings;
				IScheminType expression;
				if (isNamed)
				{
					bindings = (ScheminList) list.Cdr().Car();
					expression = list.Cdr().Cdr().Car();
				}
				else
				{
					bindings = (ScheminList) list.Car();
					expression = list.Cdr().Car();
				}

				if (!isNamed)
				{
					Environment temporary = new Environment();
					temporary.parent = env;

					foreach (IScheminType type in bindings.List)
					{
						ScheminList binding = (ScheminList) type;
						ScheminAtom symbol = (ScheminAtom) binding.Car();
						IScheminType val = binding.Cdr().Car();

						temporary.AddBinding(symbol, eval.EvaluateInternal(val, env));
					}

					return eval.EvaluateInternal(expression, temporary);
				}
				else
				{
					ScheminList argSymbols = new ScheminList();
					ScheminList argValues = new ScheminList();

					foreach (IScheminType type in bindings.List)
					{
						ScheminList binding = (ScheminList) type;
						ScheminAtom symbol = (ScheminAtom) binding.Car();
						IScheminType val = binding.Cdr().Car();

						IScheminType evaledVal = eval.EvaluateInternal(val, env);

						argSymbols.Append(symbol);
						argValues.Append(evaledVal);
					}

					ScheminList lambdaArgs = new ScheminList(argSymbols);
					lambdaArgs = lambdaArgs.Append(expression);

					ScheminLambda proc = new ScheminLambda(lambdaArgs);
					Environment temporary = new Environment();
					temporary.parent = env;

					temporary.AddBinding((ScheminAtom) first, proc);

					return proc.Evaluate(argValues, eval, temporary);
				}
			};

			Map = (list, env, eval) => {
				IScheminType toApply = (IScheminType) list.Car();
				ScheminList toMap = (ScheminList) list.Cdr().Car();

				ScheminLambda lam;

				if (toApply.GetType() == typeof(ScheminAtom))
				{
					// we suspended symbol lookup and function eval so the lambda wouldn't get called on the list, so lookup the lambda now
					lam = (ScheminLambda) eval.EvaluateInternal(toApply, env);
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

				ScheminBool conditionResults = (ScheminBool) eval.EvaluateInternal(condition, env);
				if (conditionResults.Value)
				{
					return eval.EvaluateInternal(then, env);
				}
				else
				{
					return eval.EvaluateInternal(otherwise, env);
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

			Length = (list, env, eval) => {
				ScheminList listArg = (ScheminList) list.Car();
				return new ScheminInteger(listArg.List.Count());
			};

			Cons = (list, env, eval) => {
				IScheminType head = list.Car();
				IScheminType rest = list.Cdr();

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

			Quote = (list, env, eval) => {
				IScheminType arg = list.Car();
				eval.EvalState = EvaluatorState.Normal;
				return arg;
			};

			DumpEnv = (args, env, eval) => {
				StringBuilder builder = new StringBuilder();

				foreach (KeyValuePair<string, IScheminType> kvp in env.bindings)
				{
					builder.Append(string.Format("({0} => {1}), ", kvp.Key, kvp.Value));
				}

				Console.WriteLine(builder.ToString());

				return new ScheminList();
			};

			Define = (args, env, eval) => {
				ScheminAtom symbol = (ScheminAtom) args.Car();
				IScheminType definition = args.Cdr().Car();

				if (env.HasValue(symbol))
				{
					env.RemoveBinding(symbol);
					env.AddBinding(symbol, definition);
				}
				else
				{
					env.AddBinding(symbol, definition);
				}

				eval.EvalState = EvaluatorState.Normal;
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

