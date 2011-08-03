
namespace Schemin.Evaluate.Primitives
{
	using System;
	using System.Text;
	using System.Collections.Generic;
	using System.Numerics;
	using System.Linq;
	using Cadenza.Collections;
	using Schemin.AST;
	using Schemin.Evaluate;
	using Environment = Schemin.Evaluate.Environment;

	public static class GeneralOperations
	{
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Lambda;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> If;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Let;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Begin;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> SetBang;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Display;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Define;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> DumpEnv;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Quote;

		static GeneralOperations()
		{
			Lambda = (list, env, eval) => {
				ScheminLambda lam = new ScheminLambda(list);
				eval.EvalState = EvaluatorState.Normal;
				return lam;
			};

			Display = (list, env, eval) => {
				IScheminType toDisplay = list.Car();
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
					throw new UnboundAtomException(string.Format("Unbound atom: {0}", symbol));
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

			If = (list, env, eval) => {
				eval.EvalState = EvaluatorState.Normal;
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

			Quote = (list, env, eval) => {
				eval.EvalState = EvaluatorState.Normal;
				IScheminType arg = list.Car();
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
				eval.EvalState = EvaluatorState.Normal;
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

				return new ScheminList();
			};

		}
	}
}

