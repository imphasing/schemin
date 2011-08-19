
namespace Schemin.Evaluate.Primitives
{
	using System;
	using System.Text;
	using System.Collections.Generic;
	using System.Numerics;
	using System.Linq;
	using Schemin.AST;
	using Schemin.Evaluate;
	using Environment = Schemin.Evaluate.Environment;

	public static class GeneralOperations
	{
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Lambda;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> If;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Cond;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Begin;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> SetBang;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Display;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Newline;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Define;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> DumpEnv;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Quote;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Let;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> LetRec;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> LetStar;
        public static Func<ScheminList, Environment, Evaluator, IScheminType> CallCC;

		static GeneralOperations()
		{
			Lambda = (list, env, eval) => {
				foreach (IScheminType type in list)
				{
					type.UnQuote();
				}

				ScheminLambda lam = new ScheminLambda(list, env);
				return lam;
			};

			Newline = (list, env, eval) => {
				Console.WriteLine();
				return new ScheminList();
			};

			Display = (list, env, eval) => {
				IScheminType toDisplay = list.Car();

				if (toDisplay.GetType() == typeof(ScheminString))
				{
					ScheminString temp = (ScheminString) toDisplay;
					Console.Write(temp.Value);
				}
				else
				{
					Console.Write(toDisplay.ToString());
				}

				return new ScheminList();
			};

			SetBang = (list, env, eval) => {
				ScheminAtom symbol = (ScheminAtom) list.Car();
				symbol.UnQuote();
				IScheminType definition = list.Cdr().Car();

				Environment parent = env;
				while (parent != null)
				{
					IScheminType value;
					parent.bindings.TryGetValue(symbol.Name, out value);

					if (value != null)
					{

						parent.RemoveBinding(symbol);
						parent.AddBinding(symbol, definition);
						return new ScheminList();
					}

					parent = parent.parent;
				}

				throw new UnboundAtomException(string.Format("Unbound atom: {0}", symbol));
			};

			Begin = (list, env, eval) => {
                return list.Last();
			};

			LetRec = (list, env, eval) =>
			{
				foreach (IScheminType type in list)
				{
					type.UnQuote();
				}

				ScheminList bindings = (ScheminList)list.Car();
				IScheminType expression = list.Cdr().Car();

				ScheminList args = new ScheminList();
				ScheminList argExps = new ScheminList();

				args.UnQuote();
				argExps.UnQuote();

				foreach (ScheminList bindingPair in bindings)
				{
					args.Append(bindingPair.Car());
					argExps.Append(bindingPair.Cdr().Car());
				}

				ScheminList body = new ScheminList();
				body.UnQuote();

				ScheminList next = args;
				ScheminList nextExp = argExps;
				while (next != null)
				{
					IScheminType symbol = next.Head;
					IScheminType exp = nextExp.Head;

					ScheminList setExp = new ScheminList(new ScheminPrimitive(GeneralOperations.SetBang, "set!"));
					setExp.UnQuote();
					setExp.Append(symbol);
					setExp.Append(exp);
					body.Append(setExp);

					next = next.Rest;
					nextExp = nextExp.Rest;
				}

				body.Append(expression);

				ScheminList lambdaDef = new ScheminList(args);
				lambdaDef.UnQuote();

				foreach (IScheminType type in body)
				{
					lambdaDef.Append(type);
				}

				Environment closure = env;
				ScheminLambda lam = new ScheminLambda(lambdaDef, closure);

				ScheminList toEvaluate = new ScheminList(lam);
				toEvaluate.UnQuote();

				foreach (IScheminType arg in argExps)
				{
					toEvaluate.Append(new ScheminList());
				}

				return toEvaluate;
			};

			/*LetStar = (list, env, eval) => {
			  eval.EvalState = EvaluatorState.Normal;
			  eval = new Evaluator();

			  ScheminList bindings = (ScheminList) list.Car();
			  IScheminType expression = list.Cdr();

			  Environment temporary = new Environment();
			  temporary.parent = env;

			  foreach (IScheminType type in bindings)
			  {
			  ScheminList binding = (ScheminList) type;
			  ScheminAtom symbol = (ScheminAtom) binding.Car();
			  IScheminType val = binding.Cdr().Car();

			  temporary.AddBinding(symbol, eval.EvaluateInternal(val, temporary));
			  }

			  return eval.Evaluate((ScheminList) expression, temporary);
			  };*/


			Let = (list, env, eval) => {
				foreach (IScheminType type in list)
				{
					type.UnQuote();
				}

				bool isNamed = false;
				if (list.Car().GetType() == typeof(ScheminAtom))
				{
					isNamed = true;
				}

				ScheminList bindings;
				ScheminList expression;

				if (!isNamed)
				{
					expression = list.Cdr();
					bindings = (ScheminList)list.Car();
				}
				else
				{
					expression = list.Cdr().Cdr();
					bindings = (ScheminList)list.Cdr().Car();
				}

				ScheminList args = new ScheminList();
				ScheminList argExps = new ScheminList();

				args.UnQuote();
				argExps.UnQuote();

				foreach (ScheminList bindingPair in bindings)
				{
					args.Append(bindingPair.Car());
					argExps.Append(bindingPair.Cdr().Car());
				}

                ScheminList lambdaDef = new ScheminList(args);
				lambdaDef.UnQuote();

                foreach (IScheminType type in expression)
                {
                    lambdaDef.Append(type);
                }

				Environment closure = env;
				if (isNamed)
				{
					closure = new Environment();
					closure.parent = env;
				}

				ScheminLambda lam = new ScheminLambda(lambdaDef, closure);

				if (isNamed)
				{
					ScheminAtom name = (ScheminAtom)list.Car();
					closure.AddBinding(name, lam);
				}

				ScheminList toEvaluate = new ScheminList(lam);
				toEvaluate.UnQuote();

				foreach (IScheminType arg in argExps)
				{
					toEvaluate.Append(arg);
				}

				return toEvaluate;
			};

			/*Cond = (list, env, eval) => {
			  eval.EvalState = EvaluatorState.Normal;
			  eval = new Evaluator();
			  ScheminList conditions = (ScheminList) list;

			  foreach (IScheminType type in conditions)
			  {
			  ScheminList expression = (ScheminList) type;
			  IScheminType condition = expression.Car();
			  ScheminList result = expression.Cdr();

			// check for the else condition first so we can skip anything that comes after
			if ((condition as ScheminAtom) != null)
			{
			ScheminAtom temp = (ScheminAtom) condition;
			if (temp.Name == "else")
			{
			return eval.Evaluate(result, env);
			}
			}

			ScheminBool conditionResults = eval.EvaluateInternal(condition, env).BoolValue();
			if (conditionResults.Value)
			{
			return eval.Evaluate(result, env);
			}
			else
			{
			continue;
			}
			}

			return new ScheminList();

			};*/

			If = (list, env, eval) => {
				eval = new Evaluator();
				ScheminBool condition = list.Car().BoolValue();
				IScheminType then = list.Cdr().Car();
				IScheminType otherwise = list.Cdr().Cdr().Car();

				if (condition.Value)
				{
					then.UnQuote();
					return then;
				}
				else
				{
					otherwise.UnQuote();
					return otherwise;
				}
			};

			Quote = (list, env, eval) => {
				IScheminType arg = list.Car();
				return arg;
			};

            CallCC = (list, env, eval) => {
                ScheminList applied = new ScheminList();
                applied.UnQuote();

                applied.Append(list.Car());
                applied.Append(new ScheminContinuation(eval.Stack));

                return applied;
            };

			DumpEnv = (args, env, eval) => {
				Console.WriteLine(env.ToString());
				return new ScheminList();
			};

			Define = (args, env, eval) => {
				bool deffun = false;

				if ((args.Car() as ScheminList) != null)
				{
					deffun = true;
				}

				if (!deffun)
				{
					ScheminAtom symbol = (ScheminAtom) args.Car();
					IScheminType definition = args.Cdr().Car();

					if (env.bindings.ContainsKey(symbol.Name))
					{
						env.RemoveBinding(symbol);
						env.AddBinding(symbol, definition);
					}
					else
					{
						env.AddBinding(symbol, definition);
					}

					return new ScheminList();
				}
				else
				{
					ScheminList arguments = (ScheminList) args.Car();
					ScheminList expression = args.Cdr();

					foreach (IScheminType type in expression)
					{
						type.UnQuote();
					}

					ScheminAtom name = (ScheminAtom) arguments.Car();
					ScheminList argSymbols = arguments.Cdr();

					ScheminList lamArgs = new ScheminList(argSymbols, expression);
					ScheminLambda lam = new ScheminLambda(lamArgs, env);

					if (env.bindings.ContainsKey(name.Name))
					{
						env.RemoveBinding(name);
						env.AddBinding(name, lam);
					}
					else
					{
						env.AddBinding(name, lam);
					}

					return new ScheminList();
				}

			};

		}
	}
}

