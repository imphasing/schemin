
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

		static GeneralOperations()
		{
			Lambda = (list, env, eval) => {
				ScheminLambda lam = new ScheminLambda(list, env);
				eval.EvalState = EvaluatorState.Normal;
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

			/*Begin = (list, env, eval) => {
				IScheminType last = new ScheminList();
				foreach (IScheminType type in list)
				{
					last = eval.EvaluateInternal(type, env);
				}

				return last;
			};*/

			/*LetRec = (list, env, eval) => {
                eval.EvalState = EvaluatorState.Normal;
                eval = new Evaluator();
				ScheminList bindings = (ScheminList) list.Car();
				ScheminList expression = list.Cdr();

				Environment temporary = new Environment();
				temporary.parent = env;

				// Fill bindings with an empty list for now
				foreach (IScheminType type in bindings)
				{
					ScheminList binding = (ScheminList) type;
					ScheminAtom symbol = (ScheminAtom) binding.Car();

					env.AddBinding(symbol, new ScheminList());
				}

				foreach (IScheminType type in bindings)
				{
					ScheminList binding = (ScheminList) type;
					ScheminAtom symbol = (ScheminAtom) binding.Car();
					IScheminType val = binding.Cdr().Car();

					temporary.AddBinding(symbol, eval.EvaluateInternal(val, temporary));
				}

				return eval.Evaluate(expression, temporary);
			};*/

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

            /*
			Let = (list, env, eval) => {
                eval.EvalState = EvaluatorState.Normal;
                eval = new Evaluator();

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
					expression = list.Cdr();
				}

				if (!isNamed)
				{
					Environment temporary = new Environment();
					temporary.parent = env;

					foreach (IScheminType type in bindings)
					{
						ScheminList binding = (ScheminList) type;
						ScheminAtom symbol = (ScheminAtom) binding.Car();
						IScheminType val = binding.Cdr().Car();

						temporary.AddBinding(symbol, eval.EvaluateInternal(val, env));
					}

					return eval.Evaluate((ScheminList) expression, temporary);
				}
				else
				{
					ScheminList argSymbols = new ScheminList();
					ScheminList argValues = new ScheminList();

					foreach (IScheminType type in bindings)
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

					Environment temporary = new Environment();
					temporary.parent = env;

					ScheminLambda proc = new ScheminLambda(lambdaArgs, temporary);
					temporary.AddBinding((ScheminAtom) first, proc);

					IScheminType result = proc.Evaluate(argValues, eval);

					return result;
				}
			};*/

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

			/*If = (list, env, eval) => {
                eval.EvalState = EvaluatorState.Normal;
                eval = new Evaluator();
				IScheminType condition = list.Car();
				IScheminType then = list.Cdr().Car();
				IScheminType otherwise = list.Cdr().Cdr().Car();

				ScheminBool conditionResults = eval.EvaluateInternal(condition, env).BoolValue();
				if (conditionResults.Value)
				{
					return eval.EvaluateInternal(then, env);
				}
				else
				{
					return eval.EvaluateInternal(otherwise, env);
				}
			};*/

			Quote = (list, env, eval) => {
				eval.EvalState = EvaluatorState.Normal;
				IScheminType arg = list.Car();

                if ((arg as ScheminList) != null)
                {
                    ScheminList tempList = (ScheminList)arg;
                    tempList.Quoted = true;
                }

				return arg;
			};

			DumpEnv = (args, env, eval) => {
				Console.WriteLine(env.ToString());
				return new ScheminList();
			};

			Define = (args, env, eval) => {
				bool deffun = false;
				eval.EvalState = EvaluatorState.Normal;

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

