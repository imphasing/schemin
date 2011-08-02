
namespace Schemin.Evaluate
{
	using System;
	using System.Text;
	using System.Collections;
	using System.Collections.Generic;
	using System.Linq;
	using Schemin.AST;
	using Cadenza;

	public class Evaluator
	{
		private Type integer = typeof(ScheminInteger);
		private Type atom = typeof(ScheminAtom);
		private Type list = typeof(ScheminList);
		private Type str = typeof(ScheminString);
		private Type primitive = typeof(ScheminPrimitive);
		private Type lambda = typeof(ScheminLambda);
		private Type boolean = typeof(ScheminBool);

		public EvaluatorState EvalState = EvaluatorState.Normal;

		public IScheminType Evaluate(ScheminList ast, Environment env)
		{
			IScheminType last = null;

			foreach (IScheminType type in ast.List)
			{
				last = EvaluateInternal(type, env);
			}

			return last;
		}

		public IScheminType EvaluateInternal(IScheminType ast, Environment env)
		{
			if (IsA(ast, integer))
			{
				return ast;
			}
			else if (IsA(ast, str))
			{
				return ast;
			}
			else if (IsA(ast, boolean))
			{
				return ast;
			}
			else if (IsA(ast, atom))
			{
				return EvalAtom(ast, env);
			}
			else if (IsA(ast, primitive))
			{
				return ast;
			}
			else if (IsA(ast, lambda))
			{
				return ast;
			}
			else if (IsA(ast, list))
			{
				ScheminList evalList = (ScheminList) ast;
				ScheminList complete = new ScheminList();

				if (IsEmptyList(ast))
				{
					return ast;
				}

				foreach (IScheminType type in evalList.List)
				{
					if (IsA(type, integer))
					{
						complete.Append(type);
					}
					else if (IsA(type, str))
					{
						complete.Append(type);
					}
					else if (IsA(type, boolean))
					{
						complete.Append(type);
					}
					else if (IsA(type, atom))
					{
						IScheminType atomResult = EvalAtom(type, env);
						complete.Append(atomResult);
					}
					else if (IsA(type, primitive))
					{
						ScheminPrimitive prim = (ScheminPrimitive) type;
						switch (prim.Name)
						{
							case "define":
								this.EvalState = EvaluatorState.DefineArgs;
								break;
							case "lambda":
								this.EvalState = EvaluatorState.LambdaArgs;
								break;
							case "quote":
								this.EvalState = EvaluatorState.QuoteArgs;
								break;
							case "let":
								this.EvalState = EvaluatorState.LetArgs;
								break;
						}

						complete.Append(type);
					}
					else if (IsA(type, lambda))
					{
						complete.Append(type);
					}
					else if (IsA(type, list))
					{
						switch (this.EvalState)
						{
							case EvaluatorState.LambdaArgs:
								complete.Append(type);
								continue;
							case EvaluatorState.QuoteArgs:
								complete.Append(type);
								continue;
							case EvaluatorState.LetArgs:
								complete.Append(type);
								continue;
						}

						IScheminType listResult = EvaluateInternal(type, env);
						complete.Append(listResult);
					}
				}

				IScheminType functionPosition = complete.Car();
				ScheminList functionArgs = complete.Cdr();


				if (IsA(functionPosition, primitive))
				{
					ScheminPrimitive prim = (ScheminPrimitive) functionPosition;
					return prim.Evaluate(functionArgs, env, this);
				}
				// don't forget to make lambda a real primitive type from the parser, none of this symbol mangling shit
				else if (IsA(functionPosition, lambda))
				{
					ScheminLambda lam = (ScheminLambda) functionPosition;
					Environment closure = new Environment();
					closure.CloseOver(env);
					closure.parent = env;

					Environment child = new Environment();
					child.parent = closure;
					return lam.Evaluate(functionArgs, this, child);
				}
				else
				{
					throw new Exception("non-function in function position: " + functionPosition.ToString());
				}
			}
			else
			{
				// something weird happened
				return ast;
			}
		}


		public IScheminType EvalAtom(IScheminType ast, Environment env)
		{
			switch (this.EvalState)
			{
				case EvaluatorState.DefineArgs:
					return ast;
				case EvaluatorState.LambdaArgs:
					return ast;
				case EvaluatorState.LetArgs:
					return ast;
			}

			ScheminAtom temp = (ScheminAtom) ast;

			IScheminType bound = GetEnvValueRecursive(temp, env);
			if (bound == null)
			{
				throw new Exception(string.Format("Error: Unbound atom: {0}", temp));
			}

			return bound;
		}

		public bool IsA(IScheminType ast, Type type)
		{
			if (ast.GetType() == type)
			{
				return true;
			}

			return false;
		}

		public bool IsEmptyList(IScheminType type)
		{
			if (type.GetType() == typeof(ScheminList))
			{
				ScheminList temp = (ScheminList) type;
				if (temp.Empty == true)
				{
					return true;
				}
			}

			return false;
		}

		public IScheminType GetEnvValueRecursive(ScheminAtom symbol, Environment env)
		{
			if (env.HasValue(symbol))
			{
				return env.bindings[symbol.Name];
			}

			if (env.parent == null)
			{
				return null;
			}

			return GetEnvValueRecursive(symbol, env.parent);
		}
	}
}
