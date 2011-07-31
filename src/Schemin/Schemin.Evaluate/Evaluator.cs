
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

		public IScheminType Evaluate(ScheminList ast, Environment global)
		{
			IScheminType last = null;

			foreach (IScheminType type in ast.List)
			{
				last = Evaluate(type, global, false, false);
			}

			return last;
		}

		public IScheminType Evaluate(IScheminType ast, Environment env, bool ignoreSymbols, bool suspendFunctions)
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
				return EvaluateAtom(ast, ignoreSymbols, env);
			}
			else if (IsA(ast, primitive))
			{
				return ast;
			}
			else if (IsA(ast, list))
			{
				ScheminList temp = (ScheminList) ast;

				if (IsEmptyList(temp))
				{
					return ast;
				}

				IScheminType headResult = Evaluate(temp.Car(), env, ignoreSymbols, suspendFunctions);

				if (IsEmptyList(temp.Cdr()))
				{
					return headResult;
				}
				else
				{
					IScheminType restResult = null;

					if (IsA(headResult, primitive))
					{
						restResult = EvaluateRestPrimitive(temp, headResult, env);
					}
					else if (IsA(headResult, atom))
					{
						// Need to handle lambda specially
						ScheminAtom tempAtom = (ScheminAtom) headResult;
						if (tempAtom.Name == "lambda")
						{
							ScheminLambda lam = new ScheminLambda(temp.Cdr());
							return lam;
						}
						else
						{
							restResult = Evaluate(temp.Cdr(), env, ignoreSymbols, suspendFunctions);
						}
					}
					else
					{
						restResult = Evaluate(temp.Cdr(), env, ignoreSymbols, suspendFunctions);
					}	


					if (IsA(headResult, primitive))
					{
						if (suspendFunctions)
						{
							return ConstructRemnantList(headResult, restResult);
						}

						return EvaluatePrimitive(temp, headResult, restResult, env);
					}
					if (IsA(headResult, lambda))
					{
						if (suspendFunctions)
						{
							return ConstructRemnantList(headResult, restResult);
						}

						return EvaluateLambda(temp, headResult, restResult, env);
					}
					else
					{
						return ConstructRemnantList(headResult, restResult);
					}
				}
			}
			else
			{
				// Something bad happened.
				return ast;
			}
		}

		public IScheminType ConstructRemnantList(IScheminType headResult, IScheminType restResult)
		{
			if (IsA(restResult, list))
			{
				ScheminList tempList = (ScheminList) restResult;
				if (tempList.Empty)
				{
					return headResult;
				}
				else
				{
					return new ScheminList(headResult, tempList);
				}
			}
			else
			{
				ScheminList tempList = new ScheminList(headResult);
				tempList.Append(restResult);
				return tempList;
			}
		}

		public IScheminType EvaluatePrimitive(ScheminList top, IScheminType headResult, IScheminType restResult, Environment env)
		{
			ScheminPrimitive prim = (ScheminPrimitive) headResult;

			if (IsA(restResult, list))
			{
				ScheminList tempArgList = (ScheminList) restResult;
				if (tempArgList.Empty)
				{
					// If the car is a primitive, that means we're calling with no args
					if (top.Car().GetType() == typeof(ScheminPrimitive))
					{
						return prim.Evaluate(tempArgList, env, this);
					}

					return prim;
				}
				else
				{
					return prim.Evaluate(tempArgList, env, this);
				}
			}
			else
			{
				ScheminList unaryArgList = new ScheminList(restResult);
				return prim.Evaluate(unaryArgList, env, this);
			}
		}

		public IScheminType EvaluateLambda(ScheminList top, IScheminType headResult, IScheminType restResult, Environment env)
		{
			// Again handling lambda with special care
			// Before the lambda is executed, we create a new environment for it so arguments don't clobber the scope
			// The parent environment needs to be copied into the new environment as well, to implement closures.

			ScheminLambda lam = (ScheminLambda) headResult;

			if (IsA(restResult, list))
			{
				ScheminList tempArgList = (ScheminList) restResult;
				if (tempArgList.Empty)
				{
					// If the first element is an atom, that means the lambda is being called with no args
					if (top.Car().GetType() == typeof(ScheminAtom))
					{
						// CloseOver hoists free variables to the closure's env
						Environment closure = new Environment();
						closure.CloseOver(env);
						closure.parent = env;

						Environment child = new Environment();
						child.parent = closure;

						return lam.Evaluate(tempArgList, this, child);
					}

					return lam;
				}
				else
				{
					Environment closure = new Environment();
					closure.CloseOver(env);
					closure.parent = env;

					Environment child = new Environment();
					child.parent = closure;

					return lam.Evaluate(tempArgList, this, child);
				}
			}
			else
			{
				Environment closure = new Environment();
				closure.CloseOver(env);
				closure.parent = env;

				Environment child = new Environment();
				child.parent = closure;

				ScheminList unaryArgList = new ScheminList(restResult);
				return lam.Evaluate(unaryArgList, this, child);
			}
		}

		public IScheminType EvaluateAtom(IScheminType ast, bool ignoreSymbols, Environment env)
		{
			ScheminAtom temp = (ScheminAtom) ast;
			if (temp.Name == "lambda")
			{
				return ast;
			}

			if (ignoreSymbols)
			{
				return ast;
			}
			else
			{
				IScheminType bound = GetEnvValueRecursive(temp, env);
				if (bound == null)
				{
					throw new Exception(string.Format("Error: Unbound atom: {0}", temp));
				}

				return bound;
			}
		}

		public IScheminType EvaluateRestPrimitive(ScheminList top, IScheminType headResult, Environment env)
		{
			ScheminPrimitive prim = (ScheminPrimitive) headResult;
			if (prim.Name == "define")
			{
				return Evaluate(top.Cdr(), env, true, false);
			}
			else if (prim.Name == "map")
			{
				// Need to suspend execution of primitives and lambdas for functions that can have them passed as arguments.
				return Evaluate(top.Cdr(), env, false, true);
			}
			else if (prim.Name == "if" || prim.Name == "let")
			{
				// don't evaluate the arguments yet if it's an if call
				return top.Cdr();
			}
			else
			{
				return Evaluate(top.Cdr(), env, false, false);
			}
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
