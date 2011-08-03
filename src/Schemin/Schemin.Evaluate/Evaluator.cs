
namespace Schemin.Evaluate
{
	using System;
	using System.Text;
	using System.Collections;
	using System.Collections.Generic;
	using System.Linq;
	using Schemin.AST;
	using Cadenza;
	using Schemin.Evaluate.Primitives;

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
						SetStatePrimitive(prim);
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
							case EvaluatorState.IfArgs:
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

					Environment child = new Environment();
					child.parent = env;
					return lam.Evaluate(functionArgs, this, child);
				}
				else
				{
					throw new InvalidOperationException("Non-function in function position: " + functionPosition.ToString());
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
					this.EvalState = EvaluatorState.Normal;
					return ast;
				case EvaluatorState.LambdaArgs:
					return ast;
				case EvaluatorState.LetArgs:
					return ast;
				case EvaluatorState.IfArgs:
					return ast;
			}

			ScheminAtom temp = (ScheminAtom) ast;

			IScheminType bound = GetEnvValueRecursive(temp, env);
			if (bound == null)
			{
				throw new UnboundAtomException(string.Format("Unbound atom: {0}", temp));
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

		public void SetStatePrimitive(ScheminPrimitive prim)
		{
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
				case "if":
					this.EvalState = EvaluatorState.IfArgs;
					break;
			}
		}

		public void DefinePrimitives(Environment env)
		{
			var prebound = new Dictionary<string, Func<ScheminList, Environment, Evaluator, IScheminType>>();

			prebound.Add("+", NumericOperations.Add);
			prebound.Add("-", NumericOperations.Subtract);
			prebound.Add( "*", NumericOperations.Multiply);

			prebound.Add("car", ListOperations.Car);
			prebound.Add("cons", ListOperations.Cons);
			prebound.Add("cdr", ListOperations.Cdr);
			prebound.Add("cadr", ListOperations.Cadr);
			prebound.Add("cddr", ListOperations.Cddr);
			prebound.Add("length", ListOperations.Length);
			prebound.Add("list", ListOperations.List);
			prebound.Add("append", ListOperations.Append);
			prebound.Add("map", ListOperations.Map);
			prebound.Add("filter", ListOperations.Filter);
			prebound.Add("foldl", ListOperations.Foldl);

			prebound.Add("null?", BooleanOperations.Null);
			prebound.Add("=", BooleanOperations.Equal);
			prebound.Add("eq?", BooleanOperations.Equal);
			prebound.Add(">", BooleanOperations.GreaterThan);
			prebound.Add("<", BooleanOperations.LessThan);
			prebound.Add("<=", BooleanOperations.LessThanOr);
			prebound.Add("not", BooleanOperations.Not);

			prebound.Add("dumpenv", GeneralOperations.DumpEnv);
			prebound.Add("begin", GeneralOperations.Begin);
			prebound.Add("set!", GeneralOperations.SetBang);
			prebound.Add("display", GeneralOperations.Display);
			prebound.Add("newline", GeneralOperations.Newline);

			foreach (KeyValuePair<string, Func<ScheminList, Environment, Evaluator, IScheminType>> kvp in prebound)
			{
				var func = kvp.Value;
				string symbolValue = kvp.Key;

				ScheminAtom symbol = new ScheminAtom(symbolValue);
				ScheminPrimitive prim = new ScheminPrimitive(func, symbolValue);

				env.AddBinding(symbol, prim);
			}
		}
	}
}
