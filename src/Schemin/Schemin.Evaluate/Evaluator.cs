
namespace Schemin.Evaluate
{
	using System;
	using System.Text;
	using System.Collections;
	using System.Collections.Generic;
	using System.Linq;
	using Schemin.AST;
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

			foreach (IScheminType type in ast)
			{
				last = EvaluateInternal(type, env);
			}

			return last;
		}

		public IScheminType EvaluateInternal(IScheminType ast, Environment env)
		{
			if ((ast as ScheminInteger) != null)
			{
				return ast;
			}
			else if ((ast as ScheminString) != null)
			{
				return ast;
			}
			else if ((ast as ScheminBool) != null)
			{
				return ast;
			}
			else if ((ast as ScheminAtom) != null)
			{
				return EvalAtom(ast, env);
			}
			else if ((ast as ScheminPrimitive) != null)
			{
				return ast;
			}
			else if ((ast as ScheminLambda) != null)
			{
				return ast;
			}
			else if ((ast as ScheminList) != null)
			{
				ScheminList evalList = (ScheminList) ast;
				ScheminList complete = new ScheminList();

				if (IsEmptyList(ast))
				{
					return ast;
				}

				foreach (IScheminType type in evalList)
				{
					if ((type as ScheminInteger) != null)
					{
						complete.Append(type);
					}
					else if ((type as ScheminString) != null)
					{
						complete.Append(type);
					}
					else if ((type as ScheminBool) != null)
					{
						complete.Append(type);
					}
					else if ((type as ScheminAtom) != null)
					{
						IScheminType atomResult = EvalAtom(type, env);
						complete.Append(atomResult);
					}
					else if ((type as ScheminPrimitive) != null)
					{
						ScheminPrimitive prim = (ScheminPrimitive) type;
						SetStatePrimitive(prim);
						complete.Append(type);
					}
					else if ((type as ScheminLambda) != null)
					{
						complete.Append(type);
					}
					else if ((type as ScheminList) != null)
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


				if ((functionPosition as ScheminPrimitive) != null)
				{
					ScheminPrimitive prim = (ScheminPrimitive) functionPosition;
					return prim.Evaluate(functionArgs, env, this);
				}
				else if ((functionPosition as ScheminLambda) != null)
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
					// only ignore the FIRST symbol after a define
					this.EvalState = EvaluatorState.Normal;
					return ast;
				case EvaluatorState.LambdaArgs:
					return ast;
				case EvaluatorState.LetArgs:
					return ast;
				case EvaluatorState.IfArgs:
					return ast;
				case EvaluatorState.SetBangArgs:
					// only ignore the first argument to set!
					this.EvalState = EvaluatorState.Normal;
					return ast;
			}

			ScheminAtom temp = (ScheminAtom) ast;

			IScheminType bound = GetEnvValue(temp, env);
			if (bound == null)
			{
				throw new UnboundAtomException(string.Format("Unbound atom: {0}", temp));
			}


			return bound;
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

		public IScheminType GetEnvValue(ScheminAtom symbol, Environment env)
		{
			Environment parent = env;
			while (parent != null)
			{
				if (parent.HasValue(symbol))
				{
					return parent.bindings[symbol.Name];
					
				}

				parent = parent.parent;
			}

			return null;
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
				case "set!":
					this.EvalState = EvaluatorState.SetBangArgs;
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
