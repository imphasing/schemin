
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
				last = Evaluate(type, global, false);
			}

			return last;
		}

		public IScheminType Evaluate(IScheminType ast, Environment env, bool ignoreSymbols)
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
					if (env.HasValue(temp))
					{
						return env.GetValue(temp);
					}
					else
					{
						throw new Exception(string.Format("Error: Unbound atom: {0}", temp));
					}
				}
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

				IScheminType headResult = Evaluate(temp.Car(), env, ignoreSymbols);

				if (IsEmptyList(temp.Cdr()))
				{
					return headResult;
				}
				else
				{
					IScheminType restResult = null;
					// Switch up the Cdr() parsing if the headResult is define, or another operation that needs to ignore atoms
					if (IsA(headResult, primitive))
					{
						ScheminPrimitive prim = (ScheminPrimitive) headResult;
						if (prim.Name == "define")
						{
							restResult = Evaluate(temp.Cdr(), env, true);
						}
						else if (prim.Name == "if")
						{
							// don't evaluate the arguments yet if it's an if call
							restResult = temp.Cdr();
						}
						else
						{
							restResult = Evaluate(temp.Cdr(), env, false);
						}
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
							restResult = Evaluate(temp.Cdr(), env, false);
						}
					}
					else
					{
						restResult = Evaluate(temp.Cdr(), env, false);
					}	



					if (IsA(headResult, primitive))
					{
						ScheminPrimitive prim = (ScheminPrimitive) headResult;
						if (IsA(restResult, list))
						{
							ScheminList tempArgList = (ScheminList) restResult;
							if (tempArgList.Empty)
							{
								return prim;
							}
							else
							{
								return prim.Evaluate((ScheminList) restResult, env, this);
							}
						}
						else
						{
							ScheminList unaryArgList = new ScheminList(restResult);
							return prim.Evaluate(unaryArgList, env, this);
						}
					}
					if (IsA(headResult, lambda))
					{
						// Again handling lambda with special care
						ScheminLambda lam = (ScheminLambda) headResult;
						if (IsA(restResult, list))
						{
							ScheminList tempArgList = (ScheminList) restResult;
							if (tempArgList.Empty)
							{
								return lam;
							}
							else
							{
								return lam.Evaluate((ScheminList) restResult, this, env);
							}
						}
						else
						{
							ScheminList unaryArgList = new ScheminList(restResult);
							return lam.Evaluate(unaryArgList, this, env);
						}
					}
					else
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
				}
			}
			else
			{
				Console.WriteLine("CRAP");
				return ast;
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
	}
}
