
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

		public IScheminType Evaluate(IScheminType ast, Environment env)
		{
			if (IsA(ast, integer))
			{
				return ast;
			}
			else if (IsA(ast, str))
			{
				return ast;
			}
			else if (IsA(ast, atom))
			{
				ScheminAtom temp = (ScheminAtom) ast;
				if (env.HasValue(temp))
				{
					return env.GetValue(temp);
				}
				else
				{
					throw new Exception(string.Format("Unbound atom: {0}", temp));
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

				IScheminType headResult = Evaluate(temp.Car(), env);


				if (IsEmptyList(temp.Cdr()))
				{
					return headResult;
				}
				else
				{
					IScheminType restResult = Evaluate(temp.Cdr(), env);

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
								return prim.Evaluate((ScheminList) restResult);
							}
						}
						else
						{
							ScheminList unaryArgList = new ScheminList(restResult);
							return prim.Evaluate(unaryArgList);
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

		public IScheminType DefineOperation(ScheminList args, Environment env)
		{
			ScheminAtom symbol = (ScheminAtom) args.Car();
			IScheminType definition = args.Cdr();

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
		}

		public ScheminString DumpEnv(Environment env)
		{
			StringBuilder builder = new StringBuilder();

			foreach (KeyValuePair<string, IScheminType> kvp in env.bindings)
			{
				builder.Append(string.Format("({0} => {1}), ", kvp.Key, kvp.Value));
			}

			return new ScheminString(builder.ToString());
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
