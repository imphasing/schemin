
namespace Schemin.Evaluate
{
	using System;
	using System.Collections;
	using System.Linq;
	using Schemin.AST;
	using Cadenza;

	public class Evaluator
	{
		public IScheminType Evaluate(IScheminType ast, Environment env)
		{
			IScheminType returnType = null;

			if (ast.GetType() == typeof(ScheminInteger))
			{
				returnType = (ScheminInteger) ast;
			}
			else if (ast.GetType() == typeof(ScheminString))
			{
				returnType = (ScheminString) ast;
			}
			else if (ast.GetType() == typeof(ScheminAtom))
			{
				if (env.HasValue((ScheminAtom) ast))
				{
					returnType = env.GetValue((ScheminAtom) ast);
				}
				else
				{
					var tempAtom = (ScheminAtom) ast;
					switch (tempAtom.Name)
					{
						case "+":
						case "*":
							returnType = tempAtom;
							break;
						default:
							throw new Exception("Referenced unbound symbol: " + ast.ToString());
					}
				}
			}
			else if (ast.GetType() == typeof(ScheminList))
			{
				ScheminList tempList = (ScheminList) ast;

				if (tempList.Car() == null)
				{
					// return an empty list if we're done
					return ast;
				}

				if (tempList.Car().GetType() == typeof(ScheminAtom))
				{
					ScheminList args;

					switch (tempList.Car().ToString())
					{
						case "+":
							args = (ScheminList) Evaluate(tempList.Cdr(), env);
							returnType = AddOperation(args);
							break;
						case "*":
							args = (ScheminList) Evaluate(tempList.Cdr(), env);
							returnType = MultiplyOperation(args);
							break;
						default:
							returnType = tempList;
							break;
					}
				}
				else
				{
					IScheminType headResult = Evaluate(tempList.Car(), env);

					if (tempList.Cdr() != null)
					{
						IScheminType restResult = Evaluate(tempList.Cdr(), env);

						if (restResult.GetType() == typeof(ScheminList))
						{
							returnType = new ScheminList(headResult, (ScheminList) restResult);
						}
						else
						{
							returnType = new ScheminList(headResult).Append(restResult);
						}
					}
					else
					{
						returnType = new ScheminList(headResult);
					}
				}
			}

			return returnType;
		}

		public ScheminInteger AddOperation(ScheminList args)
		{
			int result = 0;

			foreach (IScheminType type in args.List)
			{
				if (type.GetType() != typeof(ScheminInteger))
				{
					throw new Exception(string.Format("AddOperation called with some args non-integers: {0}", args.ToString()));
				}

				var temp = (ScheminInteger) type;
				result += temp.Value;
			}

			return new ScheminInteger(result);
		}


		public ScheminInteger MultiplyOperation(ScheminList args)
		{
			int result = 1;

			foreach (IScheminType type in args.List)
			{
				if (type.GetType() != typeof(ScheminInteger))
				{
					throw new Exception(string.Format("MultiplyOperation called with some args non-integers: {0}", args.ToString()));
				}

				var temp = (ScheminInteger) type;
				result = temp.Value * result;
			}

			return new ScheminInteger(result);
		}
	}
}
