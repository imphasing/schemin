
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
		public IScheminType Evaluate(IScheminType ast, Environment env, bool ignoreSymbols)
		{
			if (ast.GetType() == typeof(ScheminInteger))
			{
				return ast;
			}
			else if (ast.GetType() == typeof(ScheminString))
			{
				return ast;
			}
			else if (ast.GetType() == typeof(ScheminAtom))
			{
				ScheminAtom atom = (ScheminAtom) ast;
				Console.WriteLine("Got an atom! " + atom.Name);
				if (ignoreSymbols)
				{
					return ast;
				}
				else
				{
					if (env.HasValue(atom))
					{
						return env.GetValue(atom);
					}
					else
					{
						throw new Exception(string.Format("Unbound atom: {0}", atom));
					}
				}
			}

			else if (ast.GetType() == typeof(ScheminList))
			{
				ScheminList list = (ScheminList) ast;

				if (list.Car() == null)
				{
					return null;
				}

				if (list.Car().GetType() == typeof(ScheminAtom))
				{
					switch (list.Car().ToString())
					{
						case "+":
							return AddOperation((ScheminList) Evaluate(list.Cdr(), env, false));
						case "*":
							return MultiplyOperation((ScheminList) Evaluate(list.Cdr(), env, false));
						case "-":
							return SubtractOperation((ScheminList) Evaluate(list.Cdr(), env, false));
						case "define":
							return DefineOperation(list.Cdr(), env);
						case "dumpenv":
							return DumpEnv(env);
						default:
							return ast;
					}
				}
				else
				{
					IScheminType headResult = Evaluate(list.Car(), env, false);

					if (list.Cdr() != null)
					{
						IScheminType restResult = Evaluate(list.Cdr(), env, false);

						if (restResult != null)
						{
							return new ScheminList(headResult, (ScheminList) restResult);
						}
						else
						{
							return new ScheminList(headResult);
						}

					}
					else
					{
						return new ScheminList(headResult);
					}
				}
			}
			else
			{
				Console.WriteLine("CRAP");
				return ast;
			}
		}

		public ScheminInteger AddOperation(ScheminList args)
		{
			int result = 0;

			if (args.List.Count() < 2)
			{
				return new ScheminInteger(result * 1);
			}

			foreach (IScheminType type in args.List)
			{
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
				var temp = (ScheminInteger) type;
				result = temp.Value * result;
			}

			return new ScheminInteger(result);
		}

		public ScheminInteger SubtractOperation(ScheminList args)
		{
			var first = (ScheminInteger) args.Car();
			int result = first.Value;
			
			if (args.List.Count() < 2)
			{
				return new ScheminInteger(result * -1);
			}
			else
			{
				foreach (IScheminType type in args.Cdr().List)
				{
					var temp = (ScheminInteger) type;
					result -= temp.Value;
				}

				return new ScheminInteger(result);
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

			return null;
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
	}
}
