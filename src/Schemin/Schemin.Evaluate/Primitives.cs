
namespace Schemin.Evaluate
{
	using System;
	using System.Text;
	using System.Collections.Generic;
	using Schemin.AST;

	public static class Primitives
	{
		public static Func<ScheminList, Environment, IScheminType> Add;
		public static Func<ScheminList, Environment, IScheminType> Subtract;
		public static Func<ScheminList, Environment, IScheminType> Multiply;
		public static Func<ScheminList, Environment, IScheminType> Define;
		public static Func<ScheminList, Environment, IScheminType> DumpEnv;

		static Primitives()
		{
			DumpEnv = (args, env) => {
				StringBuilder builder = new StringBuilder();

				foreach (KeyValuePair<string, IScheminType> kvp in env.bindings)
				{
					builder.Append(string.Format("({0} => {1}), ", kvp.Key, kvp.Value));
				}

				return new ScheminString(builder.ToString());
			};


			Define = (args, env) => {
				ScheminAtom symbol = (ScheminAtom) args.Car();
				IScheminType definition = args.Cdr().Car();

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
			};

			Add = (args, env) => {
				int result = 0;

				if (args.List.Count() < 2)
				{
					var first = (ScheminInteger) args.Car();
					return new ScheminInteger(first.Value * 1);
				}

				foreach (IScheminType type in args.List)
				{
					if (type.GetType() != typeof(ScheminList))
					{
						var temp = (ScheminInteger) type;
						result += temp.Value;
					}
				}

				return new ScheminInteger(result);
			};

			Subtract = (args, env) => {
				var first = (ScheminInteger) args.Car();
				int result = first.Value;

				if (args.List.Count() < 2)
				{
					return new ScheminInteger(result * -1);
				}

				foreach (IScheminType type in args.Cdr().List)
				{
					if (type.GetType() != typeof(ScheminList))
					{
						var temp = (ScheminInteger) type;
						result -= temp.Value;
					}
				}

				return new ScheminInteger(result);
			};

			Multiply = (args, env) => {
				int result = 1;

				foreach (IScheminType type in args.List)
				{
					if (type.GetType() != typeof(ScheminList))
					{
						var temp = (ScheminInteger) type;
						result = temp.Value * result;
					}
				}

				return new ScheminInteger(result);
			};
		}
	}
}

