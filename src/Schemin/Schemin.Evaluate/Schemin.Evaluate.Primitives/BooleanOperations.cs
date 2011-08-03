namespace Schemin.Evaluate.Primitives
{
	using System;
	using System.Text;
	using System.Collections.Generic;
	using System.Numerics;
	using System.Linq;
	using Cadenza.Collections;
	using Schemin.AST;
	using Schemin.Evaluate;
	using Environment = Schemin.Evaluate.Environment;

	public static class BooleanOperations
	{
		public static Func<ScheminList, Environment, Evaluator, IScheminType> GreaterThan;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> LessThan;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> LessThanOr;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Equal;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Null;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Not;

		static BooleanOperations()
		{

			Equal = (list, env, eval) => {
				IScheminType last = list.Car();

				bool result = false;

				foreach (IScheminType type in list.Cdr().List)
				{
					if (last.Equals(type))
					{
						result = true;
					}
					else
					{
						result = false;
					}
				}

				return new ScheminBool(result);
			};

			Not = (args, env, eval) => {
				IScheminType first = args.Car();
				
				if (first.GetType() == typeof(ScheminBool))
				{
					ScheminBool temp = (ScheminBool) first;
					return new ScheminBool(!temp.Value);
				}

				return new ScheminBool(false);
			};

			GreaterThan = (args, env, eval) => {
				ScheminInteger first = (ScheminInteger) args.Car();
				ScheminInteger second = (ScheminInteger) args.Cdr().Car();

				if (first.Value > second.Value)
				{
					return new ScheminBool(true);
				}

				return new ScheminBool(false);
			};

			LessThan = (args, env, eval) => {
				ScheminInteger first = (ScheminInteger) args.Car();
				ScheminInteger second = (ScheminInteger) args.Cdr().Car();

				if (first.Value < second.Value)
				{
					return new ScheminBool(true);
				}

				return new ScheminBool(false);
			};

			LessThanOr = (args, env, eval) => {
				ScheminInteger first = (ScheminInteger) args.Car();
				ScheminInteger second = (ScheminInteger) args.Cdr().Car();

				if (first.Value <= second.Value)
				{
					return new ScheminBool(true);
				}

				return new ScheminBool(false);
			};

			Null = (list, env, eval) => {
				ScheminList listArg = (ScheminList) list.Car();

				if (listArg.Empty)
				{
					return new ScheminBool(true);
				}

				return new ScheminBool(false);
			};
		}
	}
}
