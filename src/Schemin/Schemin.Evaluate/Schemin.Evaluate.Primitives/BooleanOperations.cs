namespace Schemin.Evaluate.Primitives
{
	using System;
	using System.Text;
	using System.Collections.Generic;
	using System.Numerics;
	using System.Linq;
	using Schemin.AST;
	using Schemin.Evaluate;
	using Environment = Schemin.Evaluate.Environment;

	public static class BooleanOperations
	{
		public static Func<ScheminList, Environment, Evaluator, IScheminType> GreaterThan;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> GreaterThanOr;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> LessThan;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> LessThanOr;

		public static Func<ScheminList, Environment, Evaluator, IScheminType> Equal;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Null;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> And;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Or;

		public static Func<ScheminList, Environment, Evaluator, IScheminType> Boolean;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Symbol;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Procedure;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Pair;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Number;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> String;

		static BooleanOperations()
		{
			Boolean = (list, env, eval) => {
				IScheminType type = list.Car();

				if ((type as ScheminBool) != null)
				{
					return ScheminBool.True;
				}

				return ScheminBool.False;
			};

			Symbol = (list, env, eval) => {
				IScheminType type = list.Car();

				if ((type as ScheminAtom) != null)
				{
					return ScheminBool.True;
				}

				return ScheminBool.False;
			};

			Procedure = (list, env, eval) => {
				IScheminType type = list.Car();

				if ((type as ScheminPrimitive) != null || (type as ScheminLambda) != null || (type as ScheminContinuation) != null)
				{
					return ScheminBool.True;
				}

				return ScheminBool.False;
			};

			Pair = (list, env, eval) => {
				IScheminType type = list.Car();

				if ((type as ScheminList) != null)
				{
					ScheminList temp = (ScheminList) type;
					if (temp.Length == 2)
					{
						return ScheminBool.True;
					}
				}

				return ScheminBool.False;
			};

			Number = (list, env, eval) => {
				IScheminType type = list.Car();

				if ((type as ScheminInteger) != null || (type as ScheminDecimal) != null)
				{
					return ScheminBool.True;
				}

				return ScheminBool.False;
			};

			String = (list, env, eval) => {
				IScheminType type = list.Car();

				if ((type as ScheminString) != null)
				{
					return ScheminBool.True;
				}

				return ScheminBool.False;
			};

			Equal = (list, env, eval) => {
				IScheminType last = list.Car();

				bool result = false;

				foreach (IScheminType type in list.Cdr())
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

				return ScheminBool.GetValue(result);
			};

			And = (args, env, eval) => {
				ScheminList nextCycle = new ScheminList();
				nextCycle.UnQuote();
				nextCycle.Append(new ScheminPrimitive(Primitives.BooleanOperations.And, "and"));

				if (args.Car().BoolValue() == ScheminBool.False)
				{
					return ScheminBool.False;
				}

				if (args.Length == 1)
				{
					return args.Car().BoolValue();
				}
				else
				{
					bool first = true;
					foreach (IScheminType type in args)
					{
						if (!first)
						{
							nextCycle.Append(type);
						}

						first = false;
					}
				}

				return nextCycle;
			};

			Or = (args, env, eval) => {
				ScheminList nextCycle = new ScheminList();
				nextCycle.UnQuote();
				nextCycle.Append(new ScheminPrimitive(Primitives.BooleanOperations.Or, "or"));

				if (args.Car().BoolValue() == ScheminBool.True)
				{
					return ScheminBool.True;
				}

				if (args.Length == 1)
				{
					return args.Car().BoolValue();
				}
				else
				{
					bool first = true;
					foreach (IScheminType type in args)
					{
						if (!first)
						{
							nextCycle.Append(type);
						}

						first = false;
					}
				}

				return nextCycle;
			};

			GreaterThan = (args, env, eval) => {
				IScheminNumeric first = (IScheminNumeric) args.Car();
				IScheminNumeric second = (IScheminNumeric) args.Cdr().Car();

				if ((first as ScheminDecimal) != null || (second as ScheminDecimal) != null)
				{
					if (first.DecimalValue() > second.DecimalValue())
					{
						return ScheminBool.True;
					}
				}
				else
				{
					if (first.IntegerValue() > second.IntegerValue())
					{
						return ScheminBool.True;
					}
				}

				return ScheminBool.False;
			};

			GreaterThanOr = (args, env, eval) => {
				IScheminNumeric first = (IScheminNumeric) args.Car();
				IScheminNumeric second = (IScheminNumeric) args.Cdr().Car();

				if ((first as ScheminDecimal) != null || (second as ScheminDecimal) != null)
				{
					if (first.DecimalValue() >= second.DecimalValue())
					{
						return ScheminBool.True;
					}
				}
				else
				{
					if (first.IntegerValue() >= second.IntegerValue())
					{
						return ScheminBool.True;
					}
				}

				return ScheminBool.False;
			};

			LessThan = (args, env, eval) => {
				IScheminNumeric first = (IScheminNumeric) args.Car();
				IScheminNumeric second = (IScheminNumeric) args.Cdr().Car();

				if ((first as ScheminDecimal) != null || (second as ScheminDecimal) != null)
				{
					if (first.DecimalValue() < second.DecimalValue())
					{
						return ScheminBool.True;
					}
				}
				else
				{
					if (first.IntegerValue() < second.IntegerValue())
					{
						return ScheminBool.True;
					}
				}

				return ScheminBool.False;
			};

			LessThanOr = (args, env, eval) => {
				IScheminNumeric first = (IScheminNumeric) args.Car();
				IScheminNumeric second = (IScheminNumeric) args.Cdr().Car();

				if ((first as ScheminDecimal) != null || (second as ScheminDecimal) != null)
				{
					if (first.DecimalValue() <= second.DecimalValue())
					{
						return ScheminBool.True;
					}
				}
				else
				{
					if (first.IntegerValue() <= second.IntegerValue())
					{
						return ScheminBool.True;
					}
				}

				return ScheminBool.False;
			};

			Null = (list, env, eval) => {
				ScheminList listArg = (ScheminList) list.Car();

				if (listArg.Empty)
				{
					return ScheminBool.True;
				}

				return ScheminBool.False;
			};
		}
	}
}
