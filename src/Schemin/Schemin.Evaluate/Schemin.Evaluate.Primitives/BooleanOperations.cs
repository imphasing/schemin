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
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Not;
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
					return new ScheminBool(true);
				}

				return new ScheminBool(false);
			};

			Symbol = (list, env, eval) => {
				IScheminType type = list.Car();

				if ((type as ScheminAtom) != null)
				{
					return new ScheminBool(true);
				}

				return new ScheminBool(false);
			};

			Procedure = (list, env, eval) => {
				IScheminType type = list.Car();

				if ((type as ScheminPrimitive) != null || (type as ScheminLambda) != null)
				{
					return new ScheminBool(true);
				}

				return new ScheminBool(false);
			};

			Pair = (list, env, eval) => {
				IScheminType type = list.Car();

				if ((type as ScheminList) != null)
				{
					ScheminList temp = (ScheminList) type;
					if (temp.Length == 2)
					{
						return new ScheminBool(true);
					}
				}

				return new ScheminBool(false);
			};

			Number = (list, env, eval) => {
				IScheminType type = list.Car();

				if ((type as ScheminInteger) != null || (type as ScheminDecimal) != null)
				{
					return new ScheminBool(true);
				}

				return new ScheminBool(false);
			};

			String = (list, env, eval) => {
				IScheminType type = list.Car();

				if ((type as ScheminString) != null)
				{
					return new ScheminBool(true);
				}

				return new ScheminBool(false);
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

				return new ScheminBool(result);
			};

			Not = (args, env, eval) => {
				IScheminType first = args.Car();

				ScheminBool temp = first.BoolValue();
				return new ScheminBool(!temp.Value);
			};

			And = (args, env, eval) => {
				eval.EvalState = EvaluatorState.Normal;

				foreach (IScheminType type in args)
				{
					IScheminType result = eval.EvaluateInternal(type, env);
					ScheminBool conditionResult = result.BoolValue();

					if (conditionResult.Value != true)
					{
						return new ScheminBool(false);
					}
				}

				return new ScheminBool(true);
			};

			Or = (args, env, eval) => {
				eval.EvalState = EvaluatorState.Normal;

				foreach (IScheminType type in args)
				{
					IScheminType result = eval.EvaluateInternal(type, env);
					ScheminBool conditionResult = result.BoolValue();

					if (conditionResult.Value == true)
					{
						return new ScheminBool(true);
					}
				}

				return new ScheminBool(false);
			};

			GreaterThan = (args, env, eval) => {
				IScheminNumeric first = (IScheminNumeric) args.Car();
				IScheminNumeric second = (IScheminNumeric) args.Cdr().Car();

				if ((first as ScheminDecimal) != null || (second as ScheminDecimal) != null)
				{
					if (first.DecimalValue() > second.DecimalValue())
					{
						return new ScheminBool(true);
					}
				}
				else
				{
					if (first.IntegerValue() > second.IntegerValue())
					{
						return new ScheminBool(true);
					}
				}

				return new ScheminBool(false);
			};

			GreaterThanOr = (args, env, eval) => {
				IScheminNumeric first = (IScheminNumeric) args.Car();
				IScheminNumeric second = (IScheminNumeric) args.Cdr().Car();

				if ((first as ScheminDecimal) != null || (second as ScheminDecimal) != null)
				{
					if (first.DecimalValue() >= second.DecimalValue())
					{
						return new ScheminBool(true);
					}
				}
				else
				{
					if (first.IntegerValue() >= second.IntegerValue())
					{
						return new ScheminBool(true);
					}
				}

				return new ScheminBool(false);
			};

			LessThan = (args, env, eval) => {
				IScheminNumeric first = (IScheminNumeric) args.Car();
				IScheminNumeric second = (IScheminNumeric) args.Cdr().Car();

				if ((first as ScheminDecimal) != null || (second as ScheminDecimal) != null)
				{
					if (first.DecimalValue() < second.DecimalValue())
					{
						return new ScheminBool(true);
					}
				}
				else
				{
					if (first.IntegerValue() < second.IntegerValue())
					{
						return new ScheminBool(true);
					}
				}

				return new ScheminBool(false);
			};

			LessThanOr = (args, env, eval) => {
				IScheminNumeric first = (IScheminNumeric) args.Car();
				IScheminNumeric second = (IScheminNumeric) args.Cdr().Car();

				if ((first as ScheminDecimal) != null || (second as ScheminDecimal) != null)
				{
					if (first.DecimalValue() <= second.DecimalValue())
					{
						return new ScheminBool(true);
					}
				}
				else
				{
					if (first.IntegerValue() <= second.IntegerValue())
					{
						return new ScheminBool(true);
					}
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
