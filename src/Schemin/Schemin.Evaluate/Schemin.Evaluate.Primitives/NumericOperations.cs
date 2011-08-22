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

	public static class NumericOperations
	{
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Add;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Subtract;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Multiply;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Divide;

		static NumericOperations()
		{
			Add = (args, env, eval) => {
				bool dec = false;
				var first = (IScheminNumeric) args.Car();
				IScheminNumeric result = null;

				foreach (IScheminType type in args)
				{
					if ((type as ScheminDecimal) != null)
					{
						dec = true;
						result = new ScheminDecimal(0);
					}
				}

				if (!dec)
				{
					result = new ScheminInteger(0);
				}

				foreach (IScheminType type in args)
				{
					if ((type as IScheminNumeric) != null)
					{
						var temp = (IScheminNumeric) type;
						if (dec)
						{
							result = new ScheminDecimal(result.DecimalValue() + temp.DecimalValue());
						}
						else
						{
							result = new ScheminInteger(result.IntegerValue() + temp.IntegerValue());
						}
					}
				}


				return (IScheminType) result;
			};

			Subtract = (args, env, eval) => {
				bool dec = false;
				var first = (IScheminNumeric) args.Car();
				IScheminNumeric result = null;

				foreach (IScheminType type in args)
				{
					if ((type as ScheminDecimal) != null)
					{
						dec = true;
						result = new ScheminDecimal(first.DecimalValue());
					}
				}

				if (!dec)
				{
					result = new ScheminInteger(first.IntegerValue());
				}

				if (args.Length < 2)
				{
					if (dec)
					{
						return new ScheminDecimal(first.DecimalValue() * -1);
					}
					else
					{
						return new ScheminInteger(first.IntegerValue() * -1);
					}
				}

				foreach (IScheminType type in args.Cdr())
				{
					if ((type as IScheminNumeric) != null)
					{
						var temp = (IScheminNumeric) type;
						if (dec)
						{
							result = new ScheminDecimal(result.DecimalValue() - temp.DecimalValue());
						}
						else
						{
							result = new ScheminInteger(result.IntegerValue() - temp.IntegerValue());
						}
					}
				}


				return (IScheminType) result;
			};

			Multiply = (args, env, eval) => {
				bool dec = false;
				var first = (IScheminNumeric) args.Car();
				IScheminNumeric result = null;

				foreach (IScheminType type in args)
				{
					if ((type as ScheminDecimal) != null)
					{
						dec = true;
						result = new ScheminDecimal(1);
					}
				}

				if (!dec)
				{
					result = new ScheminInteger(1);
				}

				foreach (IScheminType type in args)
				{
					if ((type as IScheminNumeric) != null)
					{
						var temp = (IScheminNumeric) type;
						if (dec)
						{
							result = new ScheminDecimal(result.DecimalValue() * temp.DecimalValue());
						}
						else
						{
							result = new ScheminInteger(result.IntegerValue() * temp.IntegerValue());
						}
					}
				}


				return (IScheminType) result;
			};

			Divide = (args, env, eval) => {
				bool dec = false;
				var first = (IScheminNumeric) args.Car();
				IScheminNumeric result = null;

				foreach (IScheminType type in args)
				{
					if ((type as ScheminDecimal) != null)
					{
						dec = true;
						result = new ScheminDecimal(first.DecimalValue());
					}
				}

				if (!dec)
				{
					result = new ScheminInteger(first.IntegerValue());
				}

				foreach (IScheminType type in args.Cdr())
				{
					if ((type as IScheminNumeric) != null)
					{
						var temp = (IScheminNumeric) type;
						if (dec)
						{
							result = new ScheminDecimal(result.DecimalValue() / temp.DecimalValue());
						}
						else
						{
							result = new ScheminInteger(result.IntegerValue() / temp.IntegerValue());
						}
					}
				}

				return (IScheminType) result;
			};
		}
	}
}
