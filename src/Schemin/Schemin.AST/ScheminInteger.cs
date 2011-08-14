
namespace Schemin.AST
{
	using System.Numerics;
	using System;

	public class ScheminInteger : IScheminType, IScheminNumeric
	{
		public BigInteger Value;

		public ScheminInteger(int value)
		{
			this.Value = new BigInteger(value);
		}

		public ScheminInteger(BigInteger value)
		{
			this.Value = value;
		}

		public override string ToString()
		{
			return Value.ToString();
		}

        public bool Quoted()
        {
            return false;
        }

        public void Quote()
        {
        }

        public void UnQuote()
        {
        }

		public bool Equals(IScheminType type)
		{
			if (this.GetType() != type.GetType())
			{
				return false;
			}

			ScheminInteger temp = (ScheminInteger) type;
			if (this.Value == temp.Value)
			{
				return true;
			}

			return false;
		}

		public decimal DecimalValue()
		{
			return Convert.ToDecimal((double) this.Value);
		}

		public BigInteger IntegerValue()
		{
			return this.Value;
		}

		public ScheminBool BoolValue()
		{
			return ScheminBool.True;
		}
	}
}
