
namespace Schemin.AST
{
	using System.Numerics;

	public class ScheminDecimal : IScheminType, IScheminNumeric
	{
		public decimal Value;

		public ScheminDecimal(decimal value)
		{
			this.Value = value;
		}

		public override string ToString()
		{
			return Value.ToString();
		}

		public bool Equals(IScheminType type)
		{
			if (this.GetType() != type.GetType())
			{
				return false;
			}

			ScheminDecimal temp = (ScheminDecimal) type;
			if (this.Value == temp.Value)
			{
				return true;
			}

			return false;
		}

		public decimal DecimalValue()
		{
			return this.Value;
		}

		public BigInteger IntegerValue()
		{
			return new BigInteger(this.Value);
		}

		public ScheminBool BoolValue()
		{
			return ScheminBool.True;
		}
	}
}
