
namespace Schemin.AST
{
	using System.Numerics;

	public class ScheminInteger : IScheminType
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
	}
}
