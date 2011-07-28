
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
	}
}
