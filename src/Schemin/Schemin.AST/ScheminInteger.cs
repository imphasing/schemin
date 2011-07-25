
namespace Schemin.AST
{
	public class ScheminInteger : IScheminType
	{
		public int Value;

		public ScheminInteger(int value)
		{
			this.Value = value;
		}

		public override string ToString()
		{
			return Value.ToString();
		}
	}
}
