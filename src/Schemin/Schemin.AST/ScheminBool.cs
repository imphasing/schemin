
namespace Schemin.AST
{
	public class ScheminBool : IScheminType
	{
		public bool Value;

		public ScheminBool(string value)
		{
			if (value == "#f")
			{
				this.Value = false;
			}
			else
			{
				this.Value = true;
			}
		}

		public ScheminBool(bool value)
		{
			this.Value = value;
		}

		public override string ToString()
		{
			if (Value)
			{
				return "#t";
			}
			return "#f";
		}
	}
}
