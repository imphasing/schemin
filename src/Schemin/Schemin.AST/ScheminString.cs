
namespace Schemin.AST
{
	public class ScheminString : IScheminType
	{
		public string Value;

		public ScheminString(string value)
		{
			this.Value = value;
		}

		public override string ToString()
		{
			return "\"" + Value + "\"";
		}

		public bool Equals(IScheminType type)
		{
			if (this.GetType() != type.GetType())
			{
				return false;
			}

			ScheminString temp = (ScheminString) type;
			if (this.Value == temp.Value)
			{
				return true;
			}

			return false;
		}
	}
}
