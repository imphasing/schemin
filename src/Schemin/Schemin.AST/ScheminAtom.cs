
namespace Schemin.AST
{
	public class ScheminAtom : IScheminType
	{
		public string Name;

		public ScheminAtom(string name)
		{
			this.Name = name;
		}
		
		public override string ToString()
		{
			return Name;
		}

		public bool Equals(IScheminType type)
		{
			if (this.GetType() != type.GetType())
			{
				return false;
			}

			ScheminAtom temp = (ScheminAtom) type;
			if (this.Name == temp.Name)
			{
				return true;
			}

			return false;
		}

		public ScheminBool BoolValue()
		{
			return new ScheminBool(true);
		}
	}
}
