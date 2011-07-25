
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
	}
}
