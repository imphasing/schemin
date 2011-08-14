
namespace Schemin.AST
{
	public class ScheminAtom : IScheminType
	{
		public string Name;
        public bool quoted = false;

		public ScheminAtom(string name)
		{
			this.Name = name;
		}
		
		public override string ToString()
		{
			return Name;
		}

        public bool Quoted()
        {
            return this.quoted;
        }

        public void Quote()
        {
            quoted = true;
        }

        public void UnQuote()
        {
            quoted = false;
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
			return ScheminBool.True;
		}
	}
}
