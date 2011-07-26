
namespace Schemin.AST
{
	using System;

	public class ScheminPrimitive : IScheminType
	{
		public Func<ScheminList, IScheminType> Definition;
		public string Name;

		public ScheminPrimitive(string name)
		{
			this.Name = name;
		}

		public ScheminPrimitive(Func<ScheminList, IScheminType> definition, string name)
		{
			this.Definition = definition;
			this.Name = name;
		}

		public IScheminType Evaluate(ScheminList args)
		{
			return Definition(args);
		}

		public override string ToString()
		{
			if (Name != null)
			{
				return "<Primitive:" + Name + ">";
			}
			else
			{
				return "<Primitive:unbound>";
			}
		}
	}
}
