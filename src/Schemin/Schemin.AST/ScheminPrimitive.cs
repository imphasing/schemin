
namespace Schemin.AST
{
	using System;

	public class ScheminPrimitive : IScheminType
	{
		public Func<IScheminType, ScheminList> Definition;
		public string Name;

		public ScheminPrimitive(string name)
		{
			this.Name = name;
		}

		public ScheminPrimitive(Func<IScheminType, ScheminList> definition, string name)
		{
			this.Definition = definition;
			this.Name = name;
		}

		public IScheminType Evaluate(ScheminList args)
		{
			return Definition(args);
		}
	}
}
