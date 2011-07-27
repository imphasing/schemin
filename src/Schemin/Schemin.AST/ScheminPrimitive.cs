
namespace Schemin.AST
{
	using System;
	using Schemin.Evaluate;
	using Environment = Schemin.Evaluate.Environment;

	public class ScheminPrimitive : IScheminType
	{
		public Func<ScheminList, Environment, IScheminType> Definition;
		public string Name;

		public ScheminPrimitive(string name)
		{
			this.Name = name;
		}

		public ScheminPrimitive(Func<ScheminList, Environment, IScheminType> definition, string name)
		{
			this.Definition = definition;
			this.Name = name;
		}

		public IScheminType Evaluate(ScheminList args, Environment env)
		{
			return Definition(args, env);
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
