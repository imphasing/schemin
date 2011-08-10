
namespace Schemin.AST
{
	using System;
	using Schemin.Evaluate;
	using Environment = Schemin.Evaluate.Environment;

	public class ScheminPrimitive : IScheminType
	{
		public Func<ScheminList, Environment, Evaluator, IScheminType> Definition;
		public string Name;

		public ScheminPrimitive(string name)
		{
			this.Name = name;
		}

		public ScheminPrimitive(Func<ScheminList, Environment, Evaluator, IScheminType> definition, string name)
		{
			this.Definition = definition;
			this.Name = name;
		}

		public IScheminType Evaluate(ScheminList args, Environment env, Evaluator eval)
		{
			return Definition(args, env, eval);
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

		public bool Equals(IScheminType type)
		{
			return false;
		}

		public ScheminBool BoolValue()
		{
			return ScheminBool.True;
		}
	}
}
