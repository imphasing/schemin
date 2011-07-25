
namespace Schemin.AST
{
	using System;

	public class ScheminLambda : IScheminType
	{
		public ScheminList Definition;

		public ScheminLambda(ScheminList definition)
		{
			this.Definition = definition;
		}

		public IScheminType Evaluate(ScheminList args)
		{
			throw new NotImplementedException();	
		}
	}
}
