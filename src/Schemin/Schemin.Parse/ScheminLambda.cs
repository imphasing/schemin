
namespace Schemin.Parse
{
	public class ScheminLambda : ISchemeType
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
