
namespace Schemin.AST
{
	using System;
	using Schemin.Evaluate;
	using Environment = Schemin.Evaluate.Environment;

	public class ScheminLambda : IScheminType
	{
		public ScheminList Definition;

		public ScheminLambda(ScheminList definition)
		{
			this.Definition = definition;
		}

		public IScheminType Evaluate(ScheminList args, Evaluator eval, Environment env)
		{
			ScheminList symbols = (ScheminList) args.Car();
			ScheminList definition = (ScheminList) args.Cdr();

			ScheminAtom first;
			ScheminList rest;

			ScheminAtom firstArg;
			ScheminList restArgs;

			for (; ;)
			{
				first = (ScheminAtom) symbols.Car();
				firstArg = (ScheminAtom) args.Car();
				rest = symbols.Cdr();
				restArgs = args.Cdr();

					

				first = rest.Car();
				firstArg = restArgs.Car();
				rest = rest.Cdr();
				restArgs = restArgs.Cdr();
			}
			throw new NotImplementedException();	
		}
	}
}
