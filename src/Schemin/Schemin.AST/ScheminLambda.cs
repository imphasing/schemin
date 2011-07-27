
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

			ScheminAtom first = (ScheminAtom) symbols.Car();
			ScheminList rest = symbols.Cdr();

			IScheminType firstArg = (IScheminType) args.Car();
			ScheminList restArgs = args.Cdr();

			for (; ;)
			{
				first = (ScheminAtom) symbols.Car();
				firstArg = (IScheminType) args.Car();
				rest = symbols.Cdr();
				restArgs = args.Cdr();

				if (first == null || firstArg == null)
				{
					break;
				}

				env.AddBinding(first, firstArg);
			}

			IScheminType result = eval.Evaluate(Definition, env, false);

			return result;
		}
	}
}
