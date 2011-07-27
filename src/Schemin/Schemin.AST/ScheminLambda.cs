
namespace Schemin.AST
{
	using System;
	using Schemin.Evaluate;
	using Environment = Schemin.Evaluate.Environment;

	public class ScheminLambda : IScheminType
	{
		public ScheminList Definition;
		public ScheminList Arguments;

		public ScheminLambda(ScheminList definition, ScheminList args)
		{
			this.Definition = definition;
			this.Arguments = args;
		}

		public IScheminType Evaluate(ScheminList values, Evaluator eval, Environment env)
		{
			ScheminAtom first = (ScheminAtom) Arguments.Car();
			ScheminList rest = Arguments.Cdr();

			IScheminType firstArg = (IScheminType) values.Car();
			ScheminList restArgs = values.Cdr();

			for (; ;)
			{
				Console.WriteLine("Iterating. first: " + first.ToString() + " arg: " + firstArg.ToString());

				if (first == null || firstArg == null)
				{
					break;
				}

				env.AddBinding(first, firstArg);

				first = (ScheminAtom) rest.Car();
				firstArg = (IScheminType) restArgs.Car();
				rest = rest.Cdr();
				restArgs = restArgs.Cdr();
			}

			IScheminType result = eval.Evaluate(Definition, env, false);

			return result;
		}
	}
}
