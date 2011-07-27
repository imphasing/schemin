
namespace Schemin.AST
{
	using System;
	using Schemin.Evaluate;
	using Environment = Schemin.Evaluate.Environment;

	public class ScheminLambda : IScheminType
	{
		public ScheminList Definition;
		public ScheminList Arguments;

		public ScheminLambda(ScheminList definition)
		{
			this.Arguments = (ScheminList) definition.Car();
			this.Definition = (ScheminList) definition.Cdr().Car();
		}

		public IScheminType Evaluate(ScheminList values, Evaluator eval, Environment env)
		{
			IScheminType first = Arguments.Car();
			ScheminList rest = Arguments.Cdr();

			IScheminType firstArg = values.Car();
			ScheminList restArgs = values.Cdr();

			for (; ;)
			{
				if (first.GetType() == typeof(ScheminList) || firstArg.GetType() == typeof(ScheminList))
				{
					break;
				}

				env.AddBinding((ScheminAtom) first, firstArg);

				first = rest.Car();
				firstArg = restArgs.Car();
				rest = rest.Cdr();
				restArgs = restArgs.Cdr();
			}

			IScheminType result = eval.Evaluate(Definition, env, false);

			return result;
		}

		public override string ToString()
		{
			return "<Lambda>";
		}
	}
}
