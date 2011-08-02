
namespace Schemin.AST
{
	using System;
	using Schemin.Evaluate;
	using Environment = Schemin.Evaluate.Environment;

	public class ScheminLambda : IScheminType
	{
		public IScheminType Definition;
		public ScheminList Arguments;
		public Environment Closure;

		public ScheminLambda(ScheminList definition)
		{
			this.Arguments = (ScheminList) definition.Car();
			this.Definition = definition.Cdr();
		}

		public IScheminType Evaluate(ScheminList values, Evaluator eval, Environment env)
		{
			IScheminType first = Arguments.Car();
			ScheminList rest = Arguments.Cdr();
			IScheminType firstArg = values.Car();
			ScheminList restArgs = values.Cdr();

			for (; ;)
			{
				if (first.GetType() == typeof(ScheminList))
				{
					ScheminList tempFirst = (ScheminList) first;
					if (tempFirst.Empty)
					{
						break;
					}
				}

				env.AddBinding((ScheminAtom) first, firstArg);

				first = rest.Car();
				firstArg = restArgs.Car();
				rest = rest.Cdr();
				restArgs = restArgs.Cdr();
			}

			if (this.Closure != null)
			{
				// Only close over the top level with locals, otherwise we clobber our env with global stuff
				env.CloseOverTop(this.Closure);
			}
			
			// Need to evaluate each part of the lambda body and return the last value
			IScheminType last = null;
			if (Definition.GetType() == typeof(ScheminList))
			{
				ScheminList temp = (ScheminList) Definition;
				foreach (IScheminType type in temp.List)
				{
					last = eval.EvaluateInternal(type, env);
				}
			}
			else
			{
				last = eval.EvaluateInternal(Definition, env);
			}

			// Pass closure on to the next lambda if we return one
			if (last.GetType() == typeof(ScheminLambda))
			{
				ScheminLambda temp = (ScheminLambda) last;
				temp.Closure = env;
			}

			return last;
		}

		public override string ToString()
		{
			return "<Lambda>";
		}

		public bool Equals(IScheminType type)
		{
			return false;
		}
	}
}
