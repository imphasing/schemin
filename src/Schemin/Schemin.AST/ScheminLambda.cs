
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
			bool singleListArg = false;
			IScheminType first = Arguments.Car();
			ScheminList rest = Arguments.Cdr();

			if (this.Arguments.List.Count() < 2 && Arguments.List.Count() < values.List.Count())
			{
				singleListArg = true;
			}

			IScheminType firstArg;

			// if there's a single argument and the number of lambda arguments is less than the number of values in the list
			// then we're working with a lambda with a single list argument passed in, to treat as the argument
			if (singleListArg)
			{
				firstArg = values;
			}
			else
			{
				firstArg = values.Car();
			}

			ScheminList restArgs = values.Cdr();
			int argsLeft = Arguments.List.Count();

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

				if (firstArg.GetType() == typeof(ScheminList))
				{
					ScheminList tempArg = (ScheminList) firstArg;
					if (tempArg.Empty)
					{
						break;
					}
				}

				env.AddBinding((ScheminAtom) first, firstArg);
				argsLeft--;

				// break after the first argument
				if (singleListArg)
				{
					break;
				}

				// If there's only one arg left but multiple values left, treat it as a liast argument
				if (argsLeft == 1 && restArgs.List.Count() > 1)
				{
					env.AddBinding((ScheminAtom) rest.Car(), restArgs);
					break;
				}

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
					last = eval.Evaluate(type, env, false, false);
				}
			}
			else
			{
				last = eval.Evaluate(Definition, env, false, false);
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
