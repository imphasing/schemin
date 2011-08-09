
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

		public ScheminLambda(ScheminList definition, Environment closure)
		{
			this.Arguments = (ScheminList) definition.Car();
			this.Definition = definition.Cdr();
			this.Closure = closure;
		}

		public IScheminType Evaluate(ScheminList values, Evaluator eval)
		{
			IScheminType first = Arguments.Car();
			ScheminList rest = Arguments.Cdr();
			IScheminType firstArg = values.Car();
			ScheminList restArgs = values.Cdr();

			Environment args = new Environment();
			args.parent = this.Closure;

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

				args.AddBinding((ScheminAtom) first, firstArg);

				first = rest.Car();
				firstArg = restArgs.Car();
				rest = rest.Cdr();
				restArgs = restArgs.Cdr();
			}

			IScheminType last; 
			if (Definition.GetType() == typeof(ScheminList))
			{
				last = eval.Evaluate((ScheminList) Definition, args);
			}
			else
			{
				last = eval.EvaluateInternal(Definition, args); 
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

		public ScheminBool BoolValue()
		{
			return new ScheminBool(true);
		}
	}
}
