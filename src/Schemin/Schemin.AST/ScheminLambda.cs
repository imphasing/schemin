
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
			this.Definition = definition.Cdr().Car();
			this.Closure = closure;
		}

		public Environment MakeEnvironment(ScheminList values, Environment env, Evaluator eval)
		{
			IScheminType first = Arguments.Car();
			ScheminList rest = Arguments.Cdr();
			IScheminType firstArg = values.Car();
			ScheminList restArgs = values.Cdr();

			Environment args = new Environment();
			args.parent = this.Closure;
            this.Closure.parent = env;

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

            return args;
		}

		public override string ToString()
		{
			return "<Lambda>";
		}

        public bool Quoted()
        {
            return false;
        }

        public void Quote()
        {
        }

        public void UnQuote()
        {
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
