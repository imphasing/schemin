
namespace Schemin.AST
{
	using System;
	using Schemin.Evaluate;
	using Schemin.Evaluate.Primitives;
    using System.Collections.Generic;
	using Environment = Schemin.Evaluate.Environment;

	public class ScheminContinuation : IScheminType
	{
        public Stack<Evaluator.StackFrame> PreviousStack;

		public ScheminContinuation(Stack<Evaluator.StackFrame> callStack)
		{
            PreviousStack = new Stack<Evaluator.StackFrame>(callStack);
		}

		public override string ToString()
		{
			return "<Continuation>";
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
