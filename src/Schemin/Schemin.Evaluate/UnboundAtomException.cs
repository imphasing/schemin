
namespace Schemin.Evaluate
{
	using System;

	public class UnboundAtomException : Exception
	{
		public UnboundAtomException() : base() { }
		public UnboundAtomException(string message) : base(message) { }
		public UnboundAtomException(string message, System.Exception inner) : base(message, inner) { }
	}
}
