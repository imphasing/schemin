
namespace Schemin.AST
{
	using Schemin.Evaluate;

	public class ScheminContinuation : IScheminType
	{
		public ScheminList Before;
		public ScheminList After;
		public bool Empty = false;

		public ScheminContinuation(ScheminList before, ScheminList after)
		{
			this.Before = before;
			this.After = after;
		}

		public ScheminContinuation()
		{
			this.Empty = true;
		}

		public ScheminList InvokeWith(IScheminType result)
		{
			ScheminList complete = new ScheminList();

			if (!Empty)
			{
				if (Before.Length > 0)
				{
					foreach (IScheminType type in Before)
					{
						complete.Append(type);
					}
				}

				if (result != null)
				{
					complete.Append(result);
				}

				if (After.Length > 0)
				{
					foreach (IScheminType type in After)
					{
						complete.Append(type);
					}
				}
			}

			return complete;
		}

		public override string ToString()
		{
			return "<Continuation>";
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
