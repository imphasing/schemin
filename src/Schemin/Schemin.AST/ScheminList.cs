
namespace Schemin.AST
{
	using System;
	using System.Text;
	using Cadenza.Collections;

	public class ScheminList : IScheminType
	{
		public CachedSequence<IScheminType> List;
		public bool Empty;

		public ScheminList()
		{
			this.Empty = true;
			this.List = null;
		}

		public ScheminList(IScheminType head)
		{
			this.List = new CachedSequence<IScheminType>(head);
			this.Empty = false;
		}

		public ScheminList(IScheminType head, ScheminList rest)
		{
			this.List = new CachedSequence<IScheminType>(head, rest.List);
			this.Empty = false;
		}

		public ScheminList(CachedSequence<IScheminType> list)
		{
			this.List = list;
			this.Empty = false;
		}

		public IScheminType Car()
		{
			if (this.List == null)
			{
				return new ScheminList();
			}

			return this.List.Head;
		}

		public ScheminList Cdr()
		{
			if (this.List == null)
			{
				return new ScheminList();
			}

			return new ScheminList(this.List.Tail);
		}

		public ScheminList Append(IScheminType type)
		{
			if (this.List == null)
			{
				this.List = new CachedSequence<IScheminType>(type);
				this.Empty = false;
			}
			else
			{
				this.List = this.List.Append(type);
			}

			return this;
		}

		public override string ToString()
		{
			return "List";
		}
	}
}
