
namespace Schemin.AST
{
	using System;
	using System.Text;
	using System.Collections;
	using System.Collections.Generic;

	public class ScheminList : IScheminType, IEnumerable<IScheminType>
	{
		public IScheminType Head = null;
		public ScheminList Rest = null;
		public bool Empty;

		public ScheminList()
		{
			this.Empty = true;
		}

		public ScheminList(IScheminType head)
		{
			this.Head = head;
			this.Empty = false;
		}

		public ScheminList(IScheminType head, ScheminList rest)
		{
			this.Head = head;
			this.Rest = rest;
			this.Empty = false;
		}

		public IScheminType Car()
		{
			return this.Head;
		}

		public ScheminList Cdr()
		{
			return this.Rest;
		}

		public ScheminList Cons(IScheminType type)
		{
			return new ScheminList(type, this);
		}

		public ScheminList Append(IScheminType type)
		{
			if (this.Head == null)
			{
				this.Head = type;
				this.Empty = false;
				return this;
			}

			if (this.Rest == null)
			{
				this.Rest = new ScheminList(type);
				return this;
			}

			ScheminList rest = this.Rest;
			while (rest.Rest != null)
			{
				rest = rest.Rest;
			}

			if (rest.Head == null)
			{
				rest.Head = type;
				return this;
			}
			else
			{
				rest.Rest = new ScheminList(type);
				return this;
			}
		}

		IEnumerator IEnumerable.GetEnumerator()
		{
			return GetEnumerator();
		}

		public IEnumerator<IScheminType> GetEnumerator ()
		{
			var c = Rest;
			yield return Head;
			c = Rest;
			while (c != null) {
				yield return c.Head;
				c = c.Rest;
			}
		}

		public override string ToString()
		{
			return ToStringInternal(this);
		}

		private string ToStringInternal(ScheminList list)
		{
			StringBuilder builder = new StringBuilder();

			if (list.Empty)
			{
				return "()";
			}
			else
			{
				builder.Append("(");
				foreach (var type in list)
				{
					if (type.GetType() == typeof(ScheminList))
					{
						builder.Append(ToStringInternal((ScheminList) type));
					}
					else
					{
						builder.Append(type.ToString() + " ");
					}
				}
				builder.Append(")");
			}

			return builder.ToString();
		}

		public bool Equals(IScheminType type)
		{
			// list comparison isn't implemented
			return false;
		}
	}
}
