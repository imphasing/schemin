
namespace Schemin.AST
{
	using System;
	using System.Text;
	using Cadenza.Collections;

	public class ScheminListMine : IScheminType
	{
		public IScheminType Head;
		public ScheminListMine Rest;
		public bool Empty;

		public ScheminListMine()
		{
			this.Empty = true;
		}

		public ScheminListMine(IScheminType head)
		{
			this.Head = head;
			this.Empty = false;
		}

		public ScheminListMine(IScheminType head, ScheminListMine rest)
		{
			this.Head = head;
			this.Rest = rest;
			this.Empty = false;
		}

		public IScheminType Car()
		{
			if (this.Head == null)
			{
				return null;
			}

			return this.Head;
		}

		public ScheminListMine Cdr()
		{
			if (this.Rest == null)
			{
				return new ScheminListMine();
			}

			return this.Rest;
		}

		public ScheminListMine Cons(IScheminType type)
		{
			return new ScheminListMine(type, this);
		}

		public ScheminListMine Append(IScheminType type)
		{
			ScheminListMine start, c;
			start = c = new ScheminListMine(this.Car());

			ScheminListMine end = this.Cdr();

			while (!end.Empty)
			{
				var n = new ScheminListMine(end.Car());
				c.Rest = n;
				c = n;
				end = end.Cdr();
			}

			c.Rest = new ScheminListMine(type);

			return start;
		}

		public override string ToString()
		{
			return ToStringInternal(this);
		}

		private string ToStringInternal(ScheminListMine list)
		{
			StringBuilder builder = new StringBuilder();

			if (list.Empty)
			{
				return "()";
			}
			else
			{
				IScheminType start = this.Car();
				ScheminListMine end = this.Cdr();

				while (start != null)
				{
					if (start.GetType() == typeof(ScheminListMine))
					{
						builder.Append("(");
						builder.Append(start.ToString());
						builder.Append(")");
					}
					else
					{
						builder.Append(start.ToString() + " ");
					}

					start = end.Car();
					end = end.Cdr();
				}
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
