
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
		public bool Empty = true;

        private bool quoted = true;

        public static bool QuoteLists = true;

		public int Length
		{
			get
			{
				int count = 0;
				foreach (IScheminType type in this)
				{
					if (type != null)
					{
						if (type.GetType() == typeof(ScheminList))
						{
							ScheminList temp = (ScheminList) type;
							if (!temp.Empty)
								count++;
						}
						else
						{
							count++;
						}
					}
				}

				return count;
			}
		}

		public ScheminList()
		{
            if (!QuoteLists)
            {
                this.quoted = false;
            }

			this.Empty = true;
		}

		public ScheminList(IScheminType head)
		{
            if (!QuoteLists)
            {
                this.quoted = false;
            }

			this.Head = head;
			this.Empty = false;
		}

		public ScheminList(IScheminType head, ScheminList rest)
		{
            if (!QuoteLists)
            {
                this.quoted = false;
            }

			this.Head = head;
			this.Rest = rest;
			this.Empty = false;
		}

		public IScheminType Car()
		{
			if (this.Head == null)
			{
                ScheminList ret = new ScheminList();
                ret.quoted = quoted;

                return ret;
			}

			return this.Head;
		}

		public ScheminList Cdr()
		{
            ScheminList ret;
			if (this.Rest == null)
			{
                ret = new ScheminList();
                ret.quoted = quoted;

                return ret;
			}

            ret = this.Rest;
            ret.quoted = quoted;

            return ret;
		}

		public ScheminList Cons(IScheminType type)
		{
            ScheminList ret = new ScheminList(type, this);
            ret.quoted = quoted;

            return ret;
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
			var c = Cdr();
			yield return Car();
			c = Cdr();
			while (!c.Empty) {
				yield return c.Car();
				c = c.Cdr();
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

        public bool Quoted()
        {
            return this.quoted;
        }

        public void Quote()
        {
            quoted = true;
        }

        public void UnQuote()
        {
            quoted = false;
        }

		public bool Equals(IScheminType type)
		{
			// list comparison isn't implemented
			return false;
		}

		public ScheminBool BoolValue()
		{
			return ScheminBool.True;
		}
	}
}
