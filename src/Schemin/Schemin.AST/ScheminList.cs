
namespace Schemin.AST
{
	using System;
	using System.Text;
	using Cadenza.Collections;

	public class ScheminList : IScheminType
	{
		public CachedSequence<IScheminType> List;

		public ScheminList()
		{
		}

		public ScheminList(IScheminType head)
		{
			this.List = new CachedSequence<IScheminType>(head);
		}

		public ScheminList(IScheminType head, ScheminList rest)
		{
			this.List = new CachedSequence<IScheminType>(head, rest.List);
		}

		public ScheminList(CachedSequence<IScheminType> list)
		{
			this.List = list;
		}

		public IScheminType Car()
		{
			if (this.List == null)
			{
				return null;
			}

			return this.List.Head;
		}

		public ScheminList Cdr()
		{
			if (this.List == null)
			{
				return null;
			}

			return new ScheminList(this.List.Tail);
		}

		public ScheminList Append(IScheminType type)
		{
			if (this.List == null)
			{
				this.List = new CachedSequence<IScheminType>(type);
				return new ScheminList(this.List);
			}
			else
			{
				return new ScheminList(this.List.Append(type));
			}
		}

		public override string ToString()
		{
			return ToStringInternal(this);
		}

		private string ToStringInternal(ScheminList list)
		{
			StringBuilder builder = new StringBuilder();

			foreach (var type in list.List)
			{
				if (type == null)
				{
					return String.Empty;
				}

				if (type.GetType() == typeof(ScheminList))
				{
					builder.Append("(");
					builder.Append(ToStringInternal((ScheminList) type));
					builder.Append(")");
				}
				else
				{
					string value = String.Empty;
					if (type.GetType() == typeof(ScheminAtom))
					{
						ScheminAtom atom = (ScheminAtom) type;	
						value = atom.Name;
					}
					else if (type.GetType() == typeof(ScheminString))
					{
						ScheminString str = (ScheminString) type;
						value = str.Value;
					}
					else if (type.GetType() == typeof(ScheminInteger))
					{
						ScheminInteger num = (ScheminInteger) type;
						value = num.Value.ToString();
					}

					builder.Append(value + " ");
				}
			}

			return builder.ToString();
		}
	}
}
