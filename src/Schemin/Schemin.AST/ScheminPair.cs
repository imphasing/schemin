/* 
 * Copyright (c) 2011 Alex Fort 
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
namespace Schemin.AST
{
	using System;
	using System.Text;
	using System.Collections;
	using System.Collections.Generic;
	public class ScheminPair : IScheminType, IEnumerable<IScheminType>
	{
		public IScheminType Head = null;
		public IScheminType Rest = null;
		private bool quoted = true;
		public static bool QuoteLists = true;
		public int Length
		{
			get
			{
				int count = 0;
				foreach (IScheminType type in this)
				{
					count++;
				}

				return count;
			}
		}

		public bool Empty
		{
			get
			{
				if (this.Head == null && this.Rest == null)
				{
					return true;
				}

				return false;
			}
		}

		public ScheminPair()
		{
			if (!QuoteLists)
			{
				this.quoted = false;
			}
		}

		public ScheminPair(bool quoted)
		{
			this.quoted = quoted;
		}

		public ScheminPair(IScheminType head)
		{
			if (!QuoteLists)
			{
				this.quoted = false;
			}

			this.Head = head;
		}

		public ScheminPair(IScheminType head, IScheminType rest)
		{
			if (!QuoteLists)
			{
				this.quoted = false;
			}

			this.Head = head;
			this.Rest = rest;
		}

		public IScheminType Car()
		{
			return this.Head;
		}

		public IScheminType Cdr()
		{
			return this.Rest;
		}

		public IScheminType ElementAt(int position)
		{
			int current = 0;
			foreach (IScheminType type in this)
			{
				if (position == current)
					return type;

				current++;
			}

			throw new Exception("No element in list: " + this.ToString() + " at position: " + position.ToString());
		}

		public ScheminPair Cons(IScheminType type)
		{
			IScheminType oldHead = this.Head;
			if (oldHead != null)
			{
				this.Head = type;
				this.Rest = new ScheminPair(oldHead, this.Rest);
			}
			else
			{
				this.Head = type;
			}

			return this;
		}

		public ScheminPair Append(IScheminType type)
		{
			if (this.Head == null)
			{
				this.Cons(type);
				return this;
			}

			if (this.Rest == null)
			{
				this.Rest = new ScheminPair(type);
				return this;
			}

			IScheminType rest = this.Rest;
			if ((rest as ScheminPair) != null)
			{
				ScheminPair temp = (ScheminPair) rest;
				while (temp.Rest != null)
				{
					if ((temp.Rest as ScheminPair) != null)
					{
						temp = (ScheminPair)temp.Rest;
					}
					else
					{
						throw new Exception("Can't append to the non-list: " + rest);
					}
				}

				if (temp.Head == null)
				{
					temp.Head = type;
					return this;
				}
				else
				{
					ScheminPair newRest;
					if (temp.Rest != null)
					{
						newRest = new ScheminPair(type);
						newRest.Rest = temp.Rest;
					}
					else
					{
						newRest = new ScheminPair(type);
					}

					newRest.quoted = quoted;
					temp.Rest = newRest;

					return this;
				}
			}
			else
			{
				throw new Exception("Can't append to the non-list: " + rest);
			}
		}

		IEnumerator IEnumerable.GetEnumerator()
		{
			return GetEnumerator();
		}

		public IEnumerator<IScheminType> GetEnumerator ()
		{
			if (Car() != null)
				yield return Car();

			if ((Cdr() as ScheminPair) != null)
			{
				IScheminType pair = Cdr();

				while ((pair as ScheminPair) != null)
				{
					yield return ((ScheminPair)pair).Car();
					pair = ((ScheminPair) pair).Cdr();
				}

				if (pair != null)
					yield return pair;
			}
			else
			{
				if (Cdr() != null)
					yield return Cdr();
			}
		}

		public override string ToString()
		{
			return "(" + ToStringInternal(this) + ")";
		}

		public string ToStringInternal(ScheminPair list)
		{
			StringBuilder builder = new StringBuilder();
			if (list.Empty)
			{
				return "()";
			}
			else
			{
				bool dotted = false;
				if ((list.Rest as ScheminPair) == null && list.Length > 1)
					dotted = true;

				if (dotted)
				{
					builder.Append(list.Head);
					if (list.Rest != null)
						builder.Append(" . " + list.Rest);
				}
				else
				{
					builder.Append(list.Head);
					if (list.Rest != null)
						builder.Append(" ");
					if ((list.Rest as ScheminPair) != null)
						builder.Append(((ScheminPair) list.Rest).ToStringInternal((ScheminPair) list.Rest));
				}
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
			if ((type as ScheminPair) != null)
			{
				if (type == this)
				{
					return true;
				}
			}
			return false;
		}

		public ScheminBool BoolValue()
		{
			return ScheminBool.True;
		}
	}
}
