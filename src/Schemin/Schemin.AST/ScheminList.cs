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
							if (!temp.Empty || temp.Quoted())
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
			IScheminType oldHead = this.Head;

			this.Head = type;
			this.Rest = new ScheminList(oldHead, this.Rest);

			return this;
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
