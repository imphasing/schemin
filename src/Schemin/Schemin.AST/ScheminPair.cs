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
		public IScheminType Car = null;
		public IScheminType Cdr = null;
		private bool quoted = true;

		public static bool QuoteLists = true;

		public int Length {
			get {
				if (this.Proper)
				{
					int count = 0;
					foreach (IScheminType type in this)
						count++;

					return count;
				}

				return 2;
			}
		}

		public bool Empty {
			get {
				if (this.Car == null && this.Cdr == null)
					return true;

				return false;
			}
		}

		public bool Proper {
			get {
				if (this.Cdr == null || (this.Cdr as ScheminPair) != null)
					return true;

				return false;
			}
		}

		public ScheminPair()
		{
			if (!QuoteLists)
				this.quoted = false;
		}

		public ScheminPair(bool quoted)
		{
			this.quoted = quoted;
		}

		public ScheminPair(IScheminType car)
		{
			if (!QuoteLists)
				this.quoted = false;

			this.Car = car;
		}

		public ScheminPair(IScheminType car, IScheminType cdr)
		{
			if (!QuoteLists)
				this.quoted = false;

			this.Car = car;
			this.Cdr = cdr;
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

		public ScheminPair ListCdr()
		{
			if (this.Proper)
			{
				ScheminPair ret = (ScheminPair) this.Cdr;

				if (ret != null)
					ret.quoted = this.quoted;
				else
					return new ScheminPair();

				return ret;
			}

			throw new Exception("Can't get the list-cdr of an improper list");
		}

		public ScheminPair Cons(IScheminType type)
		{
			if (this.Proper)
			{
				ScheminPair ret = new ScheminPair(type, this);
				ret.quoted = quoted;
				return ret;
			}
			else
			{
				throw new Exception("Can't cons to an improper list");
			}
		}

		public ScheminPair Append(IScheminType type)
		{
			if (this.Proper)
			{
				if (this.Car == null)
				{
					ScheminPair ret = new ScheminPair(type);
					ret.quoted = quoted;
					return ret;
				}

				if (this.Cdr == null)
				{
					ScheminPair ret = new ScheminPair(this.Car, new ScheminPair(type));
					ret.quoted = quoted;
					return ret;
				}

				ScheminPair appended = new ScheminPair();
				appended.quoted = quoted;

				foreach (IScheminType e in this)
				{
					appended = appended.Append(e);
				}

				ScheminPair rest = appended;
				while (rest.Cdr != null)
				{
					rest = (ScheminPair) rest.Cdr;
				}

				rest.Cdr = new ScheminPair(type);
				return appended;
			}
			else
			{
				throw new Exception("Can't append to an improper list");
			}
		}

		IEnumerator IEnumerable.GetEnumerator()
		{
			return GetEnumerator();
		}

		public IEnumerator<IScheminType> GetEnumerator ()
		{
			if (this.Proper)
			{
				if (!this.Empty)
					yield return Car;

				var c = (ScheminPair) Cdr;
				while (c != null)
				{
					yield return c.Car;
					c = (ScheminPair) c.Cdr;
				}
			}
			else
			{
				throw new Exception("Can't enumerate an improper list");
			}
		}

		public override string ToString()
		{
			if (this.Proper)
			{
				return ToStringList(this);
			}
			else
			{
				return "(" + this.Car + " . " + this.Cdr + ")";
			}
		}

		public string ToStringList(ScheminPair list)
		{
			StringBuilder builder = new StringBuilder();

			if (list.Empty)
			{
				return "()";
			}
			else
			{
				builder.Append("(");
				int index = 0;
				foreach (var type in list)
				{
					if (index < list.Length - 1)
					{
						builder.Append(type.ToString() + " ");
					}
					else
					{
						builder.Append(type.ToString());
					}

					index++;
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
			if ((type as ScheminPair) != null)
			{
				if (type == this)
					return true;
			}
			return false;
		}

		public ScheminBool BoolValue()
		{
			return ScheminBool.True;
		}
	}
}
