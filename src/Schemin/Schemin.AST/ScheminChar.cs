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
	using System.Collections.Generic;

	public class ScheminChar : IScheminType
	{
		public char Value;
		public bool EOF = false;

		public ScheminChar(string value)
		{
			if (value.Length == 1)
			{
				this.Value = Convert.ToChar(value); 
			}
			else
			{
				this.Value = MapNamedLiteral(value);
			}
		}

		public ScheminChar(int value)
		{
			if (value == -1)
			{
				this.EOF = true;
			}
			else
			{
				this.Value = (char) value;
			}
		}

		private char MapNamedLiteral(string named)
		{
			Dictionary<string, char> mappings = new Dictionary<string, char>();
			mappings.Add("newline", '\n');
			mappings.Add("space", ' ');

			return mappings[named];
		}

		private string MapNamedValue(char value)
		{
			Dictionary<char, string> mappings = new Dictionary<char, string>();
			mappings.Add('\n', "newline");
			mappings.Add(' ', "space");

			if (mappings.ContainsKey(value))
			{
				return mappings[value];
			}
			
			return value.ToString();
		}

		public override string ToString()
		{
			if (this.EOF)
			{
				return "EOF";
			}

			return "#\\" + MapNamedValue(Value);
		}

		public bool Equals(IScheminType type)
		{
			if (this.GetType() != type.GetType())
			{
				return false;
			}

			ScheminChar temp = (ScheminChar) type;
			if (this.Value == temp.Value)
			{
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
