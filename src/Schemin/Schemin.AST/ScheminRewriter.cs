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
	using Schemin.Evaluate;
	using Schemin.Primitives;

	using Environment = Schemin.Evaluate.Environment;

	public class ScheminRewriter : IScheminType
	{
		public ScheminLambda Rewriter;

		public ScheminRewriter(ScheminLambda rewriter)
		{
			this.Rewriter = rewriter;
		}

		public IScheminType Rewrite(ScheminList values)
		{
			Evaluator eval = new Evaluator();
			ScheminList call = new ScheminList(Rewriter);
			ScheminList unquote = new ScheminList(new ScheminPrimitive("unquote"));
			unquote.UnQuote();
			call.UnQuote();

			foreach (IScheminType type in values)
			{
				call.Append(type);
			}

			unquote.Append(call);

			return unquote;
		}

		public override string ToString()
		{
			return "<Rewriter>";
		}

		public bool Quoted()
		{
			return false;
		}

		public void Quote()
		{
		}

		public void UnQuote()
		{
		}

		public bool Equals(IScheminType type)
		{
			if ((type as ScheminRewriter) != null)
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
