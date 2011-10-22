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
	using Schemin.Tokenize;
	using Schemin.Primitives;
	using Environment = Schemin.Evaluate.Environment;

	public class ScheminPrimitive : IScheminType
	{
		public Primitive Definition;
		public Token SourceToken;
		public string Name;
		public bool Rewriter = false;

		public ScheminPrimitive(string name)
		{
			this.Definition = PrimitiveFactory.Get(name);
			if (this.Definition.Rewriter)
				this.Rewriter = true;
			this.Name = name;
		}

		public ScheminPrimitive(string name, Token token)
		{
			this.SourceToken = token;
			this.Definition = PrimitiveFactory.Get(name);
			if (this.Definition.Rewriter)
				this.Rewriter = true;
			this.Name = name;
		}

		public ScheminPrimitive(Primitive definition, string name)
		{
			this.Definition = definition;
			if (this.Definition.Rewriter)
				this.Rewriter = true;
			this.Name = name;
		}

		public IScheminType Evaluate(ScheminPair args, Environment env, Evaluator eval)
		{
			Definition.CheckArguments(args);
			return Definition.Execute(env, eval, args);
		}

		public override string ToString()
		{
			if (Name != null)
			{
				return "<Primitive:" + Name + ">";
			}
			else
			{
				return "<Primitive:unbound>";
			}
		}

		public bool Equals(IScheminType type)
		{
			if ((type as ScheminPrimitive) != null)
			{
				ScheminPrimitive prim = (ScheminPrimitive) type;
				if (prim.Definition == this.Definition)
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
