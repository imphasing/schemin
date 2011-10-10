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

namespace Schemin.Primitives
{
	using System;
	using System.Collections.Generic;
	using Schemin.Evaluate;
	using Schemin.AST;
	using Environment = Schemin.Evaluate.Environment;

	public static class PrimitiveFactory
	{
		public static Dictionary<string, Primitive> Primitives;

		static PrimitiveFactory()
		{
			Primitives = new Dictionary<string, Primitive>();

			Primitives.Add("dumpenv", new GeneralOperations.DumpEnv());
			Primitives.Add("apply", new GeneralOperations.Apply());
			Primitives.Add("lambda", new GeneralOperations.Lambda());
			Primitives.Add("define", new GeneralOperations.Define());
			Primitives.Add("define-rewriter", new GeneralOperations.DefineRewriter());
			Primitives.Add("gensym", new GeneralOperations.GenSym());
			Primitives.Add("quote", new GeneralOperations.Quote());
			Primitives.Add("quasiquote", new GeneralOperations.QuasiQuote());
			Primitives.Add("unquote", new GeneralOperations.Unquote());
			Primitives.Add("unquote-splicing", new GeneralOperations.UnquoteSplicing());
			Primitives.Add("eval-macro", new GeneralOperations.EvalMacro());
			Primitives.Add("begin", new GeneralOperations.Begin());
			Primitives.Add("if", new GeneralOperations.If());
			Primitives.Add("cond", new GeneralOperations.Cond());
			Primitives.Add("let", new GeneralOperations.Let());
			Primitives.Add("letrec", new GeneralOperations.LetRec());
			Primitives.Add("let*", new GeneralOperations.LetStar());
			Primitives.Add("set!", new GeneralOperations.SetBang());
			Primitives.Add("call/cc", new GeneralOperations.CallCC());
		}

		public static Primitive Get(string name)
		{
			return Primitives[name];
		}
	}
}
