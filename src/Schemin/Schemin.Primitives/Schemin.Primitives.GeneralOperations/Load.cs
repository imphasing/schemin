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

namespace Schemin.Primitives.GeneralOperations
{
	using System;
	using System.Text;
	using System.IO;
	using Schemin.Evaluate;
	using Schemin.Tokenize;
	using Schemin.Parse;
	using Schemin.AST;
	using Environment = Schemin.Evaluate.Environment;

	public class Load : Primitive
	{
		public Load()
		{
			base.Rewriter = true;
		}

		public override IScheminType Execute(Environment env, Evaluator eval, ScheminPair args)
		{
			ScheminString filename = (ScheminString) args.Car;
			string baseDir = AppDomain.CurrentDomain.BaseDirectory;

			FileStream fs = File.OpenRead(baseDir + Path.DirectorySeparatorChar + filename.Value);
			StreamReader sr = new StreamReader(fs);

			string file = sr.ReadToEnd();

			var tokens = EvaluatorFactory.tokenizer.Tokenize(file);
			ScheminPair ast = EvaluatorFactory.parser.Parse(tokens, true).Cons(new ScheminPrimitive("begin"));

			return EvaluatorFactory.macroExpander.ExpandAll(ast);
		}

		public override void CheckArguments(ScheminPair args)
		{
			IScheminType first = args.Car;

			if (args.Length != 1)
			{
				throw new BadArgumentsException("expected 1 argument");
			}

			if ((first as ScheminString) == null)
			{
				throw new BadArgumentsException("first argument must be a string");
			}

			return;
		}
	}
}
