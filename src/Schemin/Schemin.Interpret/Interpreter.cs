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

namespace Schemin.Interpret
{
	using System;
	using System.Collections.Generic;
	using System.IO;
	using Schemin.AST;
	using Schemin.Evaluate;
	using Schemin.Parse;
	using Schemin.Primitives;
	using Schemin.Tokenize;
	using Environment = Schemin.Evaluate.Environment;

	public class Interpreter
	{
		public Environment GlobalEnv;

		public Evaluator evaluator;
		public Evaluator macroEvaluator;

		public Tokenizer tokenizer;
		public PairParser parser;

		public MacroExpander macroExpander;

		public Interpreter()
		{
			this.GlobalEnv = new Environment();

			this.tokenizer = new Tokenizer();
			this.parser = new PairParser();
			this.evaluator = new Evaluator(this);
			this.macroEvaluator = new Evaluator(this);
			this.macroExpander = new MacroExpander(macroEvaluator);

			DefinePrimitives();
		}

		public IScheminType Interpret(List<Token> tokens)
		{
			try
			{
				var parsed = this.parser.Parse(tokens, false);

				var expanded = (ScheminPair) this.macroExpander.ExpandAll(parsed);
				IScheminType returnType = this.evaluator.EvaluateInternal(expanded);

				return returnType;
			}
			catch (BadArgumentsException b)
			{
				this.evaluator.CurrentOutputPort.OutputStream.WriteLine("bad arguments: " + b.Message);
				return new ScheminPair();
			}
			catch (Exception e)
			{
				this.evaluator.CurrentOutputPort.OutputStream.WriteLine("error: " + e.Message);
				return new ScheminPair();
			}
		}

		public IScheminType Interpret(string scheminCode)
		{
			var tokens = this.tokenizer.Tokenize(scheminCode);
			return Interpret(tokens);
		}

		private void DefinePrimitives()
		{
			foreach (KeyValuePair<string, Primitive> kvp in PrimitiveFactory.Primitives)
			{
				ScheminAtom symbol = AtomFactory.GetAtom(kvp.Key);
				ScheminPrimitive prim = new ScheminPrimitive(kvp.Key);

				this.GlobalEnv.AddBinding(symbol, prim);
			}

			string filename = "ScheminLib\\\\ScheminLib.ss";
			string baseDir = AppDomain.CurrentDomain.BaseDirectory;
			FileStream fs = File.OpenRead(baseDir + Path.DirectorySeparatorChar + filename);
			StreamReader sr = new StreamReader(fs);
			string file = sr.ReadToEnd();

			var tokens = this.tokenizer.Tokenize(file);
			ScheminPair ast = this.parser.Parse(tokens, true).Cons(new ScheminPrimitive("begin"));

			this.evaluator.EvaluateInternal(ast);
		}
	}
}
