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

namespace Schemin.Primitives.PortOperations
{
	using Schemin.Evaluate;
	using Schemin.AST;
	public class Read : Primitive
	{
		public override IScheminType Execute(Environment env, Evaluator eval, ScheminList args)
		{
			IScheminType port = args.Car();
			ScheminPort readFrom = eval.CurrentInputPort;
			if ((port as ScheminPort) != null)
			{
				readFrom = (ScheminPort) port;
			}

			Tokenizer t = new Tokenizer();
			Parser p = new Parser();

			bool completeInput = false;
			bool emptyInput = true;
			int openParens = 0;
			int closeParens = 0;

			List<Token> partialInput = new List<Token>();

			while (completeInput != true)
			{
				StringBuilder built = new StringBuilder();
				char next;
				for (;;)
				{
					int nextRead = readFrom.InputStream.Read();
					next = (char)nextRead;

					if (nextRead == -1 || (next == '\n' && !emptyInput))
					{
						break;
					}

					if (next != '\r')
					{
						built.Append(next);
						emptyInput = false;
					}
				}

				var lineTokens = t.Tokenize(built.ToString());

				foreach (Token token in lineTokens)
				{
					partialInput.Add(token);
					if (token.Type == TokenType.OpenParen)
					{
						openParens++;
					}
					else if (token.Type == TokenType.CloseParen)
					{
						closeParens++;
					}
				}

				if (openParens == closeParens)
				{
					completeInput = true;
					break;
				}
			}

			var parsed = p.Parse(partialInput, true).Car();

			return parsed;
		}
	}
}
