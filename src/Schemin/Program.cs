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

namespace Schemin
{
	using System;
	using System.Collections.Generic;
	using System.Linq;
	using System.Text;
	using Schemin.Parse;
	using Schemin.AST;
	using Schemin.Evaluate;
	using Schemin.Tokenize;
	using Mono.Options;
	using System.IO;
	using Environment = Schemin.Evaluate.Environment;

	class Program
	{
		static void Main(string[] consoleArgs) 
		{
			bool help = false;
			string fileName = String.Empty;
			bool file = false;
			bool repl = false;

			var p = new OptionSet () {
				{ 
					"file=", 
					"Interpret a given file with Schemin.", 
					v => { fileName = v; file = true; } 
				},
				{ 
					"repl",
					"Start up a read, eval, print loop (REPL) session.", 
					v => {file = false; repl = v != null; } 
				},
				{ 
					"h|?|help", 
					"Display help.", 
					v => {file = false; repl = false; help = v != null; } 
				}
			};

			try 
			{
				List<string> extra = p.Parse(consoleArgs);
			}
			catch (OptionException e)
			{
				Console.WriteLine("Invalid options. Displaying help:");
				p.WriteOptionDescriptions(Console.Out);
			}

			if (repl)
			{
				ReplPrompt();
			}
			else if (file)
			{
				InterpretFile(fileName);
			}
			else if (help)
			{	
				p.WriteOptionDescriptions(Console.Out);
			}
			else
			{
				ReplPrompt();
			}
		}

		static void InterpretFile(string filename)
		{
			Tokenizer t = new Tokenizer();
			Parser p = new Parser();
			Evaluator eval = new Evaluator();
			Environment global = new Environment();
			eval.DefinePrimitives(global);

			string contents = File.ReadAllText(filename);
			var tokens = t.Tokenize(contents);
			var parsed = p.Parse(tokens);
			IScheminType returnType = eval.Evaluate(parsed, global);
			Console.WriteLine(returnType.ToString());
		}

		static void ReplPrompt()
		{
			Tokenizer t = new Tokenizer();
			Parser p = new Parser();
			Evaluator eval = new Evaluator();
			Environment global = new Environment();
			eval.DefinePrimitives(global);

			for (; ;)
			{
				bool completeInput = false;
				int openParens = 0;
				int closeParens = 0;

				List<Token> partialInput = new List<Token>();

				while (completeInput != true)
				{
					if (openParens != closeParens)
					{
						Console.Write("schemin* ");
					}
					else
					{
						Console.Write("schemin> ");
					}

					string line = Console.ReadLine();

					// bust out right here if the user wants to exit
					if (line == ",exit")
					{
						return;
					}

					var lineTokens = t.Tokenize(line);

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

				try
				{
					if (partialInput.Count < 1)
					{
						continue;
					}

					var parsed = p.Parse(partialInput);
					IScheminType returnType = eval.Evaluate(parsed, global);
					Console.WriteLine(returnType.ToString());
				}
				catch (UnboundAtomException unbound)
				{
					Console.WriteLine("Error: " + unbound.Message.ToString());
				}
				catch (InvalidOperationException invalid)
				{
					Console.WriteLine("Error: " + invalid.Message.ToString());
				}
			}
		}
	}
}
