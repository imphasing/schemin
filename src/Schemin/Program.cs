
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
