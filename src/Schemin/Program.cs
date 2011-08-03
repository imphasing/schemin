
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
	using Environment = Schemin.Evaluate.Environment;

	class Program
	{
		static void Main(string[] consoleArgs) 
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
						Console.Write("schemin>* ");
					}
					else
					{
						Console.Write("schemin> ");
					}

					string line = Console.ReadLine();
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
					var parsed = p.Parse(partialInput);
					IScheminType returnType = eval.Evaluate(parsed, global);
					Console.WriteLine(">> " + returnType.ToString());
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
