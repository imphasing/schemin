
namespace Schemin
{
	using System;
	using System.Collections.Generic;
	using System.Linq;
	using System.Text;
	using Schemin.Tokenize;
	using Schemin.Parse;
	using Schemin.AST;
	using Schemin.Evaluate;
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

			string line = String.Empty;
			for (; ;)
			{
				Console.Write("schemin> ");
				line = Console.ReadLine();

				try
				{
					if (line != String.Empty)
					{
						var tokens = t.Tokenize(line);
						var parsed = p.Parse(tokens);
						IScheminType returnType = eval.Evaluate(parsed, global);
						Console.WriteLine(">> " + returnType.ToString());
					}
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
