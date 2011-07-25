
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
		static void Main(string[] args)
		{
			Tokenizer t = new Tokenizer();

			string test = @"(+ 1 1 (* 2 2))";
			List<Token> tokens = t.Tokenize(test);

			Parser p = new Parser();
			ScheminList parsed = p.Parse(tokens);
			foreach (Token token in tokens)
			{
				Console.WriteLine(token.Value.ToString());
			}
			Console.WriteLine(parsed.ToString());
			
			Evaluator eval = new Evaluator();
			Environment env = new Environment();

			IScheminType returnType = eval.Evaluate(parsed, env);
			Console.WriteLine(returnType.ToString());
		}
	}
}
