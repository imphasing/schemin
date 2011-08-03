﻿
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
			while ((line = Console.ReadLine()) != null)
			{
				var tokens = t.Tokenize(line);
				var parsed = p.Parse(tokens);
				IScheminType returnType = eval.Evaluate(parsed, global);
				Console.WriteLine(returnType.ToString());
			}
		}
	}
}
