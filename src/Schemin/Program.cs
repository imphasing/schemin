
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
			Environment env = new Environment();

			ScheminAtom arg1 = new ScheminAtom("x");
			ScheminPrimitive operation = new ScheminPrimitive(Primitives.Add, "+");
			ScheminInteger num = new ScheminInteger(2);
	

			ScheminList test = new ScheminList();
			ScheminList args = new ScheminList();
			ScheminList values = new ScheminList();

			args.Append(arg1);
			values.Append(num);

			test.Append(operation);
			test.Append(arg1);
			test.Append(arg1);

			ScheminLambda lambda = new ScheminLambda(test, args);
			IScheminType result = lambda.Evaluate(values, eval, env);

			Console.WriteLine("Lambda result: " + result.ToString());

			string line = String.Empty;
			while ((line = Console.ReadLine()) != null)
			{
				var tokens = t.Tokenize(line);
				var parsed = p.Parse(tokens);
				IScheminType returnType = eval.Evaluate(parsed, env, false);
				Console.WriteLine(returnType.ToString());
			}
		}
	}
}
