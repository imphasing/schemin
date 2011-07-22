
namespace Schemin
{
	using System;
	using System.Collections.Generic;
	using System.Linq;
	using System.Text;

	class Program
	{
		static void Main(string[] args)
		{
			Tokenizer t = new Tokenizer();
			Environment env = new Environment();

			string test = "(display \"this is a test\" \"and another test\" \"and another\")";
			string noLiterals = t.ExtractLiterals(test, env);

			List<Token> tokens = t.Tokenize(noLiterals, env);

			foreach (Token token in tokens)
			{
				Console.WriteLine(string.Format("Token: {0}, Type: {1}", token.Value, token.Type));
			}
		}
	}
}
