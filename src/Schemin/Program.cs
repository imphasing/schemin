
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

			foreach (KeyValuePair<string, string> literal in env.stringLiterals)
			{
				Console.WriteLine(string.Format("Literal: {0} has value: {1}", literal.Key, literal.Value));
			}


			List<string> tokens = t.Tokenize(noLiterals, env);

			foreach (string token in tokens)
			{
				Console.WriteLine(token);
			}
		}
	}
}
