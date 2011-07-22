
namespace Schemin
{
	using System;
	using System.Collections.Generic;
	using System.Text.RegularExpressions;
	using System.Linq;

	public class Tokenizer
	{
		public string ExtractLiterals(string input, Environment env)
		{
			int currentId = 0;
			string literalMatch = "\"([^\"\\\\]|\\.)*\"";
			Regex literal = new Regex(literalMatch);

			var literals = new Dictionary<string, string>();

			Func<Match, string> replacer = m => {
				string placeholder = string.Format("STRING_LITERAL_{0}", currentId++);
				literals.Add(placeholder, m.ToString());
				return placeholder;
			};

			MatchEvaluator evaluator = new MatchEvaluator(replacer);

			string transformed = literal.Replace(input, evaluator);

			env.stringLiterals = literals;
			return transformed;
		}

		public List<string> Tokenize(string input, Environment env)
		{
			string addedWhitespace = input.Replace("(", " ( ");
			addedWhitespace = addedWhitespace.Replace(")", " ) ");

			List<string> tokens = addedWhitespace.Split(' ').ToList();
			return tokens.Select(token => {
				if (token.Contains("STRING_LITERAL_"))
				{
					return env.stringLiterals[token];
				}
				else
				{
					return token;
				}
			}).ToList();
		}

	}
}
