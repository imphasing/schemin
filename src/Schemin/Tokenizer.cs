
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

		public List<Token> Tokenize(string input, Environment env)
		{
			var tokens = new List<Token>();

			string addedWhitespace = input.Replace("(", " ( ");
			addedWhitespace = addedWhitespace.Replace(")", " ) ");
			string removedNewlines = addedWhitespace.Replace(System.Environment.NewLine, " ");

			List<string> stringTokens = removedNewlines.Split(' ').ToList();
			Func<string, bool> filter = token => (token != String.Empty && token != "");
			stringTokens = stringTokens.Where(filter).ToList();

			var matchTokenTypes = new Dictionary<Regex, TokenType>();
			matchTokenTypes.Add(new Regex("[-+]?[0-9]+"), TokenType.IntegerLiteral);
			matchTokenTypes.Add(new Regex("[^\"\',()]+"), TokenType.Symbol);
			matchTokenTypes.Add(new Regex("[(]"), TokenType.OpenParen);
			matchTokenTypes.Add(new Regex("[)]"), TokenType.CloseParen);

			foreach (string token in stringTokens)
			{
				bool goodToken = false;

				if (token.Contains("STRING_LITERAL_"))
				{
					tokens.Add(new Token(TokenType.StringLiteral, env.stringLiterals[token]));
					goodToken = true;
				}
				else
				{
					foreach (KeyValuePair<Regex, TokenType> kvp in matchTokenTypes)
					{
						Match m = kvp.Key.Match(token);
						if (m.Success)
						{
							tokens.Add(new Token(kvp.Value, token));
							goodToken = true;
							break;
						}
					}
				}

				if (!goodToken)
				{
					Console.WriteLine(string.Format("Error: Bad Token \"{0}\"", token));
				}
			}

			return tokens;
		}

	}
}
