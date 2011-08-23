
namespace Schemin.Tokenize
{
	using System;
	using System.Collections.Generic;
	using System.Text.RegularExpressions;
	using System.Text;
	using System.Linq;

	public class Tokenizer
	{
		private Dictionary<string, string> literals;	

		public Tokenizer()
		{
			literals = new Dictionary<string, string>();
		}

		private string ExtractLiterals(string input)
		{
			int currentId = 0;
			string literalMatch = "\"([^\"\\\\]|\\.)*\"";
			Regex literal = new Regex(literalMatch);

			var extractedLiterals = new Dictionary<string, string>();

			Func<Match, string> replacer = m => {
				string placeholder = string.Format("###STRING_LITERAL_{0}###", currentId++);
				extractedLiterals.Add(placeholder, TrimQuotes(m.ToString()));
				return placeholder;
			};

			MatchEvaluator evaluator = new MatchEvaluator(replacer);

			string transformed = literal.Replace(input, evaluator);

			this.literals = extractedLiterals;
			return transformed;
		}

		private string RemoveComments(string input)
		{
			string commentMatch = ";.*$";
			Regex comment = new Regex(commentMatch);

			StringBuilder removed = new StringBuilder();
			string[] lines = input.Split(new string[] { "\r\n", "\n" }, StringSplitOptions.None);

			foreach (string line in lines)
			{
				removed.Append(comment.Replace(line, String.Empty));
				removed.Append(System.Environment.NewLine);
			}

			return removed.ToString();
		}

		private string TrimQuotes(string input)
		{
			string leading = input.Remove(0, 1);
			return leading.Remove(leading.Length - 1, 1);
		}

		public List<Token> Tokenize(string input)
		{
			var tokens = new List<Token>();

			// Normalize then remove newlines
			string normalizedNewlines = input.Replace(System.Environment.NewLine, "\n");
			normalizedNewlines = normalizedNewlines.Replace("\n", System.Environment.NewLine);

			// Remove string literals so our type recognition doesn't screw up
			string removedLiterals = ExtractLiterals(normalizedNewlines);

			// Remove comments
			string removedComments = RemoveComments(removedLiterals);

			string removedNewlines = removedComments.Replace(System.Environment.NewLine, String.Empty);
			string removedTabs = removedNewlines.Replace("\t", String.Empty);

			string addedWhitespace = removedTabs.Replace("(", " ( ");
			addedWhitespace = addedWhitespace.Replace(")", " ) ");

			List<string> stringTokens = addedWhitespace.Split(' ').ToList();
			Func<string, bool> filter = token => (token != String.Empty && token != "" && token != System.Environment.NewLine);
			stringTokens = stringTokens.Where(filter).ToList();

			var matchTokenTypes = new Dictionary<Regex, TokenType>();
			matchTokenTypes.Add(new Regex("^#[f|t]"), TokenType.BoolLiteral);
			matchTokenTypes.Add(new Regex("^[-+]?[0-9]+$"), TokenType.IntegerLiteral);
			matchTokenTypes.Add(new Regex("^([0-9]*)?(\\.[0-9]+)$"), TokenType.DecimalLiteral);
			matchTokenTypes.Add(new Regex("[^\"\',()]+"), TokenType.Symbol);
			matchTokenTypes.Add(new Regex("[']"), TokenType.Quote);
			matchTokenTypes.Add(new Regex("[(]"), TokenType.OpenParen);
			matchTokenTypes.Add(new Regex("[)]"), TokenType.CloseParen);

			foreach (string token in stringTokens)
			{
				string working = token;
				bool goodToken = false;

				// Since we replace ( with ' ( ', a leading quote like '(1 2) will automatically be interpreted
				// as two tokens, but for atoms, ints, etc, there's no spaces added so we need to split the token apart here
				if (working.StartsWith("'") && working != "'")
				{
					string quoted = token.Substring(1, token.Length - 1);
					tokens.Add(new Token(TokenType.Quote, "'"));
					working = quoted;
				}

				if (working.Contains("###STRING_LITERAL_"))
				{
					tokens.Add(new Token(TokenType.StringLiteral, this.literals[working]));
					goodToken = true;
				}
				else
				{
					foreach (KeyValuePair<Regex, TokenType> kvp in matchTokenTypes)
					{
						Match m = kvp.Key.Match(working);
						if (m.Success)
						{
							tokens.Add(new Token(kvp.Value, working));
							goodToken = true;
							break;
						}
					}
				}

				if (!goodToken)
				{
					Console.WriteLine(string.Format("Error: Bad Token \"{0}\"", working));
				}
			}

			return tokens;
		}
	}
}
