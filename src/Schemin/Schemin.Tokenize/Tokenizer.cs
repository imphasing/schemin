/* 
 * Copyright (c) 2011 Alex Fort 
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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
