
namespace Schemin.Parse
{
	using System;
	using System.Collections.Generic;
	using Schemin.Tokenize;

	public class Parser
	{
		public KeyValuePair<SchemeList, int> Parse(List<Token> tokens, int startIndex)
		{
			var parsed = new SchemeList(new LinkedList<ISchemeType>());

			while (startIndex < tokens.Count)
			{
				if (tokens[startIndex].Type == TokenType.OpenParen)
				{
					KeyValuePair<SchemeList, int> descended = Parse(tokens, startIndex + 1);
					parsed.List.AddLast(descended.Key);
					startIndex = descended.Value;
				}
				else if (tokens[startIndex].Type == TokenType.CloseParen)
				{
					break;
				}
				else
				{
					ISchemeType converted = ConvertToken(tokens[startIndex]);
					parsed.List.AddLast(converted);
				}

				startIndex++;
			}

			return new KeyValuePair<SchemeList, int>(parsed, startIndex);
		}
				
		public ISchemeType ConvertToken(Token token)
		{
			switch (token.Type)
			{
				case TokenType.Symbol:
					return new SchemeAtom(token.Value);
				case TokenType.IntegerLiteral:
					return new SchemeInteger(int.Parse(token.Value));
				case TokenType.StringLiteral:
					return new SchemeString(token.Value);
				default:
					throw new Exception(string.Format("Unable to convert token of type: {0}", token.Type));
			}
		}
	}
}
