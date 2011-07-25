
namespace Schemin.Parse
{
	using System;
	using System.Collections.Generic;
	using Schemin.Tokenize;
	using Schemin.AST;

	public class Parser
	{
		public ScheminList Parse(List<Token> tokens)
		{
			KeyValuePair<ScheminList, int> parsed = ParseInternal(tokens, 0);
			return parsed.Key;
		}

		private KeyValuePair<ScheminList, int> ParseInternal(List<Token> tokens, int startIndex)
		{
			var parsed = new ScheminList(new LinkedList<IScheminType>());

			while (startIndex < tokens.Count)
			{
				if (tokens[startIndex].Type == TokenType.OpenParen)
				{
					KeyValuePair<ScheminList, int> descended = ParseInternal(tokens, startIndex + 1);
					parsed.List.AddLast(descended.Key);
					startIndex = descended.Value;
				}
				else if (tokens[startIndex].Type == TokenType.CloseParen)
				{
					break;
				}
				else
				{
					IScheminType converted = ConvertToken(tokens[startIndex]);
					parsed.List.AddLast(converted);
				}

				startIndex++;
			}

			return new KeyValuePair<ScheminList, int>(parsed, startIndex);
		}
				
		private IScheminType ConvertToken(Token token)
		{
			switch (token.Type)
			{
				case TokenType.Symbol:
					return new ScheminAtom(token.Value);
				case TokenType.IntegerLiteral:
					return new ScheminInteger(int.Parse(token.Value));
				case TokenType.StringLiteral:
					return new ScheminString(token.Value);
				default:
					throw new Exception(string.Format("Unable to convert token of type: {0}", token.Type));
			}
		}
	}
}
