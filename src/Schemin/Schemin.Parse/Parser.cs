
namespace Schemin.Parse
{
	using System;
	using System.Collections.Generic;
	using Schemin.Tokenize;
	using Schemin.AST;
	using Schemin.Evaluate;
	using Schemin.Evaluate.Primitives;

	public class Parser
	{
		public ScheminList Parse(List<Token> tokens)
		{
			KeyValuePair<ScheminList, int> parsed = ParseInternal(tokens, 0);
			return (ScheminList) parsed.Key;
		}

		private KeyValuePair<ScheminList, int> ParseInternal(List<Token> tokens, int startIndex)
		{
			ScheminList parsed = new ScheminList();

			while (startIndex < tokens.Count)
			{
				if (tokens[startIndex].Type == TokenType.OpenParen)
				{
					KeyValuePair<ScheminList, int> descended = ParseInternal(tokens, startIndex + 1);
					parsed = parsed.Append(descended.Key);

					startIndex = descended.Value;
				}
				else if (tokens[startIndex].Type == TokenType.CloseParen)
				{
					break;
				}
				else
				{
					IScheminType converted = ConvertToken(tokens[startIndex]);
					parsed = parsed.Append(converted);
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
					return ConvertAtom(new ScheminAtom(token.Value));
				case TokenType.IntegerLiteral:
					return new ScheminInteger(int.Parse(token.Value));
				case TokenType.StringLiteral:
					return new ScheminString(token.Value);
				case TokenType.BoolLiteral:
					return new ScheminBool(token.Value);
				default:
					throw new Exception(string.Format("Unable to convert token of type: {0}", token.Type));
			}
		}

		private IScheminType ConvertAtom(ScheminAtom atom)
		{
			switch (atom.Name)
			{
				// these are special forms that can't be bound to symbols, they're always primitives
				case "lambda":
					return new ScheminPrimitive(GeneralOperations.Lambda, "lambda");
				case "define": 
					return new ScheminPrimitive(GeneralOperations.Define, "define");
				case "quote":
					return new ScheminPrimitive(GeneralOperations.Quote, "quote");
				case "if":
					return new ScheminPrimitive(GeneralOperations.If, "if");
				case "let":
					return new ScheminPrimitive(GeneralOperations.Let, "let");
				default:
					return atom;
			}
		}
			
	}
}
