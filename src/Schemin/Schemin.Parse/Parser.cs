
namespace Schemin.Parse
{
	using System;
	using System.Collections.Generic;
	using Schemin.Tokenize;
	using Schemin.AST;
	using Cadenza.Collections;
	using Schemin.Evaluate;

	public class Parser
	{
		public ScheminList Parse(List<Token> tokens)
		{
			KeyValuePair<ScheminList, int> parsed = ParseInternal(tokens, 0);
			if (parsed.Key.List.Count() < 2)
			{
				return parsed.Key;
			}

			return (ScheminList) parsed.Key.Cdr();
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
				default:
					throw new Exception(string.Format("Unable to convert token of type: {0}", token.Type));
			}
		}

		private IScheminType ConvertAtom(ScheminAtom atom)
		{
			switch (atom.Name)
			{
				case "+":
					return new ScheminPrimitive(Primitives.Add, "+");
				case "-":
					return new ScheminPrimitive(Primitives.Subtract, "-");
				case "*":
					return new ScheminPrimitive(Primitives.Multiply, "*");
				case "define": 
					return new ScheminPrimitive(Primitives.Define, "define");
				case "dumpenv":
					return new ScheminPrimitive(Primitives.DumpEnv, "dumpenv");
				default:
					return atom;
			}
		}
			
	}
}
