
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
				case "quote":
					return new ScheminPrimitive(Primitives.Quote, "quote");
				case "car":
					return new ScheminPrimitive(Primitives.Car, "car");
				case "cons":
					return new ScheminPrimitive(Primitives.Cons, "cons");
				case "cdr":
					return new ScheminPrimitive(Primitives.Cdr, "cdr");
				case "=":
					return new ScheminPrimitive(Primitives.Equal, "=");
				case "if":
					return new ScheminPrimitive(Primitives.If, "if");
				case "map":
					return new ScheminPrimitive(Primitives.Map, "map");
				case ">":
					return new ScheminPrimitive(Primitives.GreaterThan, ">");
				case "<":
					return new ScheminPrimitive(Primitives.LessThan, "<");
				case "let":
					return new ScheminPrimitive(Primitives.Let, "let");
				case "begin":
					return new ScheminPrimitive(Primitives.Begin, "begin");
				case "set!":
					return new ScheminPrimitive(Primitives.SetBang, "set!");
				case "display":
					return new ScheminPrimitive(Primitives.Display, "display");
				default:
					return atom;
			}
		}
			
	}
}
