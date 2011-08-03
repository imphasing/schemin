
namespace Schemin.Parse
{
	using System;
	using System.Collections.Generic;
	using Schemin.Tokenize;
	using Schemin.AST;
	using Cadenza.Collections;
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
					return new ScheminAtom(token.Value); //ConvertAtom(new ScheminAtom(token.Value));
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
				case "lambda":
					return new ScheminPrimitive(GeneralOperations.Lambda, "lambda");
				case "+":
					return new ScheminPrimitive(NumericOperations.Add, "+");
				case "-":
					return new ScheminPrimitive(NumericOperations.Subtract, "-");
				case "*":
					return new ScheminPrimitive(NumericOperations.Multiply, "*");
				case "define": 
					return new ScheminPrimitive(GeneralOperations.Define, "define");
				case "dumpenv":
					return new ScheminPrimitive(GeneralOperations.DumpEnv, "dumpenv");
				case "quote":
					return new ScheminPrimitive(GeneralOperations.Quote, "quote");
				case "car":
					return new ScheminPrimitive(ListOperations.Car, "car");
				case "cons":
					return new ScheminPrimitive(ListOperations.Cons, "cons");
				case "cdr":
					return new ScheminPrimitive(ListOperations.Cdr, "cdr");
				case "cadr":
					return new ScheminPrimitive(ListOperations.Cadr, "cadr");
				case "cddr":
					return new ScheminPrimitive(ListOperations.Cddr, "cddr");
				case "length":
					return new ScheminPrimitive(ListOperations.Length, "length");
				case "list":
					return new ScheminPrimitive(ListOperations.List, "list");
				case "append":
					return new ScheminPrimitive(ListOperations.Append, "append");
				case "null?":
					return new ScheminPrimitive(BooleanOperations.Null, "null?");
				case "=":
					return new ScheminPrimitive(BooleanOperations.Equal, "=");
				case "eq?":
					return new ScheminPrimitive(BooleanOperations.Equal, "eq?");
				case "if":
					return new ScheminPrimitive(GeneralOperations.If, "if");
				case "map":
					return new ScheminPrimitive(ListOperations.Map, "map");
				case "filter":
					return new ScheminPrimitive(ListOperations.Filter, "filter");
				case "foldl":
					return new ScheminPrimitive(ListOperations.Foldl, "foldl");
				case ">":
					return new ScheminPrimitive(BooleanOperations.GreaterThan, ">");
				case "<":
					return new ScheminPrimitive(BooleanOperations.LessThan, "<");
				case "<=":
					return new ScheminPrimitive(BooleanOperations.LessThanOr, "<=");
				case "let":
					return new ScheminPrimitive(GeneralOperations.Let, "let");
				case "begin":
					return new ScheminPrimitive(GeneralOperations.Begin, "begin");
				case "set!":
					return new ScheminPrimitive(GeneralOperations.SetBang, "set!");
				case "display":
					return new ScheminPrimitive(GeneralOperations.Display, "display");
				default:
					return atom;
			}
		}
			
	}
}
