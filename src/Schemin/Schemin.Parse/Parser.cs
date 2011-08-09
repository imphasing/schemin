
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
			TransformQuotes((ScheminList) parsed.Key);
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
					parsed.Append(descended.Key);

					startIndex = descended.Value;
				}
				else if (tokens[startIndex].Type == TokenType.CloseParen)
				{
					break;
				}
				else if (tokens[startIndex].Type == TokenType.Quote)
				{
					parsed.Append(new ScheminAtom("'"));
				}
				else
				{
					IScheminType converted = ConvertToken(tokens[startIndex]);
					parsed.Append(converted);
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
				case TokenType.DecimalLiteral:
					return new ScheminDecimal(decimal.Parse(token.Value));
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
				case "or":
					return new ScheminPrimitive(BooleanOperations.Or, "or");
				case "and":
					return new ScheminPrimitive(BooleanOperations.And, "and");
				case "cond":
					return new ScheminPrimitive(GeneralOperations.Cond, "cond");
				case "let":
					return new ScheminPrimitive(GeneralOperations.Let, "let");
				case "letrec":
					return new ScheminPrimitive(GeneralOperations.LetRec, "letrec");
				case "set!":
					return new ScheminPrimitive(GeneralOperations.SetBang, "set!");
				default:
					return atom;
			}
		}

		private void TransformQuotes(ScheminList ast)
		{
			ScheminList c = ast;

			while (c != null)
			{
				IScheminType type = c.Head;

				if ((type as ScheminAtom) != null)
				{
					ScheminAtom atom = (ScheminAtom) type;
					if (atom.Name == "'")
					{
						// This pass replaces a list like (' a b) with ((quote a) b)
						ScheminList newhead = new ScheminList(new ScheminPrimitive(GeneralOperations.Quote, "quote"));
						newhead.Append(c.Rest.Head);

						c.Head = newhead;
						c.Rest = c.Rest.Rest;
					}
				}
				else if ((type as ScheminList) != null)
				{
					TransformQuotes((ScheminList) type);
				}

				c = c.Rest;
			}
		}
			
	}
}
