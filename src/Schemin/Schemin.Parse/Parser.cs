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
		public ScheminList Parse(List<Token> tokens, bool quoteLists)
		{
			ScheminList.QuoteLists = quoteLists;
			KeyValuePair<ScheminList, int> parsed = ParseInternal(tokens, 0);
			TransformQuotes((ScheminList) parsed.Key);
			ScheminList.QuoteLists = true;
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
					return ScheminBool.GetValue(token.Value == "#t" ? true : false);
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
				case "begin":
					return new ScheminPrimitive(GeneralOperations.Begin, "begin");
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
				case "let*":
					return new ScheminPrimitive(GeneralOperations.LetStar, "let*");
				case "set!":
					return new ScheminPrimitive(GeneralOperations.SetBang, "set!");
				case "call/cc":
					return new ScheminPrimitive(GeneralOperations.CallCC, "call/cc");
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
