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
	using System.Numerics;
	using Schemin.Tokenize;
	using Schemin.AST;
	using Schemin.Evaluate;
	using Schemin.Primitives;

	public class PairParser
	{
		public ScheminPair Parse(List<Token> tokens, bool quoteLists)
		{
			ScheminPair.QuoteLists = quoteLists;
			KeyValuePair<ScheminPair, int> parsed = ParseInternal(tokens, 0);
			ScheminPair.QuoteLists = true;
			return (ScheminPair) parsed.Key;
		}

		private KeyValuePair<ScheminPair, int> ParseInternal(List<Token> tokens, int startIndex)
		{
			ScheminPair parsed = new ScheminPair();

			while (startIndex < tokens.Count)
			{
				if (tokens[startIndex].Type == TokenType.VectorLiteral)
				{
					if (tokens[startIndex + 1].Type == TokenType.OpenParen)
					{
						KeyValuePair<ScheminPair, int> descended = ParseInternal(tokens, startIndex + 2);
						ScheminVector vec = new ScheminVector();
						foreach (IScheminType type in descended.Key)
						{
							type.Quote();
							vec.List.Add(type);
						}

						parsed = parsed.Append(vec);
						startIndex = descended.Value;
					}

				}
				else if (tokens[startIndex].Type == TokenType.OpenParen)
				{
					KeyValuePair<ScheminPair, int> descended = ParseInternal(tokens, startIndex + 1);
					parsed = parsed.Append(descended.Key);

					startIndex = descended.Value;
				}
				else if (tokens[startIndex].Type == TokenType.CloseParen)
				{
					break;
				}
				else if (tokens[startIndex].Type == TokenType.Quote)
				{
					parsed = parsed.Append(new ScheminAtom("'"));
				}
				else if (tokens[startIndex].Type == TokenType.BackQuote)
				{
					parsed = parsed.Append(new ScheminAtom("`"));
				}
				else if (tokens[startIndex].Type == TokenType.AtComma)
				{
					parsed = parsed.Append(new ScheminAtom(",@"));
				}
				else if (tokens[startIndex].Type == TokenType.Comma)
				{
					parsed = parsed.Append(new ScheminAtom(","));
				}
				else
				{
					IScheminType converted = ConvertToken(tokens[startIndex]);
					parsed = parsed.Append(converted);
				}

				startIndex++;
			}

			return new KeyValuePair<ScheminPair, int>(parsed, startIndex);
		}

		private IScheminType ConvertToken(Token token)
		{
			switch (token.Type)
			{
				case TokenType.Symbol:
					return ConvertAtom(new ScheminAtom(token.Value), token);
				case TokenType.IntegerLiteral:
					return new ScheminInteger(BigInteger.Parse(token.Value));
				case TokenType.DecimalLiteral:
					return new ScheminDecimal(decimal.Parse(token.Value));
				case TokenType.StringLiteral:
					return new ScheminString(token.Value);
				case TokenType.BoolLiteral:
					return ScheminBool.GetValue(token.Value == "#t" ? true : false);
				case TokenType.CharLiteral:
					return new ScheminChar(token.Value.Replace("#\\", ""));
				default:
					throw new Exception(string.Format("Unable to convert token of type: {0}", token.Type));
			}
		}

		private IScheminType ConvertAtom(ScheminAtom atom, Token token)
		{
			switch (atom.Name)
			{
				// these are special forms that can't be bound to symbols, they're always primitives
				case "lambda":
					return new ScheminPrimitive("lambda", token);
				case "define": 
					return new ScheminPrimitive("define", token);
				case "define-rewriter":
					return new ScheminPrimitive("define-rewriter", token);
				case "quote":
					return new ScheminPrimitive("quote", token);
				case "unquote":
					return new ScheminPrimitive("unquote", token);
				case "unquote-splicing":
					return new ScheminPrimitive("unquote-splicing", token);
				case "quasiquote":
					return new ScheminPrimitive("quasiquote", token);
				case "begin":
					return new ScheminPrimitive("begin", token);
				case "if":
					return new ScheminPrimitive("if", token);
				case "or":
					return new ScheminPrimitive("or", token);
				case "and":
					return new ScheminPrimitive("and", token);
				case "cond":
					return new ScheminPrimitive("cond", token);
				case "let":
					return new ScheminPrimitive("let", token);
				case "letrec":
					return new ScheminPrimitive("letrec", token);
				case "let*":
					return new ScheminPrimitive("let*", token);
				case "set!":
					return new ScheminPrimitive("set!", token);
				case "call/cc":
					return new ScheminPrimitive("call/cc", token);
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
						// and ''a with (quote (quote a))
						ScheminList newhead = new ScheminList(new ScheminPrimitive("quote"));
						TransformQuotes(c.Rest);

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

		private void TransformQuasiQuotes(ScheminList ast)
		{
			// this horrible code transforms quasiquoted literals into their expanded form
			// eg: `(a ,b) becomes (quasiquote a (unquote b))

			ScheminList c = ast;

			while (c != null)
			{
				IScheminType type = c.Head;

				if ((type as ScheminAtom) != null)
				{
					ScheminAtom atom = (ScheminAtom) type;
					if (atom.Name == "`")
					{
						ScheminList newhead = new ScheminList(new ScheminPrimitive("quasiquote"));
						TransformQuasiQuotes(c.Rest);
						newhead.Append(c.Rest.Head);

						c.Head = newhead;
						c.Rest = c.Rest.Rest;
						continue;
					}
					else if (atom.Name == ",@")
					{
						ScheminList newhead = new ScheminList(new ScheminPrimitive("unquote-splicing"));
						TransformQuasiQuotes(c.Rest);
						newhead.Append(c.Rest.Head);

						c.Head = newhead;
						c.Rest = c.Rest.Rest;
					}
					else if (atom.Name == ",")
					{
						ScheminList newhead = new ScheminList(new ScheminPrimitive("unquote"));
						TransformQuasiQuotes(c.Rest);
						newhead.Append(c.Rest.Head);

						c.Head = newhead;
						c.Rest = c.Rest.Rest;
					}
				}
				else if ((type as ScheminList) != null)
				{
					TransformQuasiQuotes((ScheminList) type);
				}

				c = c.Rest;
			}
		}
	}
}
