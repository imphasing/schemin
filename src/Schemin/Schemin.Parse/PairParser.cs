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
	using System.Linq;
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
			TransformQuoteTokens(tokens);
			int currentPosition = 0;

			ScheminPair totalParsed = new ScheminPair();
			while (currentPosition < tokens.Count)
			{
				totalParsed = totalParsed.Append(ParseTopLevel(tokens, ref currentPosition));
			}

			return totalParsed;
		}

		private IScheminType ParseTopLevel(List<Token> tokens, ref int currentPosition)
		{
			switch (tokens[currentPosition].Type)
			{
				case TokenType.OpenParen:
					if (tokens[currentPosition + 1].Type == TokenType.CloseParen)
					{
						currentPosition++;
						return new ScheminPair();
					}
					else
					{
						currentPosition++;
						return ParseList(tokens, ref currentPosition);
					}

				case TokenType.VectorLiteral:
					return ParseVector(tokens, ref currentPosition);

				default:
					IScheminType converted = ConvertToken(tokens[currentPosition]);
					currentPosition++;
					return converted;    
			}
		}

		private IScheminType ParseList(List<Token> tokens, ref int currentPosition)
		{
			ScheminPair built = new ScheminPair();

			switch (tokens[currentPosition].Type)
			{
				case TokenType.CloseParen:
					if (tokens[currentPosition - 1].Type == TokenType.OpenParen)
					{
						currentPosition++;
						return new ScheminPair();
					}

					currentPosition++;
					return null;

				case TokenType.OpenParen:
					currentPosition++;
					built.Car = ParseList(tokens, ref currentPosition);
					built.Cdr = ParseList(tokens, ref currentPosition);
					return built;

				case TokenType.VectorLiteral:
					built.Car = ParseVector(tokens, ref currentPosition);
					built.Cdr = ParseList(tokens, ref currentPosition);
					return built;

				case TokenType.Dot:
					currentPosition++;
					return ParseDot(tokens, ref currentPosition);

				default:
					built.Car = ConvertToken(tokens[currentPosition]);
					currentPosition++;
					built.Cdr = ParseList(tokens, ref currentPosition);
					return built;
			}
		}

		private IScheminType ParseVector(List<Token> tokens, ref int currentPosition)
		{
			currentPosition += 2;
			ScheminVector parsed = ((ScheminPair) ParseList(tokens, ref currentPosition)).ToVector();
			return parsed;
		}

		private IScheminType ParseDot(List<Token> tokens, ref int currentPosition)
		{
			switch (tokens[currentPosition].Type)
			{
				case TokenType.OpenParen:
					currentPosition++;
					IScheminType parsed = ParseList(tokens, ref currentPosition);
					currentPosition++;
					return parsed;
				case TokenType.VectorLiteral:
					IScheminType parsedVec = ParseVector(tokens, ref currentPosition);
					currentPosition++;
					return parsedVec;
				default:
					var converted = ConvertToken(tokens[currentPosition]);
					currentPosition += 2;
					return converted;
			}
		}

		private void TransformQuoteTokens(List<Token> tokens)
		{
			for (int i = 0; i < tokens.Count; i++)
			{
				switch (tokens[i].Type)
				{
					case TokenType.Quote:
					case TokenType.BackQuote:
					case TokenType.Comma:
					case TokenType.AtComma:
						RemapQuoteToken(tokens, i);
						break;
				}
			}
		}

		private void RemapQuoteToken(List<Token> tokens, int currentPosition)
		{
			tokens[currentPosition] = RemapQuote(tokens[currentPosition]);
			tokens.Insert(currentPosition, new Token(TokenType.OpenParen, "("));
			int quotedTermination = FindQuotedTermination(tokens, currentPosition + 2);

			tokens.Insert(quotedTermination, new Token(TokenType.CloseParen, ")"));
		}

		private IScheminType ConvertToken(Token token)
		{
			switch (token.Type)
			{
				case TokenType.Symbol:
					return ConvertAtom(AtomFactory.GetAtom(token.Value), token);
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
				case "case":
					return new ScheminPrimitive("case", token);
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

		private Token RemapQuote(Token quote)
		{
			Dictionary<string, Token> remapped = new Dictionary<string, Token>();
			remapped.Add("'", new Token(TokenType.Symbol, "quote"));
			remapped.Add("`", new Token(TokenType.Symbol, "quasiquote"));
			remapped.Add(",", new Token(TokenType.Symbol, "unquote"));
			remapped.Add(",@", new Token(TokenType.Symbol, "unquote-splicing"));

			return remapped[quote.Value];
		}

		private int FindQuotedTermination(List<Token> tokens, int startIndex)
		{
			switch (tokens[startIndex].Type)
			{
				case TokenType.OpenParen:
					startIndex++;
					return FindMatchingParen(tokens, startIndex);

				case TokenType.VectorLiteral:
					startIndex += 2;
					return FindMatchingParen(tokens, startIndex);

				case TokenType.AtComma:
				case TokenType.BackQuote:
				case TokenType.Quote:
				case TokenType.Comma:
					while (!(IsQuotable(tokens[startIndex])))
						startIndex++;
					return startIndex + 1;

				default:
					return startIndex + 1;
			}
		}

		private bool IsQuoteToken(Token token)
		{
			string[] quotes = { "'", "`", ",", ",@" };
			if (quotes.Contains(token.Value))
			{
				return true;
			}

			return false;
		}

		private bool IsQuotable(Token token)
		{
			TokenType[] quotables = { TokenType.DecimalLiteral, TokenType.IntegerLiteral, TokenType.OpenParen,
				TokenType.StringLiteral, TokenType.Symbol, TokenType.VectorLiteral };
			if (quotables.Contains(token.Type))
			{
				return true;
			}
			else
			{
				return false;
			}
		}

		private int FindMatchingParen(List<Token> tokens, int startIndex)
		{
			int neededClosing = 1;
			while (neededClosing != 0)
			{
				if (tokens[startIndex].Type == TokenType.CloseParen)
				{
					neededClosing--;
				}
				else if (tokens[startIndex].Type == TokenType.OpenParen)
				{
					neededClosing++;
				}

				if (neededClosing == 0)
					break;

				startIndex++;
			}

			return startIndex;
		}
	}
}
