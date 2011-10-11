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

namespace Schemin.Tokenize
{
	using System;
	using System.Collections.Generic;
	using System.Linq;
	using System.Text;

	public class Tokenizer
	{
		public List<Token> Tokenize(string input)
		{
			List<Token> tokenList = new List<Token>();
			int currentPosition = 0;
			char[] chars = input.ToCharArray();

			// We use KeyValuePair as a simple tuple type here, containing the next token
			// as well as what position we should resume retrieving tokens from.

			KeyValuePair<Token, int> currentToken;
			while ((currentToken = GetNextToken(chars, currentPosition)).Key != null)
			{
				currentPosition = currentToken.Value;
				tokenList.Add(currentToken.Key);
			}

			return tokenList;
		}

		private KeyValuePair<Token, int> GetNextToken(char[] input, int position)
		{
			int i = position;

			// Skip whitespace
			while (i < input.Length && Whitespace(input[i])) i++;

			// Skip comments and whitespace again, since we can have whitespace after comments etc.
			if (i < input.Length)
			{
				while (input[i] == ';')
				{
					while (i < input.Length && !Newline(input[i])) i++;
					i++;
					while (i < input.Length && Whitespace(input[i])) i++;
				}
			}

			// get the next token
			if (i < input.Length)
			{
				if (Numerical(input[i]))
					return NumberLiteral(input, i);

				switch(input[i])
				{
					case '#':
						return VectorOrBooleanOrChar(input, i);
					case '"':
						return StringLiteral(input, i);
					case '-':
					case '+':

						// only a number literal if the starting char is numerical, or if the starting char is a - or +
						// AND the next char is part of a number.
						if(Numerical(input[i + 1]) || input[i + 1] == '.')
							return NumberLiteral(input, i);
						else
							return Symbol(input, i);
					case '\'':
					case '`':
					case ',':
						return QuoteSugar(input, i);
					case '(':
						return MakeTokenPair(TokenType.OpenParen, new String(input, i, 1), input, i, i + 1);
					case ')':
						return MakeTokenPair(TokenType.CloseParen, new String(input, i, 1), input, i, i + 1);
					default:
						return Symbol(input, i);
				}
			}
			else
			{
				// no more characters, return an empty KVP
				return new KeyValuePair<Token,int>(null, 0);
			}
		}

		// retrieve a string literal token and advance our position
		private KeyValuePair<Token, int> StringLiteral(char[] input, int position)
		{
			int originalPosition = position;
			position++;
			TokenType type = TokenType.StringLiteral;
			StringBuilder sb = new StringBuilder();
			int newPosition = position;
			bool matchingQuotes = false;
			while (newPosition < input.Length)
			{
				// if we get an escape, increment the position some more and map the escaped character to what it should be
				if (input[newPosition] == '\\')
				{
					newPosition++;
					sb.Append(MapEscaped(input[newPosition]));
					newPosition++;
					continue;
				}

				// unescaped quote? We're done with this string.
				if (input[newPosition] == '"')
				{
					newPosition++;
					matchingQuotes = true;
					break;
				}

				sb.Append(input[newPosition]);
				newPosition++;
			}

			// we didn't get opening and closing quotes :(
			if (!matchingQuotes)
			{
				throw new Exception("Tokenizing error: Unmatched quotes in string literal");
			}

			return MakeTokenPair(type, sb.ToString(), input, originalPosition, newPosition);
		}

		// retrieve a symbol token and advance our position
		private KeyValuePair<Token, int> Symbol(char[] input, int position)
		{
			int originalPosition = position;
			TokenType type = TokenType.Symbol;

			int newPosition = position;
			// as long as we don't hit whitespace or something that a symbol shouldn't have, assume it's a symbol
			while (newPosition < input.Length && !Whitespace(input[newPosition]) && SymbolPart(input[newPosition]))
			{
				newPosition++;
			}

			return MakeTokenPair(type, new String(input, position, newPosition - position), input, originalPosition, newPosition);
		}

		// retrieve a quoted sugar token (' ` , ,@) and advance our position
		private KeyValuePair<Token, int> QuoteSugar(char[] input, int position)
		{
			int originalPosition = position;
			TokenType type;

			switch (input[position])
			{
				case '\'':
					type = TokenType.Quote;
					break;
				case '`':
					type = TokenType.BackQuote;
					break;
				case ',':
					type = TokenType.Comma;
					break;
				default:
					throw new Exception("Parse error!");
			}
			int positionOffset = 1;
			if ((position + 1) < input.Length && input[position + 1] == '@')
			{
				type = TokenType.AtComma;
				positionOffset = 2;
			}

			return MakeTokenPair(type, new String(input, position, positionOffset), input, originalPosition, position + positionOffset);
		}

		// retrieve a number literal token (int or decimal) and advance our position
		private KeyValuePair<Token, int> NumberLiteral(char[] input, int position)
		{
			int originalPosition = position;
			TokenType type = TokenType.IntegerLiteral;

			int newPosition = position;
			while (newPosition < input.Length && !Whitespace(input[newPosition]) && NumericalPart(input[newPosition]))
			{
				// if we get a decimal we're no longer working with an integer 
				if (input[newPosition] == '.')
					type = TokenType.DecimalLiteral;

				newPosition++;
			}

			return MakeTokenPair(type, new String(input, position, newPosition - position), input, originalPosition, newPosition);
		}

		// retrieve a vector literal marker, boolean token, or character literal token and advance our position
		private KeyValuePair<Token, int> VectorOrBooleanOrChar(char[] input, int position)
		{
			int originalPosition = position;
			List<char> boolLiterals = new List<char> { 'F', 'f', 't', 'T' };
			if (boolLiterals.Contains(input[position + 1]))
			{
				// boolean literal!
				return MakeTokenPair(TokenType.BoolLiteral, new String(input, position, 2), input, originalPosition, position + 2);
			}
			else if (input[position + 1] == '(')
			{
				// vector literal!
				return MakeTokenPair(TokenType.VectorLiteral, new String(input, position, 1), input, originalPosition, position + 1);
			}
			else if (input[position + 1] == '\\')
			{
				// char literal!
				StringBuilder sb = new StringBuilder();
				int newPosition = position;
				while (newPosition < input.Length && !Whitespace(input[newPosition]) && SymbolPart(input[newPosition]))
				{
					sb.Append(input[newPosition]);
					newPosition++;
				}
				return MakeTokenPair(TokenType.CharLiteral, new String(input, position, newPosition - position), input, originalPosition, newPosition);
			}
			else
			{
				throw new Exception("Tokenizing error: Inside '#' but no matching characters to construct a token");
			}
		}

		private bool Numerical(char candidate)
		{
			return Char.IsNumber(candidate);
		}

		private bool NumericalPart(char candidate)
		{
			char[] signs = { '-', '+' };
			return candidate == '.' || Numerical(candidate) || signs.Contains(candidate);
		}

		private bool SymbolPart(char candidate)
		{
			char[] notAllowed = { '(', ')', '"', '\'', ',', '`' };
			return !notAllowed.Contains(candidate);
		}

		private bool Newline(char candidate)
		{
			return candidate == '\r' || candidate == '\n';
		}

		private bool Whitespace(char candidate)
		{
			if (Newline(candidate))
			{
				return true;
			}

			switch (candidate)
			{
				case '\t':
				case ' ':
					return true;
			}

			return false;
		}

		private char MapEscaped(char escaped)
		{
			switch (escaped)
			{
				case 't':
					return '\t';
				case 'n':
					return '\n';
				case 'r':
					return '\r';
			}

			return escaped;
		}

		private KeyValuePair<Token, int> MakeTokenPair(TokenType type, String tokenName, char[] input, int beginPosition, int endPosition)
		{
			Token currentToken = new Token(type, tokenName);
			currentToken.LineNumber = GetLineNumber(input, beginPosition);
			currentToken.ColNumber = GetColNumber(input, beginPosition);
			return new KeyValuePair<Token, int>(currentToken, endPosition);
		}

		private int GetColNumber(char[] input, int position)
		{
			int colNumber = 0;
			for (int i = 0; i < position; i++)
			{
				if (input[i] == '\n')
				{
					colNumber = 0;
				}

				colNumber++;
			}

			return colNumber;
		}

		private int GetLineNumber(char[] input, int position)
		{
			int line = 1;
			for (int i = 0; i < position; i++)
			{
				if (input[i] == '\n')
				{
					line++;
				}
			}

			return line;
		}
	}
}
