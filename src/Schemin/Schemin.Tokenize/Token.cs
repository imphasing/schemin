
namespace Schemin.Tokenize
{
	using System;

	public class Token
	{
		public TokenType Type;
		public string Value;

		public Token(TokenType type, string value)
		{
			this.Type = type;
			this.Value = value;
		}
	}
}
