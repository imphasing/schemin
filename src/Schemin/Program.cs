
namespace Schemin
{
	using System;
	using System.Collections.Generic;
	using System.Linq;
	using System.Text;
	using Schemin.Tokenize;
	using Schemin.Parse;
	using Schemin.AST;

	class Program
	{
		static void Main(string[] args)
		{
			Tokenizer t = new Tokenizer();
			Environment env = new Environment();

			string test = @"(define (add1 x) (+ x 1))";
			List<Token> tokens = t.Tokenize(test);

			foreach (Token token in tokens)
			{
				Console.WriteLine("Token: " + token.Value + " Type: " + token.Type);
			}

			Parser p = new Parser();
			ScheminList parsed = p.Parse(tokens);
			
			DisplayList(parsed, 0);
		}


		static void DisplayList(ScheminList list, int level)
		{
			foreach (var type in list.List)
			{
				if (type.GetType() == typeof(ScheminList))
				{
					Console.WriteLine("Descending into a list...");
					DisplayList((ScheminList) type, level + 1);
				}
				else
				{
					string value = String.Empty;
					string levelPadding = String.Empty;

					for (int i = 0; i < level; i++)
					{
						levelPadding += "        ";
					}

					if (type.GetType() == typeof(ScheminAtom))
					{
						ScheminAtom atom = (ScheminAtom) type;	
						value = atom.Name;
					}
					else if (type.GetType() == typeof(ScheminString))
					{
						ScheminString str = (ScheminString) type;
						value = str.Value;
					}
					else if (type.GetType() == typeof(ScheminInteger))
					{
						ScheminInteger num = (ScheminInteger) type;
						value = num.Value.ToString();
					}

					Console.WriteLine(string.Format("{0}Type: {1} Value: {2}", levelPadding, type.GetType().ToString(), value));
				}
			}
		}
	}
}
