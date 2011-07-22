
namespace Schemin
{
	using System;
	using System.Collections.Generic;
	using System.Linq;
	using System.Text;
	using Schemin.Tokenize;
	using Schemin.Parse;

	class Program
	{
		static void Main(string[] args)
		{
			Tokenizer t = new Tokenizer();
			Environment env = new Environment();

			string test = @"
(define test (lambda () (
   begin 
   (print ""test"")
   (print 1)
 )))
 
 (test)";
			string noLiterals = t.ExtractLiterals(test, env);

			List<Token> tokens = t.Tokenize(noLiterals, env);

			Parser p = new Parser();
			KeyValuePair<SchemeList, int> parsed = p.Parse(tokens, 0);
			
			DisplayList(parsed.Key, 0);
		}


		static void DisplayList(SchemeList list, int level)
		{
			foreach (var type in list.List)
			{
				if (type.GetType() == typeof(SchemeList))
				{
					Console.WriteLine("Descending into a list...");
					DisplayList((SchemeList) type, level + 1);
				}
				else
				{
					string value = String.Empty;
					string levelPadding = String.Empty;

					for (int i = 0; i < level; i++)
					{
						levelPadding += "        ";
					}

					if (type.GetType() == typeof(SchemeAtom))
					{
						SchemeAtom atom = (SchemeAtom) type;	
						value = atom.Name;
					}
					else if (type.GetType() == typeof(SchemeString))
					{
						SchemeString str = (SchemeString) type;
						value = str.Value;
					}
					else if (type.GetType() == typeof(SchemeInteger))
					{
						SchemeInteger num = (SchemeInteger) type;
						value = num.Value.ToString();
					}


					Console.WriteLine(string.Format("{0}Type: {1} Value: {2}", levelPadding, type.GetType().ToString(), value));
				}
			}
		}
	}
}
