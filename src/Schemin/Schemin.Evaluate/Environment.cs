
namespace Schemin.Evaluate
{
	using System;
	using System.Text;
	using System.Collections.Generic;
	using Schemin.AST;
	
	public class Environment
	{
		public Dictionary<string, IScheminType> bindings;
		public Environment parent = null;

		public Environment()
		{
			bindings = new Dictionary<string, IScheminType>();
		}

		public void AddBinding(ScheminAtom symbol, IScheminType type)
		{
			if (bindings.ContainsKey(symbol.Name))
			{
				bindings[symbol.Name] = type;
			}
			else
			{
				bindings.Add(symbol.Name, type);
			}
		}

		public void RemoveBinding(ScheminAtom symbol)
		{
			if (bindings.ContainsKey(symbol.Name))
			{
				bindings.Remove(symbol.Name);
			}
		}

		public void CloseOver(Environment env)
		{
			foreach (KeyValuePair<string, IScheminType> binding in env.bindings)
			{
				this.AddBinding(new ScheminAtom(binding.Key), binding.Value);
			}

			if (env.parent == null)
			{
				return;
			}
			
			CloseOver(env.parent);
		}

		public void CloseOverTop(Environment env)
		{
			foreach (KeyValuePair<string, IScheminType> binding in env.bindings)
			{
				this.AddBinding(new ScheminAtom(binding.Key), binding.Value);
			}
		}


		public IScheminType GetValue(ScheminAtom symbol)
		{
			return bindings[symbol.Name];
		}

		public override string ToString()
		{
			StringBuilder builder = new StringBuilder();

			foreach (KeyValuePair<string, IScheminType> kvp in bindings)
			{
				builder.Append(string.Format("({0} => {1}), ", kvp.Key, kvp.Value));
			}

			return builder.ToString();
		}
	}
}
