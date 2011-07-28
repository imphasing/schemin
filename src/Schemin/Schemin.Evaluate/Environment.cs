
namespace Schemin.Evaluate
{
	using System;
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

		public bool HasValue(ScheminAtom symbol)
		{
			if (bindings.ContainsKey(symbol.Name))
			{
				return true;
			}
			else
			{
				return false;
			}
		}

				
		public IScheminType GetValue(ScheminAtom symbol)
		{
			return bindings[symbol.Name];
		}
	}
}
