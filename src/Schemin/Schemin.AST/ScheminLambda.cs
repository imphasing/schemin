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

namespace Schemin.AST
{
	using System;
	using System.Collections.Generic;
	using Schemin.Evaluate;
	using Schemin.Primitives;
	using Environment = Schemin.Evaluate.Environment;

	public class ScheminLambda : IScheminType
	{
		public ScheminPair Definition;
		public IScheminType Arguments;
		public Environment Closure;

		public ScheminLambda(ScheminPair definition, Environment closure)
		{
			this.Arguments = definition.Car;

			ScheminPair def = definition.ListCdr();
			def = def.Cons(new ScheminPrimitive("begin"));
			this.Definition = def;

			this.Closure = closure;
		}

		public Environment MakeEnvironment(ScheminPair values, Evaluator eval)
		{
			if ((this.Arguments as ScheminPair) != null)
			{
				var argBindings = ExtractArguments(values, (ScheminPair) this.Arguments);

				Environment args = new Environment();
				args.parent = this.Closure;

				foreach (KeyValuePair<ScheminAtom, IScheminType> binding in argBindings)
				{
					args.AddBinding(binding.Key, binding.Value);
				}

				return args;
			}
			else
			{
				Environment args = new Environment();
				args.parent = this.Closure;
				values.Quote();
				args.AddBinding((ScheminAtom) this.Arguments, values);
				return args;
			}
		}

		private Dictionary<ScheminAtom, IScheminType> ExtractArguments(ScheminPair values, ScheminPair arguments)
		{
			Dictionary<ScheminAtom, IScheminType> bindings = new Dictionary<ScheminAtom, IScheminType>();

			if (arguments.Proper)
			{
				if (!arguments.Empty)
				{
					for (int i = 0; i < arguments.Length; i++)
					{
						bindings.Add((ScheminAtom)arguments.ElementAt(i), values.ElementAt(i));
					}
				}

				return bindings;
			}
			else
			{
				IScheminType args = arguments;
				ScheminPair vals = values;

				while ((args as ScheminPair) != null)
				{
					ScheminPair pairArgs = (ScheminPair)args;

					ScheminAtom arg = (ScheminAtom)pairArgs.Car;
					IScheminType value = vals.Car;

					value.Quote();
					bindings.Add(arg, value);

					args = pairArgs.Cdr;

					if ((args as ScheminPair) != null)
						vals = (ScheminPair)vals.Cdr;
				}

				vals.Cdr.Quote();
				bindings.Add((ScheminAtom)args, vals.Cdr);

				return bindings;
			}
		}

		public override string ToString()
		{
			return "<Lambda>";
		}

		public bool Quoted()
		{
			return false;
		}

		public void Quote()
		{
		}

		public void UnQuote()
		{
		}

		public bool Equals(IScheminType type)
		{
			if ((type as ScheminLambda) != null)
			{
				if (type == this)
				{
					return true;
				}
			}

			return false;
		}

		public ScheminBool BoolValue()
		{
			return ScheminBool.True;
		}
	}
}
