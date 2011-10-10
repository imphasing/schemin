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
	using Schemin.Evaluate;
	using Schemin.Primitives;

	using Environment = Schemin.Evaluate.Environment;

	public class ScheminLambda : IScheminType
	{
		public IScheminType Definition;
		public IScheminType Arguments;
		public Environment Closure;

		public ScheminLambda(ScheminPair definition, Environment closure)
		{
			this.Arguments = definition.Car;

			if (definition.ListCdr().Length == 1)
			{
				this.Definition = definition.ElementAt(1);
			}
			else
			{
				ScheminPair def = definition.ListCdr();
				def.Cons(new ScheminPrimitive("begin"));
				this.Definition = def;
			}

			this.Closure = closure;
		}

		public Environment MakeEnvironment(ScheminPair values, Evaluator eval)
		{
			if ((this.Arguments as ScheminPair) != null)
			{
				ScheminPair argslist = (ScheminPair) this.Arguments;
				IScheminType first = argslist.Car;
				ScheminPair rest = argslist.ListCdr();
				IScheminType firstArg = values.Car;
				ScheminPair restArgs = values.ListCdr();

				Environment args = new Environment();
				args.parent = this.Closure;

				for (; ;)
				{
					if (first == null || rest == null)
					{
						break;
					}

					ScheminAtom atom = (ScheminAtom) first;
					if (atom.Name == ".")
					{
						restArgs = restArgs.Cons(firstArg);
						restArgs.Quote();
						args.AddBinding((ScheminAtom) rest.Car, restArgs);
						break;
					}

					args.AddBinding((ScheminAtom) first, firstArg);

					first = rest.Car;
					firstArg = restArgs.Car;
					rest = rest.ListCdr();
					restArgs = restArgs.ListCdr();
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
