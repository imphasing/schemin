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

namespace Schemin.Primitives.GeneralOperations
{
	using Schemin.Evaluate;
	using Schemin.AST;

	public class Let : Primitive
	{
		public Let()
		{
			base.Rewriter = true;
		}

		public override IScheminType Execute(Environment env, Evaluator eval, ScheminPair args)
		{
			bool isNamed = false;
			if ((args.Car as ScheminAtom) != null)
			{
				isNamed = true;
			}

			ScheminPair bindings;
			ScheminPair expression;

			if (!isNamed)
			{
				expression = args.ListCdr();
				bindings = (ScheminPair)args.Car;
			}
			else
			{
				expression = args.ListCdr().ListCdr();
				bindings = (ScheminPair)args.ElementAt(1);
			}

			ScheminPair letArgs = new ScheminPair();
			ScheminPair argExps = new ScheminPair();

			foreach (ScheminPair bindingPair in bindings)
			{
				letArgs = letArgs.Append(bindingPair.Car);
				argExps = argExps.Append(bindingPair.ElementAt(1));
			}

			ScheminPair lambdaDef = new ScheminPair(letArgs);

			foreach (IScheminType type in expression)
			{
				lambdaDef = lambdaDef.Append(type);
			}

			Environment closure = env;
			if (isNamed)
			{
				closure = new Environment();
				closure.parent = env;
			}

			ScheminLambda lam = new ScheminLambda(lambdaDef, closure);

			if (isNamed)
			{
				ScheminAtom name = (ScheminAtom)args.Car;
				closure.AddBinding(name, lam);
			}

			ScheminPair toEvaluate = new ScheminPair(lam);

			foreach (IScheminType arg in argExps)
			{
				toEvaluate = toEvaluate.Append(arg);
			}

			return toEvaluate;
		}

		public override void CheckArguments(ScheminPair args)
		{
			IScheminType first = args.Car;

			if ((first as ScheminPair) == null)
			{
				if ((first as ScheminAtom) == null)
				{
					throw new BadArgumentsException("first argument must be a symbol");
				}
			}

			return;
		}
	}
}
