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

namespace Schemin.Primitives.VectorOperations
{
	using Schemin.Evaluate;
	using Schemin.AST;
	public class MakeVector : Primitive
	{
		public override IScheminType Execute(Environment env, Evaluator eval, ScheminList args)
		{
			ScheminInteger len = (ScheminInteger) args.Car();
			IScheminType init = args.Cdr().Car();

			ScheminVector vec = new ScheminVector();
			if (args.Length == 1)
			{
				ScheminList empty = new ScheminList(true);
				for (int i = 0; i < len.IntegerValue(); i++)
				{
					vec.List.Add(empty);
				}
			}
			else
			{
				for (int i = 0; i < len.IntegerValue(); i++)
				{
					vec.List.Add(init);
				}
			}

			return vec;
		}

		public override void CheckArguments(ScheminList args)
		{
			IScheminType first = args.Car();
			IScheminType second = args.Cdr().Car();
			
			if (args.Length > 2 || args.Length < 1)
			{
				throw new BadArgumentsException("expected 1 or 2 arguments");
			}

			if ((first as ScheminInteger) == null)
			{
				throw new BadArgumentsException("first argument must be an integer");
			}

			return;
		}
	}
}
