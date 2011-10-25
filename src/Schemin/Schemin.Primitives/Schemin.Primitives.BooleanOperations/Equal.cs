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

namespace Schemin.Primitives.BooleanOperations
{
	using Schemin.Evaluate;
	using Schemin.AST;

	public class Equal : Primitive
	{
		public override IScheminType Execute(Environment env, Evaluator eval, ScheminPair args)
		{
			IScheminType first = args.Car;
			IScheminType second = args.ElementAt(1);

			return ScheminBool.GetValue(EqualsHelper(first, second));
		}

		private bool EqualsHelper(IScheminType first, IScheminType second)
		{
			if ((first as ScheminPair) != null && (second as ScheminPair) != null)
			{
				ScheminPair firstPair = (ScheminPair) first;
				ScheminPair secondPair = (ScheminPair) second;

				if (firstPair.Length != secondPair.Length)
					return false;

				for (int i = 0; i < firstPair.Length; i++)
				{
					if (!EqualsHelper(firstPair.ElementAt(i), secondPair.ElementAt(i)))
						return false;
				}

				return true;
			}
			else if ((first as ScheminVector) != null && (second as ScheminVector) != null)
			{
				ScheminVector firstVec = (ScheminVector) first;
				ScheminVector secondVec = (ScheminVector) second;

				if (firstVec.List.Count != secondVec.List.Count)
					return false;

				for (int i = 0; i < firstVec.List.Count; i++)
				{
					if (!EqualsHelper(firstVec.List[i], secondVec.List[i]))
						return false;
				}

				return true; 
			}

			return first.Equivalent(second);
		}
	}
}
