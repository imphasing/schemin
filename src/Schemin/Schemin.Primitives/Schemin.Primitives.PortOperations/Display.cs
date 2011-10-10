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

namespace Schemin.Primitives.PortOperations
{
	using Schemin.Evaluate;
	using Schemin.AST;

	public class Display : Primitive
	{
		public override IScheminType Execute(Environment env, Evaluator eval, ScheminPair args)
		{
			IScheminType toDisplay = args.Car;
			ScheminPort writeTo = eval.CurrentOutputPort;

			if (args.Length > 1)
			{
				writeTo = (ScheminPort) args.ElementAt(1);
			}

			if (toDisplay.GetType() == typeof(ScheminString))
			{
				ScheminString temp = (ScheminString) toDisplay;
				writeTo.OutputStream.Write(temp.Value);
				writeTo.OutputStream.Flush();
			}
			else
			{
				writeTo.OutputStream.Write(toDisplay.ToString());
				writeTo.OutputStream.Flush();
			}

			return new ScheminPair();
		}

		public override void CheckArguments(ScheminPair args)
		{
			if (args.Length < 1 || args.Length > 2)
			{
				throw new BadArgumentsException("expected 1 or 2 arguments");
			}

			if (args.Length == 2)
			{
				IScheminType port = args.ElementAt(1);
				if ((port as ScheminPort) == null)
				{
					throw new BadArgumentsException("second argument must be a port");
				}
			}

			return;
		}
	}
}
