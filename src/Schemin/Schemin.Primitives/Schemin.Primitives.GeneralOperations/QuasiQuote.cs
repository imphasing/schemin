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
	public class QuasiQuote : Primitive
	{
		public override IScheminType Execute(Environment env, Evaluator eval, ScheminList args)
		{
			IScheminType arg = args.Car();
			if ((arg as ScheminList) != null)
			{
				return RewriteRecursive((ScheminList) arg);
			}
			else
			{
				arg.Quote();
				return arg;
			}
		}

		private ScheminList RewriteRecursive(ScheminList rewrite)
		{
			ScheminList quoted = new ScheminList(new ScheminPrimitive("append"));
			quoted.UnQuote();

			foreach (IScheminType type in rewrite)
			{
				if ((type as ScheminList) != null)
				{
					ScheminList typeList = (ScheminList) type;
					if ((typeList.Car() as ScheminPrimitive) != null)
					{
						if (((ScheminPrimitive) typeList.Car()).Name == "unquote")
						{
							typeList.UnQuote();
							ScheminList wrapper = new ScheminList(new ScheminPrimitive("list"));
							wrapper.UnQuote();
							wrapper.Append(typeList.Cdr().Car());

							quoted.Append(wrapper);
							continue;
						}
						else if (((ScheminPrimitive)typeList.Car()).Name == "unquote-splicing")
						{
							typeList.UnQuote();
							quoted.Append(typeList.Cdr().Car());
							continue;
						}
						else
						{
							ScheminList rewritten = RewriteRecursive(typeList);
							ScheminList quotedRewrite = new ScheminList(new ScheminPrimitive("list"));
							quotedRewrite.UnQuote();

							quotedRewrite.Append(rewritten);
							quoted.Append(quotedRewrite);
							continue;
						}
					}
					else
					{
						ScheminList rewritten = RewriteRecursive(typeList);
						ScheminList quotedRewrite = new ScheminList(new ScheminPrimitive("list"));
						quotedRewrite.UnQuote();

						quotedRewrite.Append(rewritten);
						quoted.Append(quotedRewrite);
						continue;
					}
				}

				ScheminList quotedSub = new ScheminList(new ScheminPrimitive("quote"));
				ScheminList wrappedElem = new ScheminList(type);
				wrappedElem.UnQuote();
				quotedSub.UnQuote();
				quotedSub.Append(wrappedElem);
				quoted.Append(quotedSub);
			}

			return quoted;
		}
	}

}
