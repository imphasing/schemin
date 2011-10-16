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
		public QuasiQuote()
		{
			base.Rewriter = true;
		}

		public override IScheminType Execute(Environment env, Evaluator eval, ScheminPair args)
		{
			IScheminType arg = args.Car;
			if ((arg as ScheminPair) != null)
			{
				return RewriteRecursive((ScheminPair) arg, 0);
			}
			else
			{
				return new ScheminPair(new ScheminPrimitive("quote")).Append(arg);
			}
		}

		private ScheminPair RewriteRecursive(ScheminPair rewrite, int currentLevel)
		{
			ScheminPair quoted = new ScheminPair(new ScheminPrimitive("append"));
			quoted.UnQuote();

			foreach (IScheminType type in rewrite)
			{
				if ((type as ScheminPair) != null)
				{
					ScheminPair typeList = (ScheminPair) type;
					ScheminPrimitive first = typeList.Car as ScheminPrimitive;

					if (first != null)
					{
						if (currentLevel == 0)
						{
							if (first.Name == "unquote")
							{
								typeList.UnQuote();
								ScheminPair wrapper = new ScheminPair(new ScheminPrimitive("list"));
								wrapper.UnQuote();
								wrapper = wrapper.Append(typeList.ElementAt(1));
								quoted = quoted.Append(wrapper);
								continue;
							}
							else if (first.Name == "unquote-splicing")
							{
								typeList.UnQuote();
								quoted = quoted.Append(typeList.ElementAt(1));
								continue;
							}
						}

						currentLevel = AdjustLevelEnter(first, currentLevel);
					}

					ScheminPair rewritten = RewriteRecursive(typeList, currentLevel);
					currentLevel = AdjustLevelLeave(first, currentLevel);

					ScheminPair quotedRewrite = new ScheminPair(new ScheminPrimitive("list"));
					quotedRewrite.UnQuote();

					quotedRewrite = quotedRewrite.Append(rewritten);
					quoted = quoted.Append(quotedRewrite);
				}
				else
				{
					ScheminPair quotedSub = new ScheminPair(new ScheminPrimitive("quote"));
					ScheminPair wrappedElem = new ScheminPair(type);
					wrappedElem.UnQuote();
					quotedSub.UnQuote();
					quotedSub = quotedSub.Append(wrappedElem);
					quoted = quoted.Append(quotedSub);
				}
			}

			return quoted;
		}

		private int AdjustLevelEnter(ScheminPrimitive first, int currentLevel)
		{
			if (first != null)
			{
				if (first.Name == "quasiquote")
				{
					currentLevel++;
				}
				else if (first.Name == "unquote")
				{
					currentLevel--;
				}
				else if (first.Name == "unquote-splicing")
				{
					currentLevel--;
				}
			}

			return currentLevel;
		}

		private int AdjustLevelLeave(ScheminPrimitive first, int currentLevel)
		{
			if (first != null)
			{
				if (first.Name == "quasiquote")
				{
					currentLevel--;
				}
				else if (first.Name == "unquote")
				{
					currentLevel++;
				}
				else if (first.Name == "unquote-splicing")
				{
					currentLevel++;
				}
			}

			return currentLevel;
		}

		public override void CheckArguments(ScheminPair args)
		{
			if (args.Length != 1)
			{
				throw new BadArgumentsException("expected 1 or 0 arguments");
			}

			return;
		}
	}
}
