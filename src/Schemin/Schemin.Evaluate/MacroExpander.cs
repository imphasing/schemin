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

namespace Schemin.Evaluate
{
	using Schemin.AST;
	using Schemin.Primitives;

	public class MacroExpander
	{
		private Evaluator eval;

		public MacroExpander(Evaluator eval)
		{
			this.eval = eval;
		}

		public IScheminType ExpandAll(IScheminType ast)
		{
			bool modified = false;
			IScheminType expanded = Expand(ast, ref modified);

			while (modified == true)
			{
				modified = false;
				expanded = Expand(expanded, ref modified);
			}

			return expanded;
		}

		private IScheminType Expand(IScheminType ast, ref bool modified)
		{
			if ((ast as ScheminPair) != null && ((ScheminPair) ast).Proper)
			{
				ScheminPair astPair = (ScheminPair) ast;
				if ((astPair.Car as ScheminAtom) != null)
				{
					ScheminAtom functionPosition = (ScheminAtom) astPair.Car;
					IScheminType bound = null;

					if (functionPosition.Closed)
						bound = functionPosition.Closure.GetValue(functionPosition);
					else
						bound = this.eval.GlobalEnv.GetValue(functionPosition);

					if ((bound as ScheminRewriter) != null)
					{
						ScheminRewriter rewriter = (ScheminRewriter)bound;
						ScheminPair macroCall = rewriter.Rewrite(astPair);
						IScheminType expanded = this.eval.EvaluateInternal(macroCall);
						modified = true;
						return expanded;
					}
					else if ((bound as ScheminPrimitive) != null)
					{
						ScheminPrimitive boundPrim = (ScheminPrimitive)bound;
						if (boundPrim.Definition == PrimitiveFactory.Get("define-rewriter"))
						{
							modified = true;
							return this.eval.EvaluateInternal(astPair);
						}
						else if (boundPrim.Name == "quote" || boundPrim.Name == "quasiquote")
						{
							return astPair;
						}
					}
				}

				ScheminPair rewritten = new ScheminPair();
				foreach (IScheminType type in astPair)
				{
					rewritten = rewritten.Append(Expand(type, ref modified));
				}

				return rewritten;
			}
			else
			{
				return ast;
			}
		}
	}
}
