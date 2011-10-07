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
	using System;
	using System.Text;
	using System.Collections;
	using System.Collections.Generic;
	using System.Linq;
	using Schemin.AST;
	using Schemin.Tokenize;
	using Schemin.Primitives;

	public class Evaluator
	{
		public class StackFrame
		{
			public ScheminList Before;
			public ScheminList After;
			public Environment CurrentEnv;

			public IScheminType WaitingOn;
		}

		public Stack<StackFrame> Stack;
		public Environment GlobalEnv;
		public ScheminPort ConsoleIOPort;
		public ScheminPort CurrentInputPort;
		public ScheminPort CurrentOutputPort; 
		public int GenSymSeed = 0;
		public string GenSymPrefix = "GENERATED_";

		public Evaluator()
		{
			Stack = new Stack<StackFrame>();
			this.GlobalEnv = new Environment();

			var ConsoleInput = new ScheminPort(Console.In);
			var ConsoleOutput = new ScheminPort(Console.Out);

			CurrentInputPort = ConsoleInput;
			CurrentOutputPort = ConsoleOutput;
			DefinePrimitives(this.GlobalEnv);
		}

		public IScheminType Evaluate(ScheminList ast)
		{
			IScheminType last = null;

			try
			{
				foreach (IScheminType type in ast)
				{
					last = EvaluateInternal(type);
				}
			}
			catch (BadArgumentsException b)
			{
				CurrentOutputPort.OutputStream.WriteLine("bad arguments: " + b.Message);
			}
			catch (Exception e)
			{
				CurrentOutputPort.OutputStream.WriteLine("error: " + e.Message);
			}

			return last;
		}

		public IScheminType EvaluateInternal(IScheminType ast)
		{
			if ((ast as ScheminAtom) != null)
			{
				return EvalAtom(ast, this.GlobalEnv);
			}
			else if ((ast as ScheminList) != null)
			{
				return EvaluateList((ScheminList) ast);
			}
			else
			{
				return ast;
			}
		}

		public IScheminType EvaluateList(ScheminList list)
		{
			StackFrame start = new StackFrame();
			start.WaitingOn = list;
			start.CurrentEnv = this.GlobalEnv;

			Stack.Clear();
			Stack.Push(start);

			StackStart:
			while (Stack.Count > 0)
			{
				StackFrame current = Stack.Pop();
				Environment CurrentEnv = current.CurrentEnv;

				ScheminList before = current.Before;
				ScheminList after = current.After;
				IScheminType WaitingOn = current.WaitingOn;

				if ((WaitingOn as ScheminList) == null || WaitingOn.Quoted() == true || IsEmptyList(WaitingOn))
				{
					StackFrame next = new StackFrame();

					if (before == null && after == null)
					{
						if ((WaitingOn as ScheminAtom) != null && !WaitingOn.Quoted())
						{
							WaitingOn = EvalAtom(WaitingOn, CurrentEnv);
						}

						if (Stack.Count < 1)
						{
							return WaitingOn;
						}

						StackFrame previous = Stack.Pop();
						if (previous.Before == null && previous.After == null)
						{
							next.WaitingOn = WaitingOn;
						}
						else
						{
							next.WaitingOn = CombineStackFrame(previous.Before, previous.After, WaitingOn);
						}

						// Use the previous environment in this case as well
						if (Stack.Count > 0)
						{
							next.CurrentEnv = Stack.Peek().CurrentEnv;
						}
						else
						{
							next.CurrentEnv = previous.CurrentEnv;
						}

						Stack.Push(next);
						continue;
					}

					// We need to use the PREVIOUS environment here, so peek it.. otherwise we're re-using the same environment for the previous context.
					StackFrame peeked = Stack.Peek();
					next.WaitingOn = CombineStackFrame(before, after, WaitingOn);
					next.CurrentEnv = peeked.CurrentEnv;
					Stack.Push(next);
					continue;
				}

				StackFrame completeFrame = new StackFrame();
				ScheminPrimitive currentPrimitive = null;

				ScheminList rest = (ScheminList) WaitingOn;
				ScheminList pendingBefore = new ScheminList();
				pendingBefore.UnQuote();

				if ((rest.Car() as ScheminPrimitive) != null)
				{
					if (rest.Car().Quoted() == false)
					{
						currentPrimitive = (ScheminPrimitive) rest.Car();
					}
				}

				int currentArg = 0;

				while (!rest.Empty)
				{
					IScheminType type = rest.Car();

					if (currentPrimitive != null)
					{
						if (!EvaluateNextArg(currentPrimitive, currentArg, ((ScheminList) WaitingOn).Cdr()))
						{
							pendingBefore.Append(type);
							rest = rest.Cdr();
							currentArg++;
							continue;
						}
					}

					if ((type as ScheminAtom) != null)
					{
						if (type.Quoted())
						{
							pendingBefore.Append(type);
						}
						else
						{
							IScheminType atomResult = EvalAtom(type, CurrentEnv);
							if ((atomResult as ScheminRewriter) != null)
							{
								// if we get a quoted rewriter here, we're going to apply it :(
								pendingBefore.Append(atomResult);
								QuoteAll(rest.Cdr());
							}
							else
							{
								pendingBefore.Append(atomResult);
							}
						}
					}
					else if ((type as ScheminList) != null)
					{
						ScheminList tempList = (ScheminList) type;

						if (tempList.Quoted() || tempList.Empty)
						{
							pendingBefore.Append(type);
							rest = rest.Cdr();
							currentArg++;
							continue;
						}

						StackFrame next = new StackFrame();
						next.WaitingOn = type;
						next.After = rest.Cdr();
						next.Before = pendingBefore;
						next.CurrentEnv = CurrentEnv;

						Stack.Push(current);
						Stack.Push(next);

						goto StackStart;
					}
					else
					{
						pendingBefore.Append(type);
					}

					rest = rest.Cdr();
					currentArg++;
				}

				IScheminType functionPosition = pendingBefore.Car();
				ScheminList functionArgs = pendingBefore.Cdr();


				if ((functionPosition as ScheminPrimitive) != null)
				{
					ScheminPrimitive prim = (ScheminPrimitive) functionPosition;
					completeFrame.Before = before;
					completeFrame.After = after;

					// Need to pass push the previous frame back on so we can get access to the current continuation via the evaluator's Stack field.
					// also adding the primitive's name to the exception if it gets thrown.
					try
					{
						Stack.Push(current);
						completeFrame.WaitingOn = prim.Evaluate(functionArgs, CurrentEnv, this);
						Stack.Pop();
					}
					catch (BadArgumentsException ba)
					{
						Token sourceToken = prim.SourceToken;
						string line = String.Empty;
						if (sourceToken != null)
						{
							line = " line: " + sourceToken.LineNumber.ToString() + " col: " + sourceToken.ColNumber.ToString();
						}
						throw new BadArgumentsException(prim.ToString() + " " + ba.Message + line);
					}

					completeFrame.CurrentEnv = CurrentEnv;

					Stack.Push(completeFrame);
					continue;
				}
				else if ((functionPosition as ScheminLambda) != null)
				{
					ScheminLambda lam = (ScheminLambda) functionPosition;
					completeFrame.Before = before;
					completeFrame.After = after;

					Environment args = lam.MakeEnvironment(functionArgs, this);
					completeFrame.WaitingOn = lam.Definition;
					completeFrame.CurrentEnv = args;

					Stack.Push(completeFrame);
					continue;
				}
				else if ((functionPosition as ScheminContinuation) != null)
				{
					ScheminContinuation con = (ScheminContinuation) functionPosition;
					this.Stack = new Stack<StackFrame>(con.PreviousStack);
					this.Stack.Peek().WaitingOn = functionArgs.Car();
					continue;
				}
				else if ((functionPosition as ScheminRewriter) != null)
				{
					ScheminRewriter rewriter = (ScheminRewriter) functionPosition;

					QuoteAll(functionArgs);
					IScheminType result = rewriter.Rewrite(functionArgs);

					completeFrame.Before = before;
					completeFrame.After = after;
					completeFrame.WaitingOn = result;
					completeFrame.CurrentEnv = CurrentEnv;

					this.Stack.Push(completeFrame);
					continue;
				}
				else
				{
					throw new InvalidOperationException("Non-function in function position: " + functionPosition.ToString());
				}
			}

			throw new InvalidOperationException("Control escaped list evaluator...");
		}

		private IScheminType EvalAtom(IScheminType ast, Environment env)
		{
			ScheminAtom temp = (ScheminAtom) ast;

			IScheminType bound = env.GetValue(temp);
			if (bound == null)
			{
				throw new UnboundAtomException(string.Format("Unbound atom: {0}", temp));
			}

			return bound;
		}

		private ScheminList CombineStackFrame(ScheminList before, ScheminList after, IScheminType result)
		{
			ScheminList complete = new ScheminList();
			complete.UnQuote();

			if (before != null && !before.Empty)
			{
				complete.Append(before.Head);
				var restBefore = before.Rest;
				while (restBefore != null)
				{
					complete.Append(restBefore.Head);
					restBefore = restBefore.Rest;
				}
			}

			if (result != null)
			{
				complete.Append(result);
			}

			if (after != null && !after.Empty)
			{
				complete.Append(after.Head);
				var restAfter = after.Rest;
				while (restAfter != null)
				{
					complete.Append(restAfter.Head);
					restAfter = restAfter.Rest;
				}
			}

			return complete;
		}

		private bool IsEmptyList(IScheminType type)
		{
			if (type.GetType() == typeof(ScheminList))
			{
				ScheminList temp = (ScheminList) type;
				if (temp.Empty == true)
				{
					return true;
				}
			}

			return false;
		}

		private bool EvaluateNextArg(ScheminPrimitive currentPrimitive, int currentArg, ScheminList args)
		{
			if (currentPrimitive != null)
			{
				switch (currentPrimitive.Name)
				{
					case "define":
						if ((args.Car() as ScheminList) != null)
						{
							return false;
						}
						else
						{
							if (currentArg == 1)
								return false;
						}
						break;
					case "define-rewriter":
						if (currentArg == 1)
							return false;
						break;
					case "lambda":
						return false;
					case "quote":
						if (currentArg == 1)
							return false;
						break;
					case "quasiquote":
						if (currentArg == 1)
							return false;
						break;
					case "let":
						return false;
					case "letrec":
						return false;
					case "let*":
						return false;
					case "if":
						if (currentArg == 2)
							return false; 
						if (currentArg == 3)
							return false;
						break;
					case "cond":
						return false;
					case "and":
						if (currentArg != 1)
							return false;
						break;
					case "or":
						if (currentArg != 1)
							return false;
						break;
					case "set!":
						if (currentArg == 1)
							return false;
						break;
				}
			}

			return true;
		}

		private void QuoteAll(ScheminList list)
		{
			foreach (IScheminType type in list)
			{
				type.Quote();
			}
		}

		private void DefinePrimitives(Environment env)
		{
			foreach (KeyValuePair<string, Primitive> kvp in PrimitiveFactory.Primitives)
			{
				ScheminAtom symbol = new ScheminAtom(kvp.Key);
				ScheminPrimitive prim = new ScheminPrimitive(kvp.Key);

				env.AddBinding(symbol, prim);
			}

			var prebound_schemin = new List<string>();
			prebound_schemin.Add(ScheminPrimitives.Map);
			prebound_schemin.Add(ScheminPrimitives.Filter);
			prebound_schemin.Add(ScheminPrimitives.Foldl);
			prebound_schemin.Add(ScheminPrimitives.Foldr);
			prebound_schemin.Add(ScheminPrimitives.Not);
			prebound_schemin.Add(ScheminPrimitives.Id);
			prebound_schemin.Add(ScheminPrimitives.Flip);
			prebound_schemin.Add(ScheminPrimitives.Fold);
			prebound_schemin.Add(ScheminPrimitives.Unfold);
			prebound_schemin.Add(ScheminPrimitives.Reverse);
			prebound_schemin.Add(ScheminPrimitives.Curry);
			prebound_schemin.Add(ScheminPrimitives.Compose);
			prebound_schemin.Add(ScheminPrimitives.Zero);
			prebound_schemin.Add(ScheminPrimitives.Positive);
			prebound_schemin.Add(ScheminPrimitives.Negative);
			prebound_schemin.Add(ScheminPrimitives.Odd);
			prebound_schemin.Add(ScheminPrimitives.Even);
			prebound_schemin.Add(ScheminPrimitives.CallWithCC);
			prebound_schemin.Add(ScheminPrimitives.Error);
			prebound_schemin.Add(ScheminPrimitives.Sum);
			prebound_schemin.Add(ScheminPrimitives.Product);
			prebound_schemin.Add(ScheminPrimitives.Max);
			prebound_schemin.Add(ScheminPrimitives.Min);
			prebound_schemin.Add(ScheminPrimitives.Caddr);
			prebound_schemin.Add(ScheminPrimitives.DefineMacro);

			Tokenize.Tokenizer t = new Tokenize.Tokenizer();
			Schemin.Parse.Parser p = new Parse.Parser();

			foreach (string primitive in prebound_schemin)
			{
				var tokens = t.Tokenize(primitive);
				var ast = p.Parse(tokens, false);
				Evaluate(ast);
			}
		}
	}
}
