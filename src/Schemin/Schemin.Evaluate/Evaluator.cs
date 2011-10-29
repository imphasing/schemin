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
	using System.IO;
	using System.Linq;
	using Schemin.AST;
	using Schemin.Parse;
	using Schemin.Tokenize;
	using Schemin.Primitives;

	public class Evaluator
	{
		public class StackFrame
		{
			public IScheminType Evaluated;
			public IScheminType Unevaluated;
			public Environment CurrentEnv;

			public StackFrame(StackFrame original)
			{
				this.Evaluated = original.Evaluated;
				this.Unevaluated = original.Unevaluated;
				this.CurrentEnv = original.CurrentEnv;
			}

			public StackFrame()
			{
				this.Evaluated = new ScheminPair();
				this.Unevaluated = new ScheminPair();
			}
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

		public IScheminType Evaluate(ScheminPair ast)
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
			StackFrame start = new StackFrame();
			start.Unevaluated = ast;
			start.CurrentEnv = this.GlobalEnv;

			this.Stack.Clear();
			this.Stack.Push(start);

			StackStart:
			while (this.Stack.Count > 0)
			{
				StackFrame current = new StackFrame(this.Stack.Pop());
				Environment currentEnv = current.CurrentEnv;
				IScheminType unevaled = current.Unevaluated;

				if ((unevaled as ScheminAtom) != null)
				{
					if (this.Stack.Count < 1)
					{
						return EvalAtom(unevaled, currentEnv);
					}
					else
					{
						StackFrame previous = this.Stack.Pop();
						StackFrame combinedPrevious = new StackFrame(previous);
						combinedPrevious.Evaluated = ((ScheminPair) combinedPrevious.Evaluated).Append(EvalAtom(unevaled, currentEnv));
						this.Stack.Push(combinedPrevious);
						continue;
					}
				}
				else if (IsEmptyList(current.Unevaluated) && IsEmptyList(current.Evaluated))
				{
					if (this.Stack.Count < 1)
					{
						return unevaled;
					}
					else
					{
						StackFrame previous = this.Stack.Pop();
						StackFrame combinedPrevious = new StackFrame(previous);
						combinedPrevious.Evaluated = ((ScheminPair)combinedPrevious.Evaluated).Append(unevaled);
						this.Stack.Push(combinedPrevious);
						continue;
					}
				}
				else if ((unevaled as ScheminPair) != null)
				{
					ScheminPair unevaluated = (ScheminPair) unevaled;
					ScheminPair evaluatedList = (ScheminPair) current.Evaluated;

					IScheminType function;
					int currentArg = 0;
					if (!evaluatedList.Empty)
					{
						function = evaluatedList.Car;
						currentArg += evaluatedList.Length;
					}
					else
					{
						function = unevaluated.Car;
					}

					ScheminPrimitive currentPrimitive = null;
					ScheminRewriter currentRewriter = null;
					if ((function as ScheminPrimitive) != null)
					{
						currentPrimitive = (ScheminPrimitive)function;
					}
					else if ((function as ScheminAtom) != null)
					{
						IScheminType evaledFunction = EvalAtom(function, currentEnv);
						if ((evaledFunction as ScheminRewriter) != null)
							currentRewriter = (ScheminRewriter) evaledFunction;
					}

					ScheminPair fullArgs = (ScheminPair)current.Evaluated;
					foreach (IScheminType restArg in (ScheminPair) unevaluated)
					{
						fullArgs = fullArgs.Append(restArg);
					}

					ScheminPair evaluated = new ScheminPair();
					while (!unevaluated.Empty)
					{
						IScheminType type = unevaluated.Car;

						if (currentPrimitive != null || currentRewriter != null)
						{
							if (!EvaluateNextArg(currentPrimitive, currentArg, fullArgs.ListCdr()) || (currentRewriter != null && currentArg > 0))
							{
								evaluated = evaluated.Append(type);
								unevaluated = unevaluated.ListCdr();
								currentArg++;
								continue;
							}
						}

						if ((type as ScheminAtom) != null)
						{
							IScheminType atomResult = EvalAtom(type, currentEnv);
							evaluated = evaluated.Append(atomResult);
						}
						else if ((type as ScheminPair) != null)
						{
							ScheminPair tempList = (ScheminPair)type;

							if (tempList.Empty)
							{
								evaluated = evaluated.Append(type);
								unevaluated = unevaluated.ListCdr();
								currentArg++;
								continue;
							}

							StackFrame next = new StackFrame();
							next.Unevaluated = tempList;
							next.CurrentEnv = currentEnv;

							StackFrame newSublist = new StackFrame(current);
							ScheminPair doneArgs = (ScheminPair)newSublist.Evaluated;
							foreach (IScheminType evaled in evaluated)
							{
								doneArgs = doneArgs.Append(evaled);
							}
							newSublist.Evaluated = doneArgs;
							newSublist.Unevaluated = unevaluated.ListCdr();

							this.Stack.Push(newSublist);
							this.Stack.Push(next);

							goto StackStart;
						}
						else
						{
							evaluated = evaluated.Append(type);
						}

						unevaluated = unevaluated.ListCdr();
						currentArg++;
					}

					foreach (IScheminType type in evaluated)
					{
						evaluatedList = evaluatedList.Append(type);
					}
					current.Evaluated = evaluatedList;
					current.Unevaluated = unevaluated;


					IScheminType waiting = evaluatedList.Car;
					if ((waiting as ScheminAtom) != null)
					{
						waiting = EvalAtom(waiting, currentEnv);
					}

					if ((waiting as ScheminPrimitive) != null)
					{
						ScheminPrimitive prim = (ScheminPrimitive)waiting;
						this.Stack.Push(current);
						IScheminType result = EvaluatePrimitive((ScheminPrimitive)waiting, evaluatedList.ListCdr(), currentEnv);
						this.Stack.Pop();

						if (prim.Rewriter)
						{
							StackFrame rewritten = new StackFrame(current);
							rewritten.Unevaluated = result;
							rewritten.Evaluated = new ScheminPair();
							this.Stack.Push(rewritten);
							continue;
						}

						if (this.Stack.Count < 1)
						{
							return result;
						}
						else
						{
							StackFrame previous = this.Stack.Pop();
							StackFrame combinedPrevious = new StackFrame(previous);
							ScheminPair previousDone = (ScheminPair)combinedPrevious.Evaluated;
							previousDone = previousDone.Append(result);
							combinedPrevious.Evaluated = previousDone;
							this.Stack.Push(combinedPrevious);
							continue;
						}
					}
					else if ((waiting as ScheminLambda) != null)
					{
						ScheminLambda lam = (ScheminLambda)waiting;
						StackFrame next = new StackFrame();
						next.Unevaluated = lam.Definition;
						next.CurrentEnv = lam.MakeEnvironment(evaluatedList.ListCdr(), this);

						this.Stack.Push(next);
						continue;
					}
					else if ((waiting as ScheminContinuation) != null)
					{
						ScheminContinuation con = (ScheminContinuation) waiting;
						this.Stack = new Stack<StackFrame>(con.PreviousStack);
						StackFrame continuationStart = new StackFrame(this.Stack.Pop());

						if (this.Stack.Count < 1)
						{
							return evaluatedList.ListCdr().Car;
						}
						else
						{
							StackFrame previous = this.Stack.Pop();
							StackFrame combinedPrevious = new StackFrame(previous);
							ScheminPair previousDone = (ScheminPair) combinedPrevious.Evaluated;
							previousDone = previousDone.Append(evaluatedList.ListCdr().Car);
							combinedPrevious.Evaluated = previousDone;
							this.Stack.Push(combinedPrevious);
							continue;
						}
					}
					else if ((waiting as ScheminRewriter) != null)
					{
						ScheminRewriter rewriter = (ScheminRewriter) waiting;
						ScheminPair macroCall = rewriter.Rewrite(evaluatedList.ListCdr());

						StackFrame macroRewrite = new StackFrame(current);
						macroRewrite.Unevaluated = macroCall; 
						macroRewrite.Evaluated = new ScheminPair();
						this.Stack.Push(macroRewrite);
						continue;
					}
					else
					{
						throw new InvalidOperationException("Non-function in function position: " + waiting.ToString());
					}
				}
				else
				{
					if (this.Stack.Count < 1)
					{
						return unevaled;
					}
					else
					{
						StackFrame previous = this.Stack.Pop();
						StackFrame combinedPrevious = new StackFrame(previous);
						combinedPrevious.Evaluated = ((ScheminPair)combinedPrevious.Evaluated).Append(unevaled);
						this.Stack.Push(combinedPrevious);
						continue;
					}
				}
			}

			throw new Exception("Control escaped evaluator");
		}

		private IScheminType EvaluatePrimitive(ScheminPrimitive functionPosition, ScheminPair args, Environment env)
		{
			try
			{
				return functionPosition.Evaluate(args, env, this);
			}
			catch (BadArgumentsException ba)
			{
				Token sourceToken = functionPosition.SourceToken;
				string line = String.Empty;
				if (sourceToken != null)
				{
					line = " line: " + sourceToken.LineNumber.ToString() + " col: " + sourceToken.ColNumber.ToString();
				}
				throw new BadArgumentsException(functionPosition.ToString() + " " + ba.Message + line);
			}
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

		private bool EvaluateNextArg(ScheminPrimitive currentPrimitive, int currentArg, ScheminPair args)
		{
			if (currentPrimitive != null)
			{
				switch (currentPrimitive.Name)
				{
					case "define":
						if ((args.Car as ScheminPair) != null)
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

		private bool IsEmptyList(IScheminType type)
		{
			if ((type as ScheminPair) != null)
			{
				ScheminPair temp = (ScheminPair) type;
				if (temp.Empty == true)
				{
					return true;
				}
			}

			return false;
		}

		private void DefinePrimitives(Environment env)
		{
			Tokenizer t = new Tokenizer();
			PairParser p = new PairParser();

			foreach (KeyValuePair<string, Primitive> kvp in PrimitiveFactory.Primitives)
			{
				ScheminAtom symbol = AtomFactory.GetAtom(kvp.Key);
				ScheminPrimitive prim = new ScheminPrimitive(kvp.Key);

				env.AddBinding(symbol, prim);
			}

			string load = "(load \"ScheminLib\\\\ScheminLib.ss\")";
			var tokens = t.Tokenize(load);
			var ast = p.Parse(tokens, true);
			Evaluate(ast);
		}
	}
}
