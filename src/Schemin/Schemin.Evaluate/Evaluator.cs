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
			public IScheminType Evaluated = new ScheminPair();
			public IScheminType Unevaluated = new ScheminPair();
			public Environment CurrentEnv;
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

			Stack.Clear();
			Stack.Push(start);

			StackStart:
			while (Stack.Count > 0)
			{
				StackFrame current = Stack.Pop();
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
						previous.Evaluated = ((ScheminPair) previous.Evaluated).Append(EvalAtom(unevaled, currentEnv));
						Stack.Push(previous);
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
					if ((function as ScheminPrimitive) != null)
					{
						if (function.Quoted() == false)
						{
							currentPrimitive = (ScheminPrimitive)function;
						}
					}

					ScheminPair evaluated = new ScheminPair(false);
					while (!unevaluated.Empty)
					{
						IScheminType type = unevaluated.Car;

						if (currentPrimitive != null)
						{
							ScheminPair fullArgs = (ScheminPair)current.Evaluated;
							foreach (IScheminType restArg in (ScheminPair) current.Unevaluated)
							{
								fullArgs = fullArgs.Append(restArg);
							}

							if (!EvaluateNextArg(currentPrimitive, currentArg, fullArgs.ListCdr()))
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

							ScheminPair doneArgs = (ScheminPair) current.Evaluated;
							foreach (IScheminType evaled in evaluated)
							{
								doneArgs = doneArgs.Append(evaled);
							}
							current.Evaluated = doneArgs;
							current.Unevaluated = unevaluated.ListCdr();

							this.Stack.Push(current);
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
							current.Unevaluated = result;
							current.Evaluated = new ScheminPair();
							this.Stack.Push(current);
							continue;
						}

						if (this.Stack.Count < 1)
						{
							return result;
						}
						else
						{
							StackFrame previous = this.Stack.Pop();
							ScheminPair previousDone = (ScheminPair) previous.Evaluated;
							previousDone = previousDone.Append(result);
							previous.Evaluated = previousDone;
							Stack.Push(previous);
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
						this.Stack.Peek().Evaluated = new ScheminPair();
						this.Stack.Peek().Unevaluated = evaluatedList.ListCdr().Car;
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
						previous.Evaluated = ((ScheminPair)previous.Evaluated).Append(unevaled);
						Stack.Push(previous);
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

		private void DefinePrimitives(Environment env)
		{
			foreach (KeyValuePair<string, Primitive> kvp in PrimitiveFactory.Primitives)
			{
				ScheminAtom symbol = new ScheminAtom(kvp.Key);
				ScheminPrimitive prim = new ScheminPrimitive(kvp.Key);

				env.AddBinding(symbol, prim);
			}

			var prebound_schemin = new List<string>();
			prebound_schemin.Add(ScheminPrimitives.Cadr);
			prebound_schemin.Add(ScheminPrimitives.Cddr);
			prebound_schemin.Add(ScheminPrimitives.Caddr);
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
			prebound_schemin.Add(ScheminPrimitives.DefineMacro);

			Tokenize.Tokenizer t = new Tokenize.Tokenizer();
			Schemin.Parse.PairParser p = new Parse.PairParser();

			foreach (string primitive in prebound_schemin)
			{
				var tokens = t.Tokenize(primitive);
				var ast = p.Parse(tokens, false);
				Evaluate(ast);
			}
		}
	}
}
