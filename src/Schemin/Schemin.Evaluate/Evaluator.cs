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
	using Schemin.Evaluate.Primitives;

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

		public Evaluator()
		{
			Stack = new Stack<StackFrame>();
			ConsoleIOPort = new ScheminPort(new CombinedStream(Console.OpenStandardInput(), Console.OpenStandardOutput()), ScheminPort.PortType.IOPort);
			CurrentInputPort = ConsoleIOPort;
			CurrentOutputPort = ConsoleIOPort;
		}

		public IScheminType Evaluate(ScheminList ast, Environment env)
		{
			this.GlobalEnv = env;
			IScheminType last = null;

			foreach (IScheminType type in ast)
			{
				last = EvaluateInternal(type);
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

				ScheminList rest = (ScheminList) WaitingOn;
				ScheminList pendingBefore = new ScheminList();
				pendingBefore.UnQuote();

				while (!rest.Empty)
				{
					IScheminType type = rest.Car();

					if ((type as ScheminAtom) != null)
					{
						if (type.Quoted())
						{
							pendingBefore.Append(type);
						}
						else
						{
							IScheminType atomResult = EvalAtom(type, CurrentEnv);
							pendingBefore.Append(atomResult);
						}
					}
					else if ((type as ScheminPrimitive) != null)
					{
						ScheminPrimitive prim = (ScheminPrimitive)type;
						QuoteAST(prim, rest.Cdr());
						pendingBefore.Append(prim);
					}
					else if ((type as ScheminList) != null)
					{
						ScheminList tempList = (ScheminList) type;

						if (tempList.Quoted() || tempList.Empty)
						{
							pendingBefore.Append(type);
							rest = rest.Cdr();
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
				}

				IScheminType functionPosition = pendingBefore.Car();
				ScheminList functionArgs = pendingBefore.Cdr();

				StackFrame completeFrame = new StackFrame();

				if ((functionPosition as ScheminPrimitive) != null)
				{
					ScheminPrimitive prim = (ScheminPrimitive) functionPosition;
					completeFrame.Before = before;
					completeFrame.After = after;

					// Need to pass push the previous frame back on so we can get access to the current continuation via the evaluator's Stack field.
					Stack.Push(current);
					completeFrame.WaitingOn = prim.Evaluate(functionArgs, CurrentEnv, this);
					Stack.Pop();

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
				else
				{
					throw new InvalidOperationException("Non-function in function position: " + functionPosition.ToString());
				}
			}

			throw new InvalidOperationException("Control escaped list evaluator...");
		}

		public IScheminType EvalAtom(IScheminType ast, Environment env)
		{
			ScheminAtom temp = (ScheminAtom) ast;

			IScheminType bound = GetEnvValue(temp, env);
			if (bound == null)
			{
				throw new UnboundAtomException(string.Format("Unbound atom: {0}", temp));
			}

			return bound;
		}

		public ScheminList CombineStackFrame(ScheminList before, ScheminList after, IScheminType result)
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

		public bool IsEmptyList(IScheminType type)
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

		public IScheminType GetEnvValue(ScheminAtom symbol, Environment env)
		{
			Environment parent = env;
			while (parent != null)
			{
				IScheminType value;
				parent.bindings.TryGetValue(symbol.Name, out value);

				if (value != null)
				{
					return parent.bindings[symbol.Name];
				}

				parent = parent.parent;
			}

			return null;
		}

		public void QuoteAST(ScheminPrimitive prim, ScheminList args)
		{
			switch (prim.Name)
			{
				case "define":
					if ((args.Car() as ScheminList) != null)
					{
						args.Car().Quote();
						QuoteAll(args.Cdr());
					}
					else
					{
						args.Car().Quote();
					}
					break;
				case "lambda":
					QuoteAll(args);
					break;
				case "quote":
					args.Car().Quote();
					break;
				case "let":
					QuoteAll(args);
					break;
				case "letrec":
					QuoteAll(args);
					break;
				case "let*":
					QuoteAll(args);
					break;
				case "begin":
					break;
				case "if":
					args.Cdr().Car().Quote();
					args.Cdr().Cdr().Car().Quote();
					break;
				case "cond":
					QuoteAll(args);
					break;
				case "and":
					QuoteAll(args);
					args.Car().UnQuote();
					break;
				case "or":
					QuoteAll(args);
					args.Car().UnQuote();
					break;
				case "set!":
					args.Car().Quote();
					break;
			}
		}

		private void QuoteAll(ScheminList list)
		{
			foreach (IScheminType type in list)
			{
				type.Quote();
			}
		}

		public void DefinePrimitives(Environment env)
		{
			// Add the console IO port as a default binding
			env.AddBinding(new ScheminAtom("console-i/o-port"), this.ConsoleIOPort);

			var prebound = new Dictionary<string, Func<ScheminList, Environment, Evaluator, IScheminType>>();

			prebound.Add("+", NumericOperations.Add);
			prebound.Add("-", NumericOperations.Subtract);
			prebound.Add("*", NumericOperations.Multiply);
			prebound.Add("/", NumericOperations.Divide);
			prebound.Add("mod", NumericOperations.Mod);

			prebound.Add("car", ListOperations.Car);
			prebound.Add("cons", ListOperations.Cons);
			prebound.Add("cdr", ListOperations.Cdr);
			prebound.Add("cadr", ListOperations.Cadr);
			prebound.Add("cddr", ListOperations.Cddr);
			prebound.Add("length", ListOperations.Length);
			prebound.Add("list", ListOperations.List);
			prebound.Add("append", ListOperations.Append);

			prebound.Add("null?", BooleanOperations.Null);
			prebound.Add("=", BooleanOperations.Equal);
			prebound.Add("eq?", BooleanOperations.Equal);

			prebound.Add(">", BooleanOperations.GreaterThan);
			prebound.Add(">=", BooleanOperations.GreaterThanOr);
			prebound.Add("<", BooleanOperations.LessThan);
			prebound.Add("<=", BooleanOperations.LessThanOr);
			prebound.Add("prime?", BooleanOperations.Prime);

			prebound.Add("boolean?", BooleanOperations.Boolean);
			prebound.Add("symbol?", BooleanOperations.Symbol);
			prebound.Add("procedure?", BooleanOperations.Procedure);
			prebound.Add("pair?", BooleanOperations.Pair);
			prebound.Add("number?", BooleanOperations.Number);
			prebound.Add("string?", BooleanOperations.String);
			prebound.Add("port?", BooleanOperations.Port);
			prebound.Add("input-port?", BooleanOperations.InputPort);
			prebound.Add("output-port?", BooleanOperations.OutputPort);

			prebound.Add("dumpenv", GeneralOperations.DumpEnv);
			prebound.Add("apply", GeneralOperations.Apply);

			prebound.Add("string-ref", StringOperations.StringRef);
			prebound.Add("string-length", StringOperations.StringLength);

			prebound.Add("current-input-port", PortOperations.CurrentInputPort);
			prebound.Add("current-output-port", PortOperations.CurrentOutputPort);
			prebound.Add("set-current-output-port!", PortOperations.SetCurrentOutputPort);
			prebound.Add("set-current-input-port!", PortOperations.SetCurrentInputPort);
			prebound.Add("open-input-file", PortOperations.OpenInputFile);
			prebound.Add("open-output-file", PortOperations.OpenOutputFile);
			prebound.Add("close-port", PortOperations.ClosePort);
			prebound.Add("port-closed?", PortOperations.PortClosed);
			prebound.Add("display", PortOperations.Display);
			prebound.Add("newline", PortOperations.Newline);
			prebound.Add("read", PortOperations.Read);
			prebound.Add("read-char", PortOperations.ReadChar);
			prebound.Add("read-line", PortOperations.ReadLine);
			prebound.Add("write", PortOperations.Write);
			prebound.Add("write-char", PortOperations.WriteChar);

			foreach (KeyValuePair<string, Func<ScheminList, Environment, Evaluator, IScheminType>> kvp in prebound)
			{
				var func = kvp.Value;
				string symbolValue = kvp.Key;

				ScheminAtom symbol = new ScheminAtom(symbolValue);
				ScheminPrimitive prim = new ScheminPrimitive(func, symbolValue);

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

			Tokenize.Tokenizer t = new Tokenize.Tokenizer();
			Schemin.Parse.Parser p = new Parse.Parser();

			foreach (string primitive in prebound_schemin)
			{
				var tokens = t.Tokenize(primitive);
				var ast = p.Parse(tokens, false);
				Evaluate(ast, env);
			}
		}
	}
}
