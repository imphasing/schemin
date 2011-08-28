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

namespace Schemin.Evaluate.Primitives
{
	using System;
	using System.Text;
	using System.Collections.Generic;
	using System.Numerics;
	using System.Linq;
	using Schemin.AST;
	using Schemin.Evaluate;
	using Environment = Schemin.Evaluate.Environment;

	public static class GeneralOperations
	{
		public static Func<ScheminList, Environment, Evaluator, IScheminType> If;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Cond;

		public static Func<ScheminList, Environment, Evaluator, IScheminType> Begin;

		public static Func<ScheminList, Environment, Evaluator, IScheminType> Display;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Newline;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> DumpEnv;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Quote;

		public static Func<ScheminList, Environment, Evaluator, IScheminType> Lambda;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Let;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> LetRec;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> LetStar;

		public static Func<ScheminList, Environment, Evaluator, IScheminType> Define;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> SetBang;

		public static Func<ScheminList, Environment, Evaluator, IScheminType> CallCC;

		public static Func<ScheminList, Environment, Evaluator, IScheminType> Apply;

		static GeneralOperations()
		{
			Apply = (list, env, eval) => {
				IScheminType function = list.Car();
				ScheminList argList = list.Cdr();
				ScheminList toApply = (ScheminList) list.Cdr().Last();

				ScheminList args = new ScheminList();
				args.UnQuote();

				foreach (IScheminType type in toApply)
				{
					args.Append(type);
				}

				foreach (IScheminType type in argList)
				{
					if (type != toApply)
						args.Cons(type);
				}

				args.Cons(function);

				return args;
			};

			Lambda = (list, env, eval) => {
				foreach (IScheminType type in list)
				{
					type.UnQuote();
				}

				ScheminLambda lam = new ScheminLambda(list, env);
				return lam;
			};

			Newline = (list, env, eval) => {
				Console.WriteLine();
				return ScheminList.EmptyList;
			};

			Display = (list, env, eval) => {
				IScheminType toDisplay = list.Car();

				byte[] toWrite;
				ASCIIEncoding encoding = new ASCIIEncoding();
				if (toDisplay.GetType() == typeof(ScheminString))
				{
					ScheminString temp = (ScheminString) toDisplay;

					toWrite = encoding.GetBytes(temp.Value);
					eval.CurrentOutputPort.OutputStream.Write(toWrite, 0, toWrite.Length);
				}
				else
				{
					toWrite = encoding.GetBytes(toDisplay.ToString());
					eval.CurrentOutputPort.OutputStream.Write(toWrite, 0, toWrite.Length);
				}

				return ScheminList.EmptyList;
			};

			SetBang = (list, env, eval) => {
				ScheminAtom symbol = (ScheminAtom) list.Car();
				symbol.UnQuote();
				IScheminType definition = list.Cdr().Car();

				Environment parent = env;
				while (parent != null)
				{
					IScheminType value;
					parent.bindings.TryGetValue(symbol.Name, out value);

					if (value != null)
					{

						parent.RemoveBinding(symbol);
						parent.AddBinding(symbol, definition);
						return ScheminList.EmptyList;
					}

					parent = parent.parent;
				}

				throw new UnboundAtomException(string.Format("Unbound atom: {0}", symbol));
			};

			Begin = (list, env, eval) => {
				return list.Last();
			};

			LetRec = (list, env, eval) =>
			{
				foreach (IScheminType type in list)
				{
					type.UnQuote();
				}

				ScheminList bindings = (ScheminList)list.Car();
				IScheminType expression = list.Cdr().Car();

				ScheminList args = new ScheminList();
				ScheminList argExps = new ScheminList();

				args.UnQuote();
				argExps.UnQuote();

				foreach (ScheminList bindingPair in bindings)
				{
					args.Append(bindingPair.Car());
					argExps.Append(bindingPair.Cdr().Car());
				}

				ScheminList body = new ScheminList();
				body.UnQuote();

				ScheminList next = args;
				ScheminList nextExp = argExps;
				while (next != null)
				{
					IScheminType symbol = next.Head;
					IScheminType exp = nextExp.Head;

					ScheminList setExp = new ScheminList(new ScheminPrimitive(GeneralOperations.SetBang, "set!"));
					setExp.UnQuote();
					setExp.Append(symbol);
					setExp.Append(exp);
					body.Append(setExp);

					next = next.Rest;
					nextExp = nextExp.Rest;
				}

				body.Append(expression);

				ScheminList lambdaDef = new ScheminList(args);
				lambdaDef.UnQuote();

				foreach (IScheminType type in body)
				{
					lambdaDef.Append(type);
				}

				Environment closure = env;
				ScheminLambda lam = new ScheminLambda(lambdaDef, closure);

				ScheminList toEvaluate = new ScheminList(lam);
				toEvaluate.UnQuote();

				foreach (IScheminType arg in argExps)
				{
					toEvaluate.Append(ScheminList.EmptyList);
				}

				return toEvaluate;
			};

			LetStar = (list, env, eval) => {
				foreach (IScheminType type in list)
				{
					type.UnQuote();
				}

				ScheminList bindings = (ScheminList) list.Car();
				IScheminType expression = list.Cdr().Car();

				ScheminList first = new ScheminList();
				first.UnQuote();
				ScheminList firstBinding = new ScheminList(bindings.Car());
				first.UnQuote();

				first.Append(new ScheminPrimitive(GeneralOperations.Let, "let"));
				first.Append(firstBinding);

				if (bindings.Cdr().Length > 0)
				{
					ScheminList nextLet = new ScheminList(bindings.Cdr());
					nextLet.UnQuote();

					nextLet.Cons(new ScheminPrimitive(GeneralOperations.LetStar, "let*"));
					nextLet.Append(expression);

					first.Append(nextLet);
				}
				else
				{
					first.Append(expression);
				}

				return first;
			};


			Let = (list, env, eval) => {
				foreach (IScheminType type in list)
				{
					type.UnQuote();
				}

				bool isNamed = false;
				if (list.Car().GetType() == typeof(ScheminAtom))
				{
					isNamed = true;
				}

				ScheminList bindings;
				ScheminList expression;

				if (!isNamed)
				{
					expression = list.Cdr();
					bindings = (ScheminList)list.Car();
				}
				else
				{
					expression = list.Cdr().Cdr();
					bindings = (ScheminList)list.Cdr().Car();
				}

				ScheminList args = new ScheminList();
				ScheminList argExps = new ScheminList();

				args.UnQuote();
				argExps.UnQuote();

				foreach (ScheminList bindingPair in bindings)
				{
					args.Append(bindingPair.Car());
					argExps.Append(bindingPair.Cdr().Car());
				}

				ScheminList lambdaDef = new ScheminList(args);
				lambdaDef.UnQuote();

				foreach (IScheminType type in expression)
				{
					lambdaDef.Append(type);
				}

				Environment closure = env;
				if (isNamed)
				{
					closure = new Environment();
					closure.parent = env;
				}

				ScheminLambda lam = new ScheminLambda(lambdaDef, closure);

				if (isNamed)
				{
					ScheminAtom name = (ScheminAtom)list.Car();
					closure.AddBinding(name, lam);
				}

				ScheminList toEvaluate = new ScheminList(lam);
				toEvaluate.UnQuote();

				foreach (IScheminType arg in argExps)
				{
					toEvaluate.Append(arg);
				}

				return toEvaluate;
			};

			Cond = (list, env, eval) => {
				foreach (IScheminType type in list)
				{
					type.UnQuote();
				}

				ScheminList conditions = (ScheminList) list;

				ScheminList builtIf = new ScheminList();
				builtIf.UnQuote();

				ScheminList firstCondition = (ScheminList) conditions.Car();
				if ((firstCondition.Car() as ScheminAtom) != null)
				{
					ScheminAtom atom = (ScheminAtom) firstCondition.Car();
					if (atom.Name == "else")
					{
						ScheminList elseClause = firstCondition.Cdr();
						elseClause.Cons(new ScheminPrimitive(GeneralOperations.Begin, "begin"));

						return elseClause;
					}
				}

				builtIf.Append(new ScheminPrimitive(GeneralOperations.If, "if"));
				builtIf.Append(firstCondition.Car());

				ScheminList beginExpression = firstCondition.Cdr();
				beginExpression.Cons(new ScheminPrimitive(GeneralOperations.Begin, "begin"));

				builtIf.Append(beginExpression);

				if (conditions.Cdr().Length > 0)
				{
					ScheminList nextConditions = conditions.Cdr();
					nextConditions.Cons(new ScheminPrimitive(GeneralOperations.Cond, "cond"));

					builtIf.Append(nextConditions);
				}

				return builtIf;
			};

			If = (list, env, eval) => {
				eval = new Evaluator();
				ScheminBool condition = list.Car().BoolValue();
				IScheminType then = list.Cdr().Car();
				IScheminType otherwise = list.Cdr().Cdr().Car();

				if (condition.Value)
				{
					then.UnQuote();
					return then;
				}
				else
				{
					otherwise.UnQuote();
					return otherwise;
				}
			};

			Quote = (list, env, eval) => {
				IScheminType arg = list.Car();
				if ((arg as ScheminList) != null)
				{
					if (((ScheminList) arg).Empty)
					{
						return ScheminList.EmptyList;
					}
				}

				return arg;
			};

			CallCC = (list, env, eval) => {
				ScheminList applied = new ScheminList();
				applied.UnQuote();

				applied.Append(list.Car());
				applied.Append(new ScheminContinuation(eval.Stack));

				return applied;
			};

			DumpEnv = (args, env, eval) => {
				Console.WriteLine(env.ToString());
				return ScheminList.EmptyList;
			};

			Define = (args, env, eval) => {
				bool deffun = false;

				if ((args.Car() as ScheminList) != null)
				{
					deffun = true;
				}

				if (!deffun)
				{
					ScheminAtom symbol = (ScheminAtom) args.Car();
					IScheminType definition = args.Cdr().Car();

					if (env.bindings.ContainsKey(symbol.Name))
					{
						env.RemoveBinding(symbol);
						env.AddBinding(symbol, definition);
					}
					else
					{
						env.AddBinding(symbol, definition);
					}

					return ScheminList.EmptyList;
				}
				else
				{
					ScheminList arguments = (ScheminList) args.Car();
					ScheminList expression = args.Cdr();

					foreach (IScheminType type in expression)
					{
						type.UnQuote();
					}

					ScheminAtom name = (ScheminAtom) arguments.Car();
					ScheminList argSymbols = arguments.Cdr();

					ScheminList lamArgs = new ScheminList(argSymbols, expression);
					lamArgs.UnQuote();

					ScheminLambda lam = new ScheminLambda(lamArgs, env);

					if (env.bindings.ContainsKey(name.Name))
					{
						env.RemoveBinding(name);
						env.AddBinding(name, lam);
					}
					else
					{
						env.AddBinding(name, lam);
					}

					return ScheminList.EmptyList;
				}
			};
		}
	}
}

