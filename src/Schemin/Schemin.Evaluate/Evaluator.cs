
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
		private Type integer = typeof(ScheminInteger);
		private Type atom = typeof(ScheminAtom);
		private Type list = typeof(ScheminList);
		private Type str = typeof(ScheminString);
		private Type primitive = typeof(ScheminPrimitive);
		private Type lambda = typeof(ScheminLambda);
		private Type boolean = typeof(ScheminBool);

        public Stack<StackFrame> Stack = new Stack<StackFrame>();

        public Environment GlobalEnv;

        public class StackFrame
        {
            public ScheminList Before;
            public ScheminList After;
            public Environment CurrentEnv;

            public IScheminType WaitingOn;
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

            Stack.Push(start);

        StackStart:
            while (Stack.Count > 0)
            {
                StackFrame current = Stack.Pop();
                Environment CurrentEnv = current.CurrentEnv;

                ScheminList before = current.Before;
                ScheminList after = current.After;
                IScheminType WaitingOn = current.WaitingOn;

                if ((WaitingOn as ScheminList) == null || IsEmptyList(WaitingOn) || WaitingOn.Quoted() == true)
                {
                    StackFrame next = new StackFrame();

                    if (before == null && after == null)
                    {
                        if (Stack.Count < 1)
                        {
                            return WaitingOn;
                        }

                        if ((WaitingOn as ScheminAtom) != null)
                        {
                            WaitingOn = EvalAtom(WaitingOn, CurrentEnv);
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

                        next.CurrentEnv = previous.CurrentEnv;

                        Stack.Push(next);
                        continue;
                    }

                    next.WaitingOn = CombineStackFrame(before, after, WaitingOn);
                    next.CurrentEnv = CurrentEnv;
                    Stack.Push(next);
                    continue;
                }

                ScheminList evalList = (ScheminList)WaitingOn;
                ScheminList complete = new ScheminList();
                complete.UnQuote();

                bool foundWaiting = false;

                ScheminList rest = evalList;
                ScheminList pendingBefore = new ScheminList();
                pendingBefore.UnQuote();

                ScheminList pendingAfter = new ScheminList();
                pendingAfter.UnQuote();

                while (!rest.Empty)
                {
                    IScheminType type = rest.Car();

                    if ((type as ScheminAtom) != null)
                    {
                        if (type.Quoted())
                        {
                            AppendToPartialStackFrame(pendingBefore, pendingAfter, type, foundWaiting);
                        }
                        else
                        {
                            IScheminType atomResult = EvalAtom(type, CurrentEnv);
                            AppendToPartialStackFrame(pendingBefore, pendingAfter, atomResult, foundWaiting);
                        }
                    }
                    else if ((type as ScheminPrimitive) != null)
                    {
                        ScheminPrimitive prim = (ScheminPrimitive)type;
                        SetStatePrimitive(prim, rest.Cdr());
                        AppendToPartialStackFrame(pendingBefore, pendingAfter, prim, foundWaiting);
                    }
                    else if ((type as ScheminList) != null)
                    {
                        ScheminList tempList = (ScheminList)type;

                        if (tempList.Quoted())
                        {
                            AppendToPartialStackFrame(pendingBefore, pendingAfter, type, foundWaiting);
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
                        AppendToPartialStackFrame(pendingBefore, pendingAfter, type, foundWaiting);
                    }

                    rest = rest.Cdr();
                }

                ScheminList completed = CombineStackFrame(pendingBefore, pendingAfter, null);

                IScheminType functionPosition = completed.Car();
                ScheminList functionArgs = completed.Cdr();

                StackFrame completeFrame = new StackFrame();

                if ((functionPosition as ScheminPrimitive) != null)
                {
                    ScheminPrimitive prim = (ScheminPrimitive) functionPosition;
                    completeFrame.Before = before;
                    completeFrame.After = after;
                    completeFrame.WaitingOn = prim.Evaluate(functionArgs, CurrentEnv, this);
                    completeFrame.CurrentEnv = CurrentEnv;

                    Stack.Push(completeFrame);
                    continue;
                }
                else if ((functionPosition as ScheminLambda) != null)
                {
                    ScheminLambda lam = (ScheminLambda)functionPosition;
                    completeFrame.Before = before;
                    completeFrame.After = after;

                    Environment args = lam.Evaluate(functionArgs, this);

                    completeFrame.WaitingOn = lam.Definition;
                    completeFrame.CurrentEnv = args;

                    Stack.Push(completeFrame);
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

            if (before != null && before.Length > 0)
            {
                foreach (IScheminType type in before)
                {
                    complete.Append(type);
                }
            }

            if (result != null)
            {
                complete.Append(result);
            }

            if (after != null && after.Length > 0)
            {
                foreach (IScheminType type in after)
                {
                    complete.Append(type);
                }
            }

            return complete;
        }

        public void AppendToPartialStackFrame(ScheminList before, ScheminList after, IScheminType toAppend, bool foundWaiting)
        {
            if (foundWaiting)
            {
                after.Append(toAppend);
            }
            else
            {
                before.Append(toAppend);
            }
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

		public void SetStatePrimitive(ScheminPrimitive prim, ScheminList args)
		{
			switch (prim.Name)
			{
				case "define":
                    if ((args.Car() as ScheminList) != null)
                    {
                        args.Car().Quote();
                        foreach (IScheminType type in args.Cdr())
                        {
                            type.Quote();
                        }
                    }
                    else
                    {
                        args.Car().Quote();
                    }
					break;
				case "lambda":
                    foreach (IScheminType type in args)
                    {
                        type.Quote();
                    }
					break;
				case "quote":
                    args.Car().Quote();
					break;
				case "let":
					break;
				case "letrec":
					break;
				case "let*":
					break;
				case "if":
                    args.Cdr().Car().Quote();
                    args.Cdr().Cdr().Car().Quote();
					break;
				case "cond":
					break;
				case "and":
					break;
				case "or":
					break;
				case "set!":
					break;
			}
		}

		public void DefinePrimitives(Environment env)
		{
            var prebound_schemin = new Dictionary<string, string>();
            prebound_schemin.Add("map", ListOperations.Map);
            prebound_schemin.Add("filter", ListOperations.Filter);
            prebound_schemin.Add("foldl", ListOperations.Foldl);
            prebound_schemin.Add("foldr", ListOperations.Foldr);

            Tokenize.Tokenizer t = new Tokenize.Tokenizer();
            Schemin.Parse.Parser p = new Parse.Parser();

            foreach (KeyValuePair<string, string> kvp in prebound_schemin)
            {
                var tokens = t.Tokenize(kvp.Value);
                var ast = p.Parse(tokens);
                Evaluate(ast, env);
            }

			var prebound = new Dictionary<string, Func<ScheminList, Environment, Evaluator, IScheminType>>();

			prebound.Add("+", NumericOperations.Add);
			prebound.Add("-", NumericOperations.Subtract);
			prebound.Add( "*", NumericOperations.Multiply);
			prebound.Add( "/", NumericOperations.Divide);

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
			prebound.Add("zero?", BooleanOperations.Zero);

			prebound.Add("not", BooleanOperations.Not);
			prebound.Add("boolean?", BooleanOperations.Boolean);
			prebound.Add("symbol?", BooleanOperations.Symbol);
			prebound.Add("procedure?", BooleanOperations.Procedure);
			prebound.Add("pair?", BooleanOperations.Pair);
			prebound.Add("number?", BooleanOperations.Number);
			prebound.Add("string?", BooleanOperations.String);

			prebound.Add("dumpenv", GeneralOperations.DumpEnv);
			prebound.Add("begin", GeneralOperations.Begin);
			prebound.Add("display", GeneralOperations.Display);
			prebound.Add("newline", GeneralOperations.Newline);

			foreach (KeyValuePair<string, Func<ScheminList, Environment, Evaluator, IScheminType>> kvp in prebound)
			{
				var func = kvp.Value;
				string symbolValue = kvp.Key;

				ScheminAtom symbol = new ScheminAtom(symbolValue);
				ScheminPrimitive prim = new ScheminPrimitive(func, symbolValue);

				env.AddBinding(symbol, prim);
			}
		}
	}
}
