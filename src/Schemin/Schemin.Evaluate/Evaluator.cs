
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

		public EvaluatorState EvalState = EvaluatorState.Normal;

		public Stack<KeyValuePair<ScheminContinuation, ScheminList>> Continuations;

		public Evaluator()
		{
			Continuations = new Stack<KeyValuePair<ScheminContinuation, ScheminList>>();
		}

		public IScheminType Evaluate(ScheminList ast, Environment env)
		{
			IScheminType last = null;

			foreach (IScheminType type in ast)
			{
				last = EvaluateInternal(type, env);
			}

			return last;
		}

		public KeyValuePair<ScheminContinuation, ScheminList> ExtractFirstSublist(ScheminList evalList, Environment env)
		{
			bool encounteredList = false;
			ScheminList before = new ScheminList();
			ScheminList after = new ScheminList();
			ScheminList sublist = null;

			foreach (IScheminType type in evalList)
			{
                IScheminType working = type;

                if ((working as ScheminAtom) != null)
                {
                    working = EvalAtom((ScheminAtom) working, env);
                }

                if ((working as ScheminPrimitive) != null)
                {
                    SetStatePrimitive((ScheminPrimitive) working);
                }

				if ((working as ScheminList) != null)
				{
                    ScheminList tempList = (ScheminList) working;
                    if (((tempList.Car() as ScheminPrimitive != null) || (tempList.Car() as ScheminLambda != null)) && !encounteredList)
                    {
                        sublist = (ScheminList) working;
						encounteredList = true;
						continue;
                    }
				}

				if (encounteredList)
				{
					after.Append(working);
				}
				else
				{
					before.Append(working);
				}
			}

			ScheminContinuation current = new ScheminContinuation(before, after);
			return new KeyValuePair<ScheminContinuation, ScheminList>(current, sublist);
		}

        public IScheminType EvaluateSublist(ScheminList sublist, Environment env)
        {
            IScheminType functionPosition = sublist.Car();
            ScheminList functionArgs = sublist.Cdr();

            if ((functionPosition as ScheminAtom) != null)
            {
                functionPosition = EvalAtom((ScheminAtom)functionPosition, env);
            }

            if ((functionPosition as ScheminPrimitive) != null)
            {
                ScheminPrimitive prim = (ScheminPrimitive)functionPosition;
                return prim.Evaluate(functionArgs, env, this);
            }
            else if ((functionPosition as ScheminLambda) != null)
            {
                ScheminLambda lam = (ScheminLambda)functionPosition;
                return lam.Evaluate(functionArgs, this);
            }
            else
            {
                throw new InvalidOperationException("Non-function in function position: " + functionPosition.ToString());
            }
        }

		public IScheminType EvaluateInternal(IScheminType ast, Environment env)
		{
			if ((ast as IScheminNumeric) != null)
			{
				return ast;
			}
			else if ((ast as ScheminString) != null)
			{
				return ast;
			}
			else if ((ast as ScheminBool) != null)
			{
				return ast;
			}
			else if ((ast as ScheminAtom) != null)
			{
				return EvalAtom(ast, env);
			}
			else if ((ast as ScheminPrimitive) != null)
			{
				return ast;
			}
			else if ((ast as ScheminLambda) != null)
			{
				return ast;
			}
			else if ((ast as ScheminList) != null)
			{
				if (IsEmptyList(ast))
				{
					return ast;
				}

				var cont = ExtractFirstSublist((ScheminList) ast, env);
				this.Continuations.Push(cont);

				while (this.Continuations.Count > 0)
				{
					var current = this.Continuations.Pop();

					ScheminContinuation waiting = current.Key;
					ScheminList sublist = current.Value;

					IScheminType continuationValue = null;

                    if (sublist != null)
                    {
                        var nextCycle = ExtractFirstSublist(sublist, env);
                        this.Continuations.Push(current);
                        this.Continuations.Push(nextCycle);
                    }
                    else
                    {
                        continuationValue = EvaluateSublist(waiting.InvokeWith(null), env);

                        if (Continuations.Count < 1)
                        {
                            return continuationValue;
                        }

                        var previous = this.Continuations.Pop();

                        ScheminList previousList = previous.Key.InvokeWith(continuationValue);
                        var extracted = ExtractFirstSublist(previousList, env);

                        this.Continuations.Push(extracted);
                        continue;
                    }
				}
			}
		    else
		    {
			    // something weird happened
			    return ast;
		    }

            return new ScheminList();
	}

	public IScheminType EvalAtom(IScheminType ast, Environment env)
	{
		switch (this.EvalState)
		{
			case EvaluatorState.DefineArgs:
				// only ignore the FIRST symbol after a define
				this.EvalState = EvaluatorState.Normal;
				return ast;
			case EvaluatorState.LambdaArgs:
			case EvaluatorState.LetArgs:
			case EvaluatorState.IfArgs:
			case EvaluatorState.CondArgs:
			case EvaluatorState.QuoteArgs:
				return ast;
			case EvaluatorState.SetBangArgs:
				// only ignore the first argument to set!
				this.EvalState = EvaluatorState.Normal;
				return ast;
		}

		ScheminAtom temp = (ScheminAtom) ast;

		IScheminType bound = GetEnvValue(temp, env);
		if (bound == null)
		{
			throw new UnboundAtomException(string.Format("Unbound atom: {0}", temp));
		}


		return bound;
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

	public void SetStatePrimitive(ScheminPrimitive prim)
	{
		switch (prim.Name)
		{
			case "define":
				this.EvalState = EvaluatorState.DefineArgs;
				break;
			case "lambda":
				this.EvalState = EvaluatorState.LambdaArgs;
				break;
			case "quote":
				this.EvalState = EvaluatorState.QuoteArgs;
				break;
			case "let":
				this.EvalState = EvaluatorState.LetArgs;
				break;
			case "letrec":
				this.EvalState = EvaluatorState.LetArgs;
				break;
			case "let*":
				this.EvalState = EvaluatorState.LetArgs;
				break;
			case "if":
				this.EvalState = EvaluatorState.IfArgs;
				break;
			case "cond":
				this.EvalState = EvaluatorState.CondArgs;
				break;
			case "and":
				this.EvalState = EvaluatorState.AndArgs;
				break;
			case "or":
				this.EvalState = EvaluatorState.OrArgs;
				break;
			case "set!":
				this.EvalState = EvaluatorState.SetBangArgs;
				break;
		}
	}

	public void DefinePrimitives(Environment env)
	{
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
			prebound.Add("map", ListOperations.Map);
			prebound.Add("filter", ListOperations.Filter);
			prebound.Add("foldl", ListOperations.Foldl);

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
