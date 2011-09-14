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
	using System.Collections.Generic;
	using Schemin.AST;
	using Schemin.Evaluate;
	using Environment = Schemin.Evaluate.Environment;

	public static class PrimitiveFactory
	{
		private static Dictionary<string, Primitive> Primitives;

		static PrimitiveFactory()
		{
			Primitives = new Dictionary<string, Primitive>();
			Primitives.Add("+", NumericOperations.Add);
			Primitives.Add("-", NumericOperations.Subtract);
			Primitives.Add("*", NumericOperations.Multiply);
			Primitives.Add("/", NumericOperations.Divide);
			Primitives.Add("mod", NumericOperations.Mod);

			Primitives.Add("car", ListOperations.Car);
			Primitives.Add("cons", ListOperations.Cons);
			Primitives.Add("cdr", ListOperations.Cdr);
			Primitives.Add("cadr", ListOperations.Cadr);
			Primitives.Add("cddr", ListOperations.Cddr);
			Primitives.Add("length", ListOperations.Length);
			Primitives.Add("list", ListOperations.List);
			Primitives.Add("append", ListOperations.Append);

			Primitives.Add("null?", BooleanOperations.Null);
			Primitives.Add("=", BooleanOperations.Equal);
			Primitives.Add("eq?", BooleanOperations.Equal);

			Primitives.Add(">", BooleanOperations.GreaterThan);
			Primitives.Add(">=", BooleanOperations.GreaterThanOr);
			Primitives.Add("<", BooleanOperations.LessThan);
			Primitives.Add("<=", BooleanOperations.LessThanOr);
			Primitives.Add("prime?", BooleanOperations.Prime);
			Primitives.Add("or", BooleanOperations.Or);
			Primitives.Add("and", BooleanOperations.And);

			Primitives.Add("boolean?", BooleanOperations.Boolean);
			Primitives.Add("symbol?", BooleanOperations.Symbol);
			Primitives.Add("procedure?", BooleanOperations.Procedure);
			Primitives.Add("pair?", BooleanOperations.Pair);
			Primitives.Add("number?", BooleanOperations.Number);
			Primitives.Add("string?", BooleanOperations.String);
			Primitives.Add("char?", BooleanOperations.Char);
			Primitives.Add("port?", BooleanOperations.Port);
			Primitives.Add("input-port?", BooleanOperations.InputPort);
			Primitives.Add("output-port?", BooleanOperations.OutputPort);
			Primitives.Add("eof-object?", BooleanOperations.EOFObject);

			Primitives.Add("dumpenv", GeneralOperations.DumpEnv);
			Primitives.Add("apply", GeneralOperations.Apply);
			Primitives.Add("lambda", GeneralOperations.Lambda);
			Primitives.Add("define", GeneralOperations.Define);
			Primitives.Add("quote", GeneralOperations.Quote);
			Primitives.Add("begin", GeneralOperations.Begin);
			Primitives.Add("if", GeneralOperations.If);
			Primitives.Add("cond", GeneralOperations.Cond);
			Primitives.Add("let", GeneralOperations.Let);
			Primitives.Add("letrec", GeneralOperations.LetRec);
			Primitives.Add("let*", GeneralOperations.LetStar);
			Primitives.Add("set!", GeneralOperations.SetBang);
			Primitives.Add("call/cc", GeneralOperations.CallCC);

			Primitives.Add("string-ref", StringOperations.StringRef);
			Primitives.Add("string-length", StringOperations.StringLength);

			Primitives.Add("current-input-port", PortOperations.CurrentInputPort);
			Primitives.Add("current-output-port", PortOperations.CurrentOutputPort);
			Primitives.Add("set-current-output-port!", PortOperations.SetCurrentOutputPort);
			Primitives.Add("set-current-input-port!", PortOperations.SetCurrentInputPort);
			Primitives.Add("open-input-file", PortOperations.OpenInputFile);
			Primitives.Add("open-output-file", PortOperations.OpenOutputFile);
			Primitives.Add("close-port", PortOperations.ClosePort);
			Primitives.Add("port-closed?", PortOperations.PortClosed);
			Primitives.Add("display", PortOperations.Display);
			Primitives.Add("newline", PortOperations.Newline);
			Primitives.Add("read", PortOperations.Read);
			Primitives.Add("read-char", PortOperations.ReadChar);
			Primitives.Add("read-line", PortOperations.ReadLine);
			Primitives.Add("write", PortOperations.Write);
			Primitives.Add("write-char", PortOperations.WriteChar);

			Primitives.Add("char=?", CharOperations.CharEquals);
			Primitives.Add("char<?", CharOperations.CharGreaterThan);
			Primitives.Add("char>?", CharOperations.CharLessThan);
			Primitives.Add("char>=?", CharOperations.CharLessThanOr);
			Primitives.Add("char<=?", CharOperations.CharGreaterThanOr);

			Primitives.Add("char-alphabetic?", CharOperations.CharAlphabetic);
			Primitives.Add("char-numeric?", CharOperations.CharNumeric);
			Primitives.Add("char-whitespace?", CharOperations.CharWhitespace);
			Primitives.Add("char-upper-case?", CharOperations.CharUpperCase);
			Primitives.Add("char-lower-case?", CharOperations.CharLowerCase);
			Primitives.Add("char->integer", CharOperations.CharInteger);
			Primitives.Add("integer->char", CharOperations.IntegerChar);
			Primitives.Add("char-upcase", CharOperations.CharUpcase);
			Primitives.Add("char-downcase", CharOperations.CharDowncase);
		}

		static Primitive Get(string name)
		{
			return Primitives[name];
		}
	}
}

