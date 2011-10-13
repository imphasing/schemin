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

namespace Schemin.Primitives
{
	using System;
	using System.Collections.Generic;
	using Schemin.Evaluate;
	using Schemin.AST;
	using Environment = Schemin.Evaluate.Environment;

	public static class PrimitiveFactory
	{
		public static Dictionary<string, Primitive> Primitives;

		static PrimitiveFactory()
		{
			Primitives = new Dictionary<string, Primitive>();

			Primitives.Add("dumpenv", new GeneralOperations.DumpEnv());
			Primitives.Add("apply", new GeneralOperations.Apply());
			Primitives.Add("lambda", new GeneralOperations.Lambda());
			Primitives.Add("define", new GeneralOperations.Define());
			Primitives.Add("define-rewriter", new GeneralOperations.DefineRewriter());
			Primitives.Add("gensym", new GeneralOperations.GenSym());
			Primitives.Add("quote", new GeneralOperations.Quote());
			Primitives.Add("quasiquote", new GeneralOperations.QuasiQuote());
			Primitives.Add("unquote", new GeneralOperations.Unquote());
			Primitives.Add("unquote-splicing", new GeneralOperations.UnquoteSplicing());
			Primitives.Add("eval-macro", new GeneralOperations.EvalMacro());
			Primitives.Add("begin", new GeneralOperations.Begin());
			Primitives.Add("if", new GeneralOperations.If());
			Primitives.Add("cond", new GeneralOperations.Cond());
			Primitives.Add("let", new GeneralOperations.Let());
			Primitives.Add("letrec", new GeneralOperations.LetRec());
			Primitives.Add("let*", new GeneralOperations.LetStar());
			Primitives.Add("set!", new GeneralOperations.SetBang());
			Primitives.Add("call/cc", new GeneralOperations.CallCC());

			Primitives.Add("car", new ListOperations.Car());
			Primitives.Add("cons", new ListOperations.Cons());
			Primitives.Add("cdr", new ListOperations.Cdr());
			Primitives.Add("length", new ListOperations.Length());
			Primitives.Add("append", new ListOperations.Append());
			Primitives.Add("list", new ListOperations.List());

			Primitives.Add("+", new NumericOperations.Add());
			Primitives.Add("-", new NumericOperations.Subtract());
			Primitives.Add("*", new NumericOperations.Multiply());
			Primitives.Add("/", new NumericOperations.Divide());
			Primitives.Add("mod", new NumericOperations.Mod());

			Primitives.Add("null?", new BooleanOperations.Null());
			Primitives.Add("=", new BooleanOperations.Equal());
			Primitives.Add("eq?", new BooleanOperations.Equal());

			Primitives.Add(">", new BooleanOperations.GreaterThan());
			Primitives.Add(">=", new BooleanOperations.GreaterThanOr());
			Primitives.Add("<", new BooleanOperations.LessThan());
			Primitives.Add("<=", new BooleanOperations.LessThanOr());
			Primitives.Add("prime?", new BooleanOperations.Prime());
			Primitives.Add("or", new BooleanOperations.Or());
			Primitives.Add("and", new BooleanOperations.And());

			Primitives.Add("boolean?", new BooleanOperations.Boolean());
			Primitives.Add("symbol?", new BooleanOperations.Symbol());
			Primitives.Add("procedure?", new BooleanOperations.Procedure());
			Primitives.Add("pair?", new BooleanOperations.Pair());
			Primitives.Add("vector?", new BooleanOperations.Vector());
			Primitives.Add("number?", new BooleanOperations.Number());
			Primitives.Add("string?", new BooleanOperations.String());
			Primitives.Add("char?", new BooleanOperations.Char());
			Primitives.Add("port?", new BooleanOperations.Port());
			Primitives.Add("input-port?", new BooleanOperations.InputPort());
			Primitives.Add("output-port?", new BooleanOperations.OutputPort());
			Primitives.Add("eof-object?", new BooleanOperations.EOFObject());

			Primitives.Add("current-input-port", new PortOperations.CurrentInputPort());
			Primitives.Add("current-output-port", new PortOperations.CurrentOutputPort());
			Primitives.Add("set-current-output-port!", new PortOperations.SetCurrentOutputPort());
			Primitives.Add("set-current-input-port!", new PortOperations.SetCurrentInputPort());
			Primitives.Add("open-input-file", new PortOperations.OpenInputFile());
			Primitives.Add("open-output-file", new PortOperations.OpenOutputFile());
			Primitives.Add("close-port", new PortOperations.ClosePort());
			Primitives.Add("port-closed?", new PortOperations.PortClosed());
			Primitives.Add("display", new PortOperations.Display());
			Primitives.Add("newline", new PortOperations.Newline());
			Primitives.Add("read", new PortOperations.Read());
			Primitives.Add("read-char", new PortOperations.ReadChar());
			Primitives.Add("read-line", new PortOperations.ReadLine());
			Primitives.Add("write", new PortOperations.Write());
			Primitives.Add("write-char", new PortOperations.WriteChar());

			Primitives.Add("string-ref", new StringOperations.StringRef());
			Primitives.Add("string-length", new StringOperations.StringLength());

			Primitives.Add("char=?", new CharOperations.CharEquals());
			Primitives.Add("char<?", new CharOperations.CharGreaterThan());
			Primitives.Add("char>?", new CharOperations.CharLessThan());
			Primitives.Add("char>=?", new CharOperations.CharLessThanOr());
			Primitives.Add("char<=?", new CharOperations.CharGreaterThanOr());

			Primitives.Add("char-alphabetic?", new CharOperations.CharAlphabetic());
			Primitives.Add("char-numeric?", new CharOperations.CharNumeric());
			Primitives.Add("char-whitespace?", new CharOperations.CharWhitespace());
			Primitives.Add("char-upper-case?", new CharOperations.CharUpperCase());
			Primitives.Add("char-lower-case?", new CharOperations.CharLowerCase());
			Primitives.Add("char->integer", new CharOperations.CharInteger());
			Primitives.Add("integer->char", new CharOperations.IntegerChar());
			Primitives.Add("char-upcase", new CharOperations.CharUpcase());
			Primitives.Add("char-downcase", new CharOperations.CharDowncase());
		}

		public static Primitive Get(string name)
		{
			return Primitives[name];
		}
	}
}
