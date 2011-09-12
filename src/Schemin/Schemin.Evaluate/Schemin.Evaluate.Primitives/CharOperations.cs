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

	public static class CharOperations
	{
		public static Func<ScheminList, Environment, Evaluator, IScheminType> CharEquals;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> CharLessThan;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> CharGreaterThan;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> CharLessThanOr;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> CharGreaterThanOr;

		public static Func<ScheminList, Environment, Evaluator, IScheminType> CharAlphabetic;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> CharNumeric;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> CharWhitespace;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> CharUpperCase;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> CharLowerCase;

		public static Func<ScheminList, Environment, Evaluator, IScheminType> CharInteger;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> IntegerChar;

		public static Func<ScheminList, Environment, Evaluator, IScheminType> CharUpcase;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> CharDowncase;

		static CharOperations()
		{
			CharEquals = (list, env, eval) => {
				ScheminChar first = (ScheminChar) list.Car();
				ScheminChar second = (ScheminChar) list.Cdr().Car();

				if (first.Equals(second))
				{
					return ScheminBool.True;
				}

				return ScheminBool.False;
			};

			CharLessThan = (list, env, eval) => {
				ScheminChar first = (ScheminChar) list.Car();
				ScheminChar second = (ScheminChar) list.Cdr().Car();

				int result = first.Value.CompareTo(second.Value);

				if (result > 0)
				{
					return ScheminBool.True;
				}

				return ScheminBool.False;
			};

			CharGreaterThan = (list, env, eval) => {
				ScheminChar first = (ScheminChar) list.Car();
				ScheminChar second = (ScheminChar) list.Cdr().Car();

				int result = first.Value.CompareTo(second.Value);

				if (result < 0)
				{
					return ScheminBool.True;
				}

				return ScheminBool.False;
			};

			CharLessThanOr = (list, env, eval) => {
				ScheminChar first = (ScheminChar) list.Car();
				ScheminChar second = (ScheminChar) list.Cdr().Car();

				int result = first.Value.CompareTo(second.Value);

				if (result >= 0)
				{
					return ScheminBool.True;
				}

				return ScheminBool.False;
			};

			CharGreaterThanOr = (list, env, eval) => {
				ScheminChar first = (ScheminChar) list.Car();
				ScheminChar second = (ScheminChar) list.Cdr().Car();

				int result = first.Value.CompareTo(second.Value);

				if (result <= 0)
				{
					return ScheminBool.True;
				}

				return ScheminBool.False;
			};

			CharAlphabetic = (list, env, eval) => {
				ScheminChar chr = (ScheminChar) list.Car();

				if (Char.IsLetter(chr.Value))
				{
					return ScheminBool.True;
				}

				return ScheminBool.False;
			};

			CharNumeric = (list, env, eval) => {
				ScheminChar chr = (ScheminChar) list.Car();

				if (Char.IsNumber(chr.Value))
				{
					return ScheminBool.True;
				}

				return ScheminBool.False;
			};

			CharWhitespace = (list, env, eval) => {
				ScheminChar chr = (ScheminChar) list.Car();

				if (Char.IsWhiteSpace(chr.Value))
				{
					return ScheminBool.True;
				}

				return ScheminBool.False;
			};

			CharUpperCase = (list, env, eval) => {
				ScheminChar chr = (ScheminChar) list.Car();

				if (Char.IsUpper(chr.Value))
				{
					return ScheminBool.True;
				}

				return ScheminBool.False;
			};

			CharLowerCase = (list, env, eval) => {
				ScheminChar chr = (ScheminChar) list.Car();

				if (Char.IsLower(chr.Value))
				{
					return ScheminBool.True;
				}

				return ScheminBool.False;
			};

			CharInteger = (list, env, eval) => {
				ScheminChar chr = (ScheminChar) list.Car();

				int result = Convert.ToInt32(chr.Value);
				return new ScheminInteger(result);
			};

			IntegerChar = (list, env, eval) => {
				ScheminInteger num = (ScheminInteger) list.Car();

				return new ScheminChar((char) num.Value);
			};

			CharUpcase = (list, env, eval) => {
				ScheminChar chr = (ScheminChar) list.Car();
				return new ScheminChar(Char.ToUpperInvariant(chr.Value));
			};

			CharDowncase = (list, env, eval) => {
				ScheminChar chr = (ScheminChar) list.Car();
				return new ScheminChar(Char.ToLowerInvariant(chr.Value));
			};
		}
	}
}
