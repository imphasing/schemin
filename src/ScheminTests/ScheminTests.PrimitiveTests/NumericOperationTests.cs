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

namespace ScheminTests.PrimitiveTests
{
	using System;
	using NUnit.Framework;
	using Schemin.Primitives;
	using Schemin.AST;

	[TestFixture]
	public class NumericOperationTests
	{
		[Test]
		public void TestAdd()
		{
			var prim = PrimitiveFactory.Get("+");
			ScheminDecimal test_decimal = new ScheminDecimal(1.5m);
			ScheminInteger test_integer = new ScheminInteger(2);

			ScheminList decimal_args = new ScheminList(test_decimal);
			decimal_args.Append(test_decimal);

			ScheminList int_args = new ScheminList(test_integer);
			int_args.Append(test_integer);

			ScheminList mixed_args = new ScheminList(test_integer);
			mixed_args.Append(test_decimal);

			ScheminDecimal decimal_result = (ScheminDecimal) prim.Execute(null, null, decimal_args);
			ScheminInteger int_result = (ScheminInteger) prim.Execute(null, null, int_args);
			ScheminDecimal mixed_result = (ScheminDecimal) prim.Execute(null, null, mixed_args);

			ScheminInteger expected = new ScheminInteger(4);

			Assert.AreEqual(decimal_result.DecimalValue(), 3.0m);
			Assert.AreEqual(int_result.IntegerValue(), expected.IntegerValue());
			Assert.AreEqual(mixed_result.DecimalValue(), 3.5m);
		}

		[Test]
		public void TestDivide()
		{
			var prim = PrimitiveFactory.Get("/");
			ScheminDecimal test_decimal = new ScheminDecimal(3.0m);
			ScheminInteger test_integer = new ScheminInteger(3);
			ScheminInteger test_divisor_int = new ScheminInteger(2);
			ScheminDecimal test_divisor_decimal = new ScheminDecimal(2);

			ScheminList decimal_args = new ScheminList(test_decimal);
			decimal_args.Append(test_divisor_decimal);

			ScheminList int_args = new ScheminList(test_integer);
			int_args.Append(test_divisor_int);

			ScheminList mixed_args = new ScheminList(test_integer);
			mixed_args.Append(test_divisor_decimal);

			ScheminDecimal decimal_result = (ScheminDecimal) prim.Execute(null, null, decimal_args);
			ScheminInteger int_result = (ScheminInteger) prim.Execute(null, null, int_args);
			ScheminDecimal mixed_result = (ScheminDecimal) prim.Execute(null, null, mixed_args);

			ScheminInteger expected = new ScheminInteger(1);

			Assert.AreEqual(decimal_result.DecimalValue(), 1.5m);
			Assert.AreEqual(int_result.IntegerValue(), expected.IntegerValue());
			Assert.AreEqual(mixed_result.DecimalValue(), 1.5m);
		}
	}
}
