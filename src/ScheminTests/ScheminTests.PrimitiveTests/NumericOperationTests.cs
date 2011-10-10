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
	using System.Numerics;
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

			ScheminPair decimal_args = new ScheminPair(test_decimal);
			decimal_args = decimal_args.Append(test_decimal);

			ScheminPair int_args = new ScheminPair(test_integer);
			int_args = int_args.Append(test_integer);

			ScheminPair mixed_args = new ScheminPair(test_integer);
			mixed_args = mixed_args.Append(test_decimal);

			ScheminDecimal decimal_result = (ScheminDecimal) prim.Execute(null, null, decimal_args);
			ScheminInteger int_result = (ScheminInteger) prim.Execute(null, null, int_args);
			ScheminDecimal mixed_result = (ScheminDecimal) prim.Execute(null, null, mixed_args);

			ScheminInteger expected = new ScheminInteger(4);

			Assert.AreEqual(3.0m, decimal_result.DecimalValue());
			Assert.AreEqual(expected.IntegerValue(), int_result.IntegerValue());
			Assert.AreEqual(3.5m, mixed_result.DecimalValue());
		}

		[Test]
		public void TestDivide()
		{
			var prim = PrimitiveFactory.Get("/");
			ScheminDecimal test_decimal = new ScheminDecimal(3.0m);
			ScheminInteger test_integer = new ScheminInteger(3);
			ScheminInteger test_divisor_int = new ScheminInteger(2);
			ScheminDecimal test_divisor_decimal = new ScheminDecimal(2);

			ScheminPair decimal_args = new ScheminPair(test_decimal);
			decimal_args = decimal_args.Append(test_divisor_decimal);

			ScheminPair int_args = new ScheminPair(test_integer);
			int_args = int_args.Append(test_divisor_int);

			ScheminPair mixed_args = new ScheminPair(test_integer);
			mixed_args = mixed_args.Append(test_divisor_decimal);

			ScheminDecimal decimal_result = (ScheminDecimal) prim.Execute(null, null, decimal_args);
			ScheminInteger int_result = (ScheminInteger) prim.Execute(null, null, int_args);
			ScheminDecimal mixed_result = (ScheminDecimal) prim.Execute(null, null, mixed_args);

			ScheminInteger expected = new ScheminInteger(1);

			Assert.AreEqual(1.5m, decimal_result.DecimalValue());
			Assert.AreEqual(expected.IntegerValue(), int_result.IntegerValue());
			Assert.AreEqual(1.5m, mixed_result.DecimalValue());
		}

		[Test]
		public void TestMod()
		{
			var prim = PrimitiveFactory.Get("mod");
			ScheminDecimal test_decimal = new ScheminDecimal(1.5m);
			ScheminInteger test_integer = new ScheminInteger(2);
			ScheminInteger test_mod_int = new ScheminInteger(1);
			ScheminDecimal test_mod_decimal = new ScheminDecimal(0.8m);

			ScheminPair decimal_args = new ScheminPair(test_decimal);
			decimal_args = decimal_args.Append(test_mod_decimal);

			ScheminPair int_args = new ScheminPair(test_integer);
			int_args = int_args.Append(test_mod_int);

			ScheminPair mixed_args = new ScheminPair(test_integer);
			mixed_args = mixed_args.Append(test_mod_decimal);

			ScheminDecimal decimal_result = (ScheminDecimal) prim.Execute(null, null, decimal_args);
			ScheminInteger int_result = (ScheminInteger) prim.Execute(null, null, int_args);
			ScheminDecimal mixed_result = (ScheminDecimal) prim.Execute(null, null, mixed_args);

			ScheminInteger expected = new ScheminInteger(0);

			Assert.AreEqual(0.7m, decimal_result.DecimalValue());
			Assert.AreEqual(expected.IntegerValue(), int_result.IntegerValue());
			Assert.AreEqual(0.4m, mixed_result.DecimalValue());
		}

		[Test]
		public void TestMultiply()
		{
			var prim = PrimitiveFactory.Get("*");
			ScheminDecimal test_decimal = new ScheminDecimal(1.5m);
			ScheminInteger test_integer = new ScheminInteger(2);
			ScheminInteger test_mult_int = new ScheminInteger(6);
			ScheminDecimal test_mult_decimal = new ScheminDecimal(0.8m);
			ScheminInteger test_large = new ScheminInteger(BigInteger.Parse("100000000000000000000000000000000"));
			ScheminInteger test_large_2nd = new ScheminInteger(BigInteger.Parse("400000000000000000000"));


			ScheminPair decimal_args = new ScheminPair(test_decimal);
			decimal_args = decimal_args.Append(test_mult_decimal);

			ScheminPair int_args = new ScheminPair(test_integer);
			int_args = int_args.Append(test_mult_int);

			ScheminPair mixed_args = new ScheminPair(test_integer);
			mixed_args = mixed_args.Append(test_mult_decimal);

			ScheminPair large_args = new ScheminPair(test_large);
			large_args = large_args.Append(test_large_2nd);

			ScheminDecimal decimal_result = (ScheminDecimal) prim.Execute(null, null, decimal_args);
			ScheminInteger int_result = (ScheminInteger) prim.Execute(null, null, int_args);
			ScheminDecimal mixed_result = (ScheminDecimal) prim.Execute(null, null, mixed_args);
			ScheminInteger large_result = (ScheminInteger) prim.Execute(null, null, large_args);

			ScheminInteger expected = new ScheminInteger(12);
			ScheminInteger exp_large = new ScheminInteger(BigInteger.Parse("40000000000000000000000000000000000000000000000000000"));

			Assert.AreEqual(1.2m, decimal_result.DecimalValue());
			Assert.AreEqual(expected.IntegerValue(), int_result.IntegerValue());
			Assert.AreEqual(1.6m, mixed_result.DecimalValue());
			Assert.AreEqual(exp_large.IntegerValue(), large_result.IntegerValue());
		}

		[Test]
		public void TestSubtract()
		{
			var prim = PrimitiveFactory.Get("-");
			ScheminDecimal test_decimal = new ScheminDecimal(8.5m);
			ScheminInteger test_integer = new ScheminInteger(20);
			ScheminDecimal test_decimal_2 = new ScheminDecimal(4.1m);
			ScheminInteger test_integer_2 = new ScheminInteger(6);

			ScheminPair decimal_args = new ScheminPair(test_decimal);
			decimal_args = decimal_args.Append(test_decimal_2);

			ScheminPair int_args = new ScheminPair(test_integer);
			int_args = int_args.Append(test_integer_2);

			ScheminPair mixed_args = new ScheminPair(test_integer);
			mixed_args = mixed_args.Append(test_decimal);

			ScheminDecimal decimal_result = (ScheminDecimal) prim.Execute(null, null, decimal_args);
			ScheminInteger int_result = (ScheminInteger) prim.Execute(null, null, int_args);
			ScheminDecimal mixed_result = (ScheminDecimal) prim.Execute(null, null, mixed_args);

			ScheminInteger expected = new ScheminInteger(14);

			Assert.AreEqual(4.4m, decimal_result.DecimalValue());
			Assert.AreEqual(expected.IntegerValue(), int_result.IntegerValue());
			Assert.AreEqual(11.5m, mixed_result.DecimalValue());
		}
	}
}
