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
	using System.Linq;
	using Schemin.AST;
	using Schemin.Evaluate;
	using Environment = Schemin.Evaluate.Environment;

	public static class ListOperations
	{
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Car;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Cdr;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Cadr;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Cddr;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Cons;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Length;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> List;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Append;

		static ListOperations()
		{
			Length = (list, env, eval) => {
				ScheminList listArg = (ScheminList) list.Car();
				return new ScheminInteger(listArg.Length);
			};

			Cons = (list, env, eval) => {
				IScheminType head = list.Car();
				IScheminType rest = list.Cdr().Car();

				if (rest.GetType() == typeof(ScheminList))
				{
					ScheminList temp = (ScheminList) rest;
					return new ScheminList(head, temp);
				}

				var append = new ScheminList(head);
				append.Append(rest);
				return append; 
			};

			Car = (list, env, eval) => {
				ScheminList listArg = (ScheminList) list.Car();
				return listArg.Car();
			};

			Cdr = (list, env, eval) => {
				ScheminList listArg = (ScheminList) list.Car();
				return listArg.Cdr();
			};

			Cadr = (list, env, eval) => {
				ScheminList listArg = (ScheminList) list.Car();
				return listArg.Cdr().Car();
			};

			Cddr = (list, env, eval) => {
				ScheminList listArg = (ScheminList) list.Car();
				return listArg.Cdr().Cdr();
			};

			List = (list, env, eval) => {
				IScheminType args = list;
				ScheminList ret = new ScheminList();

				if (args.GetType() == typeof(ScheminList))
				{
					ScheminList temp = (ScheminList) args;
					foreach (IScheminType type in temp)
					{
						ret.Append(type);
					}
				}
				else
				{
					ret.Append(args);
				}

				return ret;
			};

			Append = (list, env, eval) => {
				ScheminList appended = new ScheminList();

				foreach (IScheminType type in list)
				{
					if (type.GetType() == typeof(ScheminList))
					{
						ScheminList temp = (ScheminList) type;

						if (temp.Empty)
						{
							continue;
						}

						foreach (IScheminType subType in temp)
						{
							appended.Append(subType);
						}
					}
					else
					{
						throw new InvalidOperationException(string.Format("Non-list argument given to append: {0}", type));
					}
				}

				return appended;
			};
		}
	}
}
