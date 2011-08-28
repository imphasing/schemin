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
	using System.IO;
	using System.Text;
	using System.Collections.Generic;
	using System.Numerics;
	using System.Linq;
	using Schemin.AST;
	using Schemin.Evaluate;
	using Environment = Schemin.Evaluate.Environment;

	public static class PortOperations
	{
		public static Func<ScheminList, Environment, Evaluator, IScheminType> CurrentInputPort;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> CurrentOutputPort;

		public static Func<ScheminList, Environment, Evaluator, IScheminType> SetCurrentOutputPort;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> SetCurrentInputPort;

		public static Func<ScheminList, Environment, Evaluator, IScheminType> OpenInputFile;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> OpenOutputFile;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> ClosePort;

		static PortOperations()
		{
			ClosePort = (list, env, eval) => {
				ScheminPort toClose = (ScheminPort) list.Car();

				if (toClose.Type == ScheminPort.PortType.IOPort)
				{
					toClose.InputStream.Close();
					toClose.OutputStream.Close();
				}
				else if (toClose.Type == ScheminPort.PortType.OutputPort)
				{
					toClose.OutputStream.Close();
				}
				else if (toClose.Type == ScheminPort.PortType.InputPort)
				{
					toClose.InputStream.Close();
				}

				return ScheminList.EmptyList;
			};
			CurrentInputPort = (list, env, eval) => {
				return eval.CurrentInputPort;
			};

			CurrentOutputPort = (list, env, eval) => {
				return eval.CurrentOutputPort;
			};

			SetCurrentOutputPort = (list, env, eval) => {
				ScheminPort outputPort = (ScheminPort) list.Car();
				eval.CurrentOutputPort = outputPort;
				return ScheminList.EmptyList;
			};

			SetCurrentInputPort = (list, env, eval) => {
				ScheminPort inputPort = (ScheminPort) list.Car();
				eval.CurrentInputPort = inputPort;
				return ScheminList.EmptyList;
			};

			OpenInputFile = (list, env, eval) => {
				ScheminString filename = (ScheminString) list.Car();
				FileStream fs = File.OpenRead(filename.Value);
				ScheminPort filePort = new ScheminPort(fs, ScheminPort.PortType.InputPort);

				return filePort;
			};

			OpenOutputFile = (list, env, eval) => {
				ScheminString filename = (ScheminString) list.Car();
				FileStream fs = new FileStream(filename.Value, FileMode.Append, FileAccess.Write, FileShare.Write);
				ScheminPort filePort = new ScheminPort(fs, ScheminPort.PortType.OutputPort);

				return filePort;
			};
		}
	}
}
