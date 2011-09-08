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
	using Schemin.Tokenize;
	using Schemin.Parse;
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
		public static Func<ScheminList, Environment, Evaluator, IScheminType> PortClosed;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Newline;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Display;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Write;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> WriteChar;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> Read;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> ReadChar;
		public static Func<ScheminList, Environment, Evaluator, IScheminType> ReadLine;

		public static Func<ScheminList, Environment, Evaluator, IScheminType> PortPosition;

		static PortOperations()
		{
			ClosePort = (list, env, eval) => {
				ScheminPort toClose = (ScheminPort) list.Car();
				if (toClose.Type == ScheminPort.PortType.InputPort)
				{
					toClose.InputStream.Close();
				}
				else
				{
					toClose.OutputStream.Close();
				}
				toClose.Closed = true;
				return ScheminList.EmptyList;
			};

			PortClosed = (list, env, eval) => {
				ScheminPort toCheck = (ScheminPort) list.Car();
				if (toCheck.Closed)
				{
					return ScheminBool.True;
				}

				return ScheminBool.False;
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

			Newline = (list, env, eval) => {
				ScheminPort writeTo = eval.CurrentOutputPort;

				IScheminType port = list.Car();
				if ((port as ScheminPort) != null)
				{
					writeTo = (ScheminPort) port;
				}

				writeTo.OutputStream.Write(System.Environment.NewLine);
				return ScheminList.EmptyList;
			};

			Display = (list, env, eval) => {
				IScheminType toDisplay = list.Car();
				ScheminPort writeTo = eval.CurrentOutputPort;

				IScheminType port = list.Cdr().Car();
				if ((port as ScheminPort) != null)
				{
					writeTo = (ScheminPort) port;
				}

				if (toDisplay.GetType() == typeof(ScheminString))
				{
					ScheminString temp = (ScheminString) toDisplay;
					writeTo.OutputStream.Write(temp.Value);
				}
				else
				{
					writeTo.OutputStream.Write(toDisplay.ToString());
				}

				return ScheminList.EmptyList;
			};

			Write = (list, env, eval) => {
				IScheminType obj = list.Car();
				ScheminPort writeTo = eval.CurrentOutputPort;

				IScheminType port = list.Cdr().Car();
				if ((port as ScheminPort) != null)
				{
					writeTo = (ScheminPort) port;
				}

				writeTo.OutputStream.Write(obj.ToString());

				return ScheminList.EmptyList;
			};

			WriteChar = (list, env, eval) => {
				ScheminChar chr = (ScheminChar) list.Car();
				ScheminPort writeTo = eval.CurrentOutputPort;

				IScheminType port = list.Cdr().Car();
				if ((port as ScheminPort) != null)
				{
					writeTo = (ScheminPort) port;
				}

				writeTo.OutputStream.Write(chr.Value);

				return ScheminList.EmptyList;
			};

			ReadChar = (list, env, eval) => {
				IScheminType port = list.Car();
				ScheminPort readFrom = eval.CurrentInputPort;
				if ((port as ScheminPort) != null)
				{
					readFrom = (ScheminPort) port;
				}

				int read = readFrom.InputStream.Read();

				// Ditch the \r if we get one, because windows CR+LF newline is stupid
				if ((char) readFrom.InputStream.Peek() == '\r')
				{
					readFrom.InputStream.Read();
				}

				return new ScheminChar(read);
			};

			ReadLine = (list, env, eval) => {
				IScheminType port = list.Car();
				ScheminPort readFrom = eval.CurrentInputPort;
				if ((port as ScheminPort) != null)
				{
					readFrom = (ScheminPort) port;
				}

				StringBuilder built = new StringBuilder();
				char next;
				bool emptyInput = true;
				for (;;)
				{
					int nextRead = readFrom.InputStream.Read();
					next = (char) nextRead;

					if (nextRead == -1 || (next == '\n' && !emptyInput))
					{
						break;
					}

					if (next != '\r')
					{
						built.Append(next);
						emptyInput = false;
					}
				}


				return new ScheminString(built.ToString());
			};

			Read = (list, env, eval) => {
				IScheminType port = list.Car();
				ScheminPort readFrom = eval.CurrentInputPort;
				if ((port as ScheminPort) != null)
				{
					readFrom = (ScheminPort) port;
				}

				Tokenizer t = new Tokenizer();
				Parser p = new Parser();

				bool completeInput = false;
				bool emptyInput = true;
				int openParens = 0;
				int closeParens = 0;

				List<Token> partialInput = new List<Token>();

				while (completeInput != true)
				{
					StringBuilder built = new StringBuilder();
					char next;
					for (;;)
					{
						int nextRead = readFrom.InputStream.Read();
						next = (char)nextRead;

						if (nextRead == -1 || (next == '\n' && !emptyInput))
						{
							break;
						}

						if (next != '\r')
						{
							built.Append(next);
							emptyInput = false;
						}
					}

					var lineTokens = t.Tokenize(built.ToString());

					foreach (Token token in lineTokens)
					{
						partialInput.Add(token);
						if (token.Type == TokenType.OpenParen)
						{
							openParens++;
						}
						else if (token.Type == TokenType.CloseParen)
						{
							closeParens++;
						}
					}

					if (openParens == closeParens)
					{
						completeInput = true;
						break;
					}
				}

				var parsed = p.Parse(partialInput, true).Car();

				return parsed;
			};
		}
	}
}
