
namespace Schemin.AST
{
	using System.Collections.Generic;

	public class ScheminList : IScheminType
	{
		public LinkedList<IScheminType> List;

		public ScheminList(LinkedList<IScheminType> list)
		{
			this.List = list;
		}
	}
}
