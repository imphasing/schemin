
namespace Schemin.Parse
{
	using System.Collections.Generic;

	public class ScheminList : ISchemeType
	{
		public LinkedList<ISchemeType> List;

		public ScheminList(LinkedList<ISchemeType> list)
		{
			this.List = list;
		}
	}
}
