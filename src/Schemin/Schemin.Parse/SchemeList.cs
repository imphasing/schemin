
namespace Schemin.Parse
{
	using System.Collections.Generic;

	public class SchemeList : ISchemeType
	{
		public LinkedList<ISchemeType> List;

		public SchemeList(LinkedList<ISchemeType> list)
		{
			this.List = list;
		}
	}
}
