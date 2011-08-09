
namespace Schemin.AST
{
	public interface IScheminType
	{
		bool Equals(IScheminType type);
		ScheminBool BoolValue();
	}
}
