
namespace Schemin.AST
{
	public interface IScheminType
	{
        bool Quoted();
        void Quote();
        void UnQuote();
		bool Equals(IScheminType type);
		ScheminBool BoolValue();
	}
}
