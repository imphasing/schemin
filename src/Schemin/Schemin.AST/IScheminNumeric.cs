
namespace Schemin.AST
{
	using System.Numerics;

	public interface IScheminNumeric
	{
		decimal DecimalValue();
		BigInteger IntegerValue(); 
	}
}
