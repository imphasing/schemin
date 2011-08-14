
namespace Schemin.AST
{
	public class ScheminBool : IScheminType
	{
		public static ScheminBool True;
		public static ScheminBool False;
		
		static ScheminBool()
		{
			True = new ScheminBool(true);
			False = new ScheminBool(false);
		}

		public static ScheminBool GetValue(bool value)
		{
			if (value)
			{
				return True;
			}
			
			return False;
		}

		public bool Value;

		public ScheminBool(string value)
		{
			if (value == "#f")
			{
				this.Value = false;
			}
			else
			{
				this.Value = true;
			}
		}

		public ScheminBool(bool value)
		{
			this.Value = value;
		}

        public bool Quoted()
        {
            return false;
        }

        public void Quote()
        {
        }

        public void UnQuote()
        {
        }

		public override string ToString()
		{
			if (Value)
			{
				return "#t";
			}
			return "#f";
		}

		public bool Equals(IScheminType type)
		{
			if (this.GetType() != type.GetType())
			{
				return false;
			}

			ScheminBool temp = (ScheminBool) type;
			if (this.Value == temp.Value)
			{
				return true;
			}

			return false;
		}

		public ScheminBool BoolValue()
		{
			if (this.Value)
			{
				return ScheminBool.True;

			}
			else
			{
				return ScheminBool.False;
			}
		}
	}
}
