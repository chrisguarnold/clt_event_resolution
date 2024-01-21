using System.Threading.Tasks;

namespace NlpParser
{
	public class Program
	{
		static async Task Main(string[] args)
		{
			var parser = new Parser();
			await parser.Run();
		}
	}
}