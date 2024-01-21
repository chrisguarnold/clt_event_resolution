using Spectre.Console;
using System;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Threading.Tasks;

namespace NlpParser
{
	internal class TempDirectory : IDisposable
	{
		private readonly string _name;

		[DllImport("kernel32.dll", SetLastError = true)]
		[return: MarshalAs(UnmanagedType.Bool)]
		static extern bool DeleteFile(string lpFileName);

		private string _tempPath;

		public TempDirectory(string name)
		{
			_name = name;
			_tempPath = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName());
			while(Directory.Exists(_tempPath))
				_tempPath = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName());

			Directory.CreateDirectory(_tempPath);
		}

		public void Dispose()
		{
			AnsiConsole.MarkupLine($"Deleting {_name} directory...");
			AnsiConsole.Progress()
				.Start(ctx =>
				{
					var fileCount = Directory.EnumerateFiles(_tempPath, "*.*", SearchOption.AllDirectories).Count();

					var deleteFilesTask = ctx.AddTask($"Deleting {fileCount:N0} files...", maxValue: fileCount);
					Parallel.ForEach(Directory.EnumerateFiles(_tempPath, "*.*", SearchOption.AllDirectories),
						file =>
						{
							DeleteFile(file);
							deleteFilesTask.Increment(1);
						});
				});
			Directory.Delete(_tempPath, true);
		}

		public static implicit operator string(TempDirectory d) { return d._tempPath; }
	}
}
