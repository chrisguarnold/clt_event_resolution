using System;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Net.Http;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using System.Xml.Linq;
using edu.stanford.nlp.ling;
using edu.stanford.nlp.pipeline;
using edu.stanford.nlp.tagger.maxent;
using edu.stanford.nlp.time;
using edu.stanford.nlp.util;
using java.util;
using SharpCompress.Readers.Tar;
using Spectre.Console;

namespace NlpParser
{
	public class Parser
	{
		[DllImport("kernel32.dll", SetLastError = true)]
		[return: MarshalAs(UnmanagedType.Bool)]
		static extern bool DeleteFile(string lpFileName);

		private const string StanfordCoreNlpVersion = "stanford-corenlp-4.2.0";
		private const string StandfordModelUrl = "https://nlp.stanford.edu/software/stanford-corenlp-4.2.0.zip";

		public async Task Run()
		{
			using var corpusExtractedDirectory = new TempDirectory("Extracted corpus");
			using var corpusDecompressedDirectory = new TempDirectory("decompressed corpus");
			ExtractCorpus(corpusExtractedDirectory);
			DecompressCorpus(corpusExtractedDirectory, corpusDecompressedDirectory);

			// await ExtractTimeXAnnotations();
			// CombineAnnotationsWithOriginalFiles();
			// MoveTimeXTagsIntoXmlFile();
			AnsiConsole.MarkupLine("Cleaning up...");
		}


		private void ExtractCorpus(TempDirectory targetPath)
		{
			AnsiConsole.Markup($"Extracting corpus to [yellow]{(string)targetPath}[/]...");
			UnTarFromFile(@"nyt_corpus_LDC2008T19.tgz", targetPath);
			AnsiConsole.Markup("Done");
			AnsiConsole.WriteLine();
		}

		private void DecompressCorpus(string sourceDirectory, TempDirectory targetDirectory)
		{
			var tgzCount = Directory.EnumerateFiles(sourceDirectory, "*.tgz", SearchOption.AllDirectories).Count();
			
			AnsiConsole.Progress()
				.Start(ctx =>
				{
					var task = ctx.AddTask($"Decompressing corpus to [yellow]{(string)targetDirectory}[/]...", maxValue: tgzCount);

					Parallel.ForEach(Directory.EnumerateFiles(sourceDirectory, "*.tgz", SearchOption.AllDirectories),
						(file, _) =>
						{
							UnTarFromFile(file, targetDirectory);
							task.Increment(1);
						});
				});
		}

		void CombineAnnotationsWithOriginalFiles()
		{
			var annotationsPath = "annotations";
			var originalFilesPath = "cop_26";
			var outputPath = "output";

			if (Directory.Exists(outputPath) == false)
				Directory.CreateDirectory(outputPath);

			foreach (var file in Directory.EnumerateFiles(annotationsPath, "*.xml", SearchOption.AllDirectories))
			{
				Console.WriteLine($"Updating {file}...");

				var originalFile = File.ReadAllText(Path.Combine(originalFilesPath, Path.GetFileName(file)));
				var annotations = File.ReadAllText(Path.Combine(annotationsPath, Path.GetFileName(file)));
				originalFile = originalFile.Insert(originalFile.IndexOf("</nitf>"), annotations);

				File.WriteAllText(Path.Combine(outputPath, Path.GetFileName(file)), originalFile);
			}
		}

		void MoveTimeXTagsIntoXmlFile()
		{
			var annotationsPath = "annotations";
			var originalsPath = "cop_26";

			foreach (var file in Directory.EnumerateFiles(annotationsPath, "*.xml", SearchOption.AllDirectories))
			{
				Console.WriteLine($"Updating {file}...");
				var parentDir = new DirectoryInfo(Path.GetDirectoryName(file));

				var text = File.ReadAllText(Path.Combine(originalsPath, Path.GetFileName(file)));
				var lines = text.Split('\n');
				var timeXLines = lines.Where(l => l.StartsWith("<TIMEX3"));

				var insertAt = text.IndexOf("</nitf>");
				var sb = new StringBuilder(text);
				sb.Insert(insertAt, "\n");

				foreach (var line in timeXLines)
				{
					sb.Insert(insertAt, line + "\n");
					insertAt += line.Length + 1;
				}

				var startIndex = sb.ToString().IndexOf("</nitf>") + "</nitf>".Length;
				sb.Remove(startIndex, sb.Length - startIndex);

				File.WriteAllText(Path.Combine("annotations", parentDir.Name, Path.GetFileName(file)), sb.ToString());
			}
		}

		private async Task ExtractTimeXAnnotations()
		{
			if (!Directory.Exists(StanfordCoreNlpVersion))
			{
				AnsiConsole.MarkupLine($"Standford NLP model not found. Downloading from [yellow]{StandfordModelUrl}[/]");

				using var client = new HttpClient();
				using var outputStream = new MemoryStream();
				await AnsiConsole.Progress()
					.Columns(new ProgressColumn[]
					{
						new TaskDescriptionColumn(),
						new ProgressBarColumn(),
						new PercentageColumn(),
						new RemainingTimeColumn(),
						new SpinnerColumn(),
					})
					.StartAsync(async ctx =>
					{
						var task = ctx.AddTask("Downloading...");
						await Download(client, task, StandfordModelUrl, outputStream);
					});

				AnsiConsole.MarkupLine($"Extracting model to [yellow]'{StanfordCoreNlpVersion}'[/]");
				using var zipArchive = new ZipArchive(outputStream, ZipArchiveMode.Read);
				zipArchive.ExtractToDirectory(".");
			}

			using var cop26Directory = new TempDirectory("Cop_26");
			UnZipFromFile("cop_26.zip", cop26Directory);

			var jarRoot = StanfordCoreNlpVersion;
			var modelsDirectory = Path.Combine(jarRoot, @"edu\stanford\nlp\models");

			// Annotation pipeline configuration
			var pipeline = new AnnotationPipeline();
			pipeline.addAnnotator(new TokenizerAnnotator(false));
			pipeline.addAnnotator(new WordsToSentencesAnnotator(false));

			// Loading POS Tagger and including them into pipeline
			var tagger = new MaxentTagger(Path.Combine(modelsDirectory, @"pos-tagger\english-left3words-distsim.tagger"));
			pipeline.addAnnotator(new POSTaggerAnnotator(tagger));

			// SUTime configuration
			var sutimeRules = modelsDirectory + @"\sutime\defs.sutime.txt,"
			                                  + modelsDirectory + @"\sutime\english.holidays.sutime.txt,"
			                                  + modelsDirectory + @"\sutime\english.sutime.txt";
			var props = new Properties();
			props.setProperty("sutime.rules", sutimeRules);
			props.setProperty("sutime.binders", "0");
			pipeline.addAnnotator(new TimeAnnotator("sutime", props));

			var timexAnnotationClass = new TimeAnnotations.TimexAnnotation().getClass();
			var docDateAnnotationClass = new CoreAnnotations.DocDateAnnotation().getClass();
			var timexAnnotationsClass = new TimeAnnotations.TimexAnnotations().getClass();

			var outputDirectory = "annotations";
			if (Directory.Exists(outputDirectory) == false)
				Directory.CreateDirectory(outputDirectory);

			Parallel.ForEach(Directory.EnumerateFiles(cop26Directory, "*.xml", SearchOption.AllDirectories),
				new ParallelOptions(),
				file =>
				{
					Console.WriteLine($"Parsing file {file}...");

					// extract the date
					// using var fileStream = File.OpenRead(file);
					XDocument document = null;
					try
					{
						var xml = File.ReadAllText(file, Encoding.UTF8);
						document = XDocument.Parse(xml.Replace("&", "&amp;amp;"), LoadOptions.PreserveWhitespace);
					}
					catch (Exception e)
					{
						Console.WriteLine($"Couldn't parse file {file}\n{e.Message}");
						return;
					}

					var metaNodes = document.Descendants("meta");

					var publicationDayOfMonth = metaNodes
						.First(e => e.HasAttributes && e.Attribute("name") != null && e.Attribute("name").Value == "publication_day_of_month")
						.Attribute("content").Value;

					var publicationMonth = metaNodes
						.First(e => e.HasAttributes && e.Attribute("name") != null && e.Attribute("name").Value == "publication_month")
						.Attribute("content").Value;

					var publicationYear = metaNodes
						.First(e => e.HasAttributes && e.Attribute("name") != null && e.Attribute("name").Value == "publication_year")
						.Attribute("content").Value;

					var content = document.Descendants("body.content").First();

					var annotation = new Annotation(content.Value);
					annotation.set(docDateAnnotationClass, $"{publicationYear}-{publicationMonth}-{publicationDayOfMonth}");
					pipeline.annotate(annotation);

					var timexAnnsAll = annotation.get(timexAnnotationsClass) as ArrayList;
					var timexAnnotations = new List<Timex>();

					foreach (CoreMap cm in timexAnnsAll)
					{
						// var tokens = cm.get(new CoreAnnotations.TokensAnnotation().getClass()) as List;
						// var first = tokens.get(0);
						// var last = tokens.get(tokens.size() - 1);
						// var time = cm.get(new TimeExpression.Annotation().getClass()) as TimeExpression;

						var timex = cm.get(timexAnnotationClass) as Timex;
						timexAnnotations.Add(timex);
						// Console.WriteLine("{0} [from char offset {1} to {2}] --> {3}",
						// 	cm, first, last, time.getTemporal());
					}

					File.WriteAllText(Path.Combine(outputDirectory, Path.GetFileName(file)), string.Join("\n", timexAnnotations));
				});
		}

		void UnTarFromFile(string tarFilePath, string destinationDirectory, Func<Stream, bool> filter = null)
		{
			using var stream = new FileStream(tarFilePath, FileMode.Open, FileAccess.Read, FileShare.None);
			UnTarFromStream(stream, filter, destinationDirectory);
		}

		void UnZipFromFile(string zipFilePath, string destinationDirectory)
		{
			using var fileStream = File.OpenRead(zipFilePath);
			using var zipArchive = new ZipArchive(fileStream, ZipArchiveMode.Read);
			zipArchive.ExtractToDirectory(destinationDirectory);
		}

		void UnTarFromStream(Stream fileStream, Func<Stream, bool> func, string destinationDirectory)
		{
			using var gzipStream = new GZipStream(fileStream, CompressionMode.Decompress);
			using var reader = TarReader.Open(gzipStream);

			while (reader.MoveToNextEntry())
			{
				var entry = reader.Entry;

				if (entry.IsDirectory)
					continue;

				using var memoryStream = new MemoryStream();
				using var dataStream = reader.OpenEntryStream();

				dataStream.CopyTo(memoryStream);
				memoryStream.Seek(0, SeekOrigin.Begin);

				if (func != null)
				{
					using var streamReader = new StreamReader(memoryStream);
					var content = streamReader.ReadToEnd();

					if (!HasRelevantContent(content)) continue;
				}


				var targetDirectory = Path.Combine(destinationDirectory, entry.Key);
				if (!Directory.Exists(Path.GetDirectoryName(targetDirectory)))
					Directory.CreateDirectory(Path.GetDirectoryName(targetDirectory));

				var writeStream =
					new FileStream(Path.Join(destinationDirectory, entry.Key), FileMode.CreateNew, FileAccess.Write, FileShare.None);
				writeStream.Write(memoryStream.GetBuffer(), 0, (int)memoryStream.Length);
				writeStream.Dispose();
			}
		}

		bool HasRelevantContent(string content)
		{
			var searchStrings = new List<string>
			{
				"presidential election of 1988",
				"presidential election of 1992",
				"presidential election of 1996",
				"presidential election of 2000",
				"presidential election of 2004"
			};

			return searchStrings.Any(content.Contains);
		}

		async Task Download(HttpClient client, ProgressTask task, string url, Stream output)
		{
			try
			{
				using var response = await client.GetAsync(url, HttpCompletionOption.ResponseHeadersRead);
				response.EnsureSuccessStatusCode();

				task.MaxValue(response.Content.Headers.ContentLength ?? 0);
				task.StartTask();

				var filename = url.Substring(url.LastIndexOf('/') + 1);
				AnsiConsole.MarkupLine($"Starting download of [u]{filename}[/] ({task.MaxValue} bytes)");

				using var contentStream = await response.Content.ReadAsStreamAsync();
				var buffer = new byte[8192];
				while (true)
				{
					var read = await contentStream.ReadAsync(buffer, 0, buffer.Length);
					if (read == 0)
					{
						AnsiConsole.MarkupLine($"Download of [u]{filename}[/] [green]completed![/]");
						break;
					}

					task.Increment(read);
					await output.WriteAsync(buffer, 0, read);
				}
			}
			catch (Exception ex)
			{
				AnsiConsole.MarkupLine($"[red]Error:[/] {ex}");
			}
		}
	}
}