namespace CsBenchmark
{
    using BenchmarkDotNet.Configs;
    using BenchmarkDotNet.Jobs;

    public sealed class BenchmarkConfig : ManualConfig
    {
        public BenchmarkConfig()
        {
            // Use .NET 6.0 default mode:
            AddJob(Job.Default.WithId("STD"));

            // Use Dynamic PGO mode:
            AddJob(Job.Default.WithId("PGO")
                .WithEnvironmentVariables(
                    new EnvironmentVariable("DOTNET_TieredPGO"          , "1"),
                    new EnvironmentVariable("DOTNET_TC_QuickJitForLoops", "1"),
                    new EnvironmentVariable("DOTNET_ReadyToRun"         , "0")));
        }
    }
}