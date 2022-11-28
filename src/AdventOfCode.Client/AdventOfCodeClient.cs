using Refit;

namespace AdventOfCode.Client;

public interface IAdventOfCodeClient
{
    [Get("/{year}/day/{day}/input")]
    internal Task<string> GetInputImpl(int year, int day, [Header("Cookie")] string cookie);

    public string GetInput(int year, int day, string session) =>
        GetInputImpl(year, day, $"session={session}").Result;
}
