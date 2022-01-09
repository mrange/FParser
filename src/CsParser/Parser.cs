using System.Globalization;
using System.Text;
using static System.FormattableString;

namespace CsParser
{
  public delegate bool Parser<T>(string input, ref int pos, out T value);

  public struct Unit { }

  public static class Parser
  {
    public static class Details
    {
      public static int Scan(string input, int pos, Func<char, int, bool> f)
      {
        var current = pos;
        while (current < input.Length && f(input[current], current-pos))
        {
          ++current;
        }
        return current;
      }
    }

    public static Parser<T> Fail<T>() => (string input, ref int pos, out T value) =>
    {
      value = default!;
      return false;
    };

    public static Parser<Unit> Eof() => (string input, ref int pos, out Unit value) =>
    {
      value = default!;
      return pos >= input.Length;
    };

    public static (Parser<T>, Action<Parser<T>>) Forward<T>()
    {
      Parser<T> shared = Fail<T>();
      Parser<T> p = (string input, ref int pos, out T value) =>
        {
          return shared(input, ref pos, out value);
        };
      var s = (Parser<T> pp) => 
      {
        shared = pp;
      };
      return (p, s);
    }

    public static Parser<T> Debug<T>(this Parser<T> p, string nm) => (string input, ref int pos, out T value) =>
    {
      Console.WriteLine(Invariant($"PARSE - {nm} - {pos}"));
      if (p(input, ref pos, out value))
      {
        Console.WriteLine(Invariant($"GOOD  - {nm} - {pos} - {value}"));
        return true;
      }
      else
      {
        Console.WriteLine(Invariant($"FAIL  - {nm} - {pos}"));
        value = default!;
        return false;
      }
    };

    public static Parser<T> KeepLeft<T, U>(this Parser<T> f, Parser<U> s) => (string input, ref int pos, out T value) =>
    {
      if (f(input, ref pos, out value))
      {
        return s(input, ref pos, out var sv);
      }
      else
      {
        value = default!;
        return false;
      }
    };

    public static Parser<U> KeepRight<T, U>(this Parser<T> f, Parser<U> s) => (string input, ref int pos, out U value) =>
    {
      if (f(input, ref pos, out var fv))
      {
        return s(input, ref pos, out value);
      }
      else
      {
        value = default!;
        return false;
      }
    };

    public static Parser<T> Between<T>(this Parser<T> p, Parser<Unit> b, Parser<Unit> e) => 
      b
        .KeepRight(p)
        .KeepLeft(e)
        ;

    public static Parser<T> OrElse<T>(this Parser<T> f, Parser<T> s) => (string input, ref int pos, out T value) =>
    {
      var spos = pos;
      if (f(input, ref pos, out var fv))
      {
        value = fv;
        return true;
      }
      else
      {
        pos = spos;
        return s(input, ref pos, out value);
      }
    };

    public static Parser<U> Map<T, U>(this Parser<T> p, Func<T, U> f) => (string input, ref int pos, out U value) =>
    {
      if (p(input, ref pos, out var v))
      {
        value = f(v);
        return true;
      }
      else
      {
        value = default!;
        return false;
      }
    };

    public static Parser<Unit> SkipChar(char ch) => (string input, ref int pos, out Unit value) =>
    {
      if (pos < input.Length && input[pos] == ch)
      {
        value = default;
        ++pos;
        return true;
      }
      else
      {
        value = default;
        return false;
      }
    };

    public static Parser<char> CharSat(Func<char, bool> sat) => (string input, ref int pos, out char value) =>
    {
      if (pos < input.Length && sat(input[pos]))
      {
        value = input[pos];
        ++pos;
        return true;
      }
      else
      {
        value = default;
        return false;
      }
    };

    public static Parser<Unit> SkipWhiteSpace() => (string input, ref int pos, out Unit value) =>
    {
      value = default;
      pos   = Details.Scan(input, pos, (ch, p) => (ch >= '\u0009' && ch <= '\u000D') || (ch == '\u0020'));
      return true;
    };

    public static Parser<string> StringSat1(Func<char, int, bool> sat) => (string input, ref int pos, out string value) =>
    {
      var end = Details.Scan(input, pos, sat);
      if (pos < end)
      {
        value = input.Substring(pos, end - pos);
        pos = end;
        return true;
      }
      else
      {
        value = default!;
        return false;
      }
    };

    public static Parser<T> ChainLeft1<T>(this Parser<T> p, Parser<Func<T, T, T>> op) => (string input, ref int pos, out T value) =>
    {
      if (p(input, ref pos, out var hv))
      {
        var agg = hv;

        while(true)
        {
          var spos = pos;
          if (op(input, ref pos, out var opf))
          {
            if (p(input, ref pos, out var tv))
            {
              agg = opf(agg, tv);
            }
            else
            {
              value = default!;
              return false;
            }
          }
          else
          {
            pos   = spos;
            value = agg;
            return true;
          }
        }
      }
      else
      {
        value = default!;
        return false;
      }
    };

    public static Parser<int> Int() => (string input, ref int pos, out int value) =>
    {
      var end = Details.Scan(input, pos, (ch, p) => ch >= '0' && ch <= '9');

      if (pos < end)
      {
        value = int.Parse(input.AsSpan(pos, end - pos), NumberStyles.Integer, CultureInfo.InvariantCulture);
        pos = end;
        return true;
      }
      else
      {
        value = default;
        return false;
      }
    };
  }

  public abstract record Expression()
  {
    public string AsString()
    {
      var sb = new StringBuilder(16);
      void Traverse(Expression e)
      {
        switch(e)
        {
          case IntExpression ie:
            sb.Append(ie.Value);
            break;
          case VarExpression ve:
            sb.Append(ve.Variable);
            break;
          case BinaryExpression be:
            sb.Append('(');
            Traverse(be.Left);
            sb.Append(be.Op);
            Traverse(be.Right);
            sb.Append(')');
            break;
          default:
            throw new Exception("Oops");
        }
      }
      Traverse(this);
      return sb.ToString();
    }
  }

  public record IntExpression(int Value)        : Expression();
  public record VarExpression(string Variable)  : Expression();
  public record BinaryExpression(
      char                Op
    , Func<int, int, int> Opf
    , Expression          Left
    , Expression          Right
    ) : Expression();

  public class CsParserCalculator
  {
    readonly Parser<Expression> _parser = CreateParser();

    static Parser<Expression> CreateParser()
    {
      static Parser<Unit> Token(char ch) => Parser
        .SkipChar(ch)
        .KeepRight(Parser.SkipWhiteSpace())
        ;

      static Parser<T> Op<T>(Func<char, bool> sat, Func<char, T> map) => Parser
        .CharSat(sat)
        .Map(map)
        .KeepLeft(Parser.SkipWhiteSpace())
        ;

      var (pexpr, spexpr) = Parser.Forward<Expression>();

      var pint = Parser
        .Int()
        .Map(v => new IntExpression(v) as Expression)
        .KeepLeft(Parser.SkipWhiteSpace())
        ;

      var pvar = Parser
        .StringSat1((ch, p) => (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z'))
        .Map(v => new VarExpression(v) as Expression)
        .KeepLeft(Parser.SkipWhiteSpace())
        ;

      var pgroup = pexpr.Between(Token('('), Token(')'));

      var pterm = pint.OrElse(pvar).OrElse(pgroup);

      static Func<Expression, Expression, Expression> Op0(char ch) =>
        ch switch
        {
          '*' => (l, r) => new BinaryExpression('*', (l, r) => l * r, l, r)
        , '/' => (l, r) => new BinaryExpression('/', (l, r) => l / r, l, r)
        , _   => throw new Exception("Oops")
        };
      static Func<Expression, Expression, Expression> Op1(char ch) =>
        ch switch
        {
          '+' => (l, r) => new BinaryExpression('+', (l, r) => l + r, l, r)
        , '-' => (l, r) => new BinaryExpression('-', (l, r) => l - r, l, r)
        , _   => throw new Exception("Oops")
        };

      var pop0 = pterm.ChainLeft1(Op(ch => ch == '*' || ch == '/', Op0));
      var pop1 = pop0.ChainLeft1(Op(ch => ch == '+' || ch == '-', Op1));

      spexpr(pop1);

      return Parser.SkipWhiteSpace().KeepRight(pop1).KeepLeft(Parser.Eof());

    }

    public Expression? Parse(string input)
    {
      var pos = 0;
      if (_parser(input, ref pos, out var e))
      {
        return e;
      }
      else
      {
        return null;
      }
    }

    public static void Test()
    {
      var parser = new CsParserCalculator();
      var testCases =
        new [] 
        {
          "1"
        , "123"
        , "(123+2)*x"
        , "(123+2)*5"
        , "x"
        , "force*mass/(r*r)-G*MASS*mass/(r*r)"
        };

      Console.WriteLine("CsParser calculator");
      foreach(var testCase in testCases)
      {
        var result = parser.Parse(testCase);
        var s = result is not null
          ? Invariant($"OK : {result.AsString()}")
          : "ERR: <NULL>"
          ;
        Console.WriteLine(Invariant($"  {testCase} -> {s}"));
      }
    }
  }

}
