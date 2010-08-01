using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace B4F.TotalGiro.Utils
{
    public static class StringExtensions
    {
        public static IEnumerable<string> WhereNonEmpty(this IEnumerable<string> strings)
        {
            return strings.Where(s => s != null)
                          .Select(s => s.Trim())
                          .Where(s => s != "");
        }

        public static string JoinStrings(this IEnumerable<string> strings)
        {
            return strings.JoinStrings(", ");
        }

        public static string JoinStrings(this IEnumerable<string> strings, string separator)
        {
            return strings.JoinStrings("", "", separator);
        }

        public static string JoinStrings(this IEnumerable<string> strings, string beforeEach, string afterEach, string separator)
        {
            return string.Join(separator, strings.Select(s => beforeEach + s + afterEach).ToArray());
        }

        /// <param name="lastSeparator">
        /// Separator between last two items in list, different from separator between first (n - 1) items.
        /// E.g.: if list is {"A", "B", "C", "D"}, separator is ", " and lastSeparator is " and ", 
        /// the result will be "A, B, C and D".
        /// </param>
        public static string JoinStrings(this IEnumerable<string> strings, string beforeEach, string afterEach, string separator,
                                                                                                                string lastSeparator)
        {
            string[] stringsArr = strings.ToArray();
            int firstGroupLength = Math.Max(0, stringsArr.Length - 2);

            return stringsArr.Take(firstGroupLength)
                             .JoinStrings(beforeEach, afterEach, separator) +
                   (firstGroupLength > 0 ? separator : "") +
                   stringsArr.Skip(firstGroupLength)
                             .JoinStrings(beforeEach, afterEach, lastSeparator);
        }

        public static string RepeatString(this string s, int count)
        {
            return string.Join("", Enumerable.Repeat(s, count).ToArray());
        }
    }
}
