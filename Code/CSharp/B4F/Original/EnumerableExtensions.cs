using System;
using System.Collections.Generic;
using System.Linq;
using B4F.TotalGiro.Utils.Tuple;

namespace B4F.TotalGiro.Utils.Linq
{
    public static class EnumerableExtensions
    {
        public static IEnumerable<IEnumerable<T>> Split<T>(this IEnumerable<T> source, int subsequenceLength)
        {
            for (IEnumerable<T> subseq = source.Take(subsequenceLength), rest = source.Skip(subsequenceLength);
                 subseq.Count() > 0;
                 subseq = rest.Take(subsequenceLength), rest = rest.Skip(subsequenceLength))
                yield return subseq;
        }

        public static IEnumerable<T> ConcatMany<T, Col>(this IEnumerable<Col> source)
            where Col : IEnumerable<T>
        {
            return source.Cast<IEnumerable<T>>().SelectMany(subSeq => subSeq);
        }

        public static IEnumerable<T> ConcatMany<T>(this IEnumerable<IEnumerable<T>> source)
        {
            return source.ConcatMany<T, IEnumerable<T>>();
        }

        public static IEnumerable<T> ConcatMany<T>(this IEnumerable<T[]> source)
        {
            return source.ConcatMany<T, T[]>();
        }

        public static IEnumerable<T> ConcatMany<T>(this IEnumerable<List<T>> source)
        {
            return source.ConcatMany<T, List<T>>();
        }

        public static IEnumerable<Tuple<A, B>> Zip<A, B>(this IEnumerable<A> a, IEnumerable<B> b)
        {
            return a.Zip(b, (aa, bb) => Tuple.Tuple.Create(aa, bb));
        }

        public static IEnumerable<R> Zip<A, B, R>(this IEnumerable<A> a, IEnumerable<B> b,
                                                  Func<A, B, R> func)
        {
            IEnumerator<A> enumA = a.GetEnumerator();
            IEnumerator<B> enumB = b.GetEnumerator();

            while (enumA.MoveNext() && enumB.MoveNext())
                yield return func(enumA.Current, enumB.Current);
        }

        public static IEnumerable<Tuple<A, B, C>> Zip3<A, B, C>(this IEnumerable<A> a, IEnumerable<B> b, IEnumerable<C> c)
        {
            return a.Zip3(b, c, (aa, bb, cc) => Tuple.Tuple.Create(aa, bb, cc));
        }

        public static IEnumerable<R> Zip3<A, B, C, R>(this IEnumerable<A> a, IEnumerable<B> b, IEnumerable<C> c,
                                                      Func<A, B, C, R> func)
        {
            return Zip(Zip<A, B, Func<C, R>>(a, b, (aa, bb) => cc => func(aa, bb, cc)),
                       c,
                       (f, ccc) => f(ccc));
        }

        public static IEnumerable<Tuple<A, B, C, D>> Zip4<A, B, C, D>(this IEnumerable<A> a, IEnumerable<B> b, IEnumerable<C> c, IEnumerable<D> d)
        {
            return a.Zip4(b, c, d, (aa, bb, cc, dd) => Tuple.Tuple.Create(aa, bb, cc, dd));
        }

        public static IEnumerable<R> Zip4<A, B, C, D, R>(this IEnumerable<A> a, IEnumerable<B> b, IEnumerable<C> c, IEnumerable<D> d,
                                                         Func<A, B, C, D, R> func)
        {
            return Zip(Zip3<A, B, C, Func<D, R>>(a, b, c, (aa, bb, cc) => dd => func(aa, bb, cc, dd)),
                       d,
                       (f, ddd) => f(ddd));
        }

        public static IEnumerable<T> Singleton<T>(T item)
        {
            return (new T[] { item }).AsEnumerable();
        }

        public static IEnumerable<int> Range(int from, int to)
        {
            return Enumerable.Range(from, to - from + 1);
        }

        public static IEnumerable<int> Range(int from, int to, int step)
        {
            if (from <= to && step > 0)
                for (int i = from; i <= to; i += step)
                    yield return i;
            else if (from >= to && step < 0)
                for (int i = from; i >= to; i += step)
                    yield return i;
            else
                yield break;
        }

        public static IEnumerable<int> RangeFrom(int from)
        {
            return RangeFrom(from, 1);
        }

        public static IEnumerable<int> RangeFrom(int from, int step)
        {
            for (int i = from; ; i += step)
                yield return i;
        }
    }
}
