using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;

namespace B4F.TotalGiro.Utils.Linq
{
    public static class ExpressionExtensions
    {
        public static IQueryable<TSource> WhereAll<TSource>(this IQueryable<TSource> source, 
                                                            IEnumerable<Expression<Func<TSource, bool>>> predicates)
        {
            foreach (var pred in predicates)
                source = source.Where(pred);

            return source;
        }

        public static Expression<Func<T, bool>> PredicateAnd<T>(this IEnumerable<Expression<Func<T, bool>>> predicates)
        {
            if (predicates.Count() > 0)
                return predicates.Aggregate(PredicateAnd);
            else
                return x => true;
        }

        public static Expression<Func<T, bool>> PredicateAnd<T>(this Expression<Func<T, bool>> pred1, Expression<Func<T, bool>> pred2)
        {
            return Expression.Lambda<Func<T, bool>>(Expression.AndAlso(pred1.Body, pred2.Body), 
                                                    pred1.Parameters);
        }

        public static Expression<Func<T, bool>> PredicateOr<T>(this IEnumerable<Expression<Func<T, bool>>> predicates)
        {
            if (predicates.Count() > 0)
                return predicates.Aggregate(PredicateOr);
            else
                return x => false;
        }

        public static Expression<Func<T, bool>> PredicateOr<T>(this Expression<Func<T, bool>> pred1, Expression<Func<T, bool>> pred2)
        {
            return Expression.Lambda<Func<T, bool>>(Expression.OrElse(pred1.Body, pred2.Body),
                                                    pred1.Parameters);
        }
    }
}
