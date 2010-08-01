using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using B4F.TotalGiro.Accounts;
using B4F.TotalGiro.Accounts.Portfolios.FundPositions;
using B4F.TotalGiro.ClientApplicationLayer.SecurityLayer;
using B4F.TotalGiro.Dal;
using B4F.TotalGiro.Valuations;

namespace B4F.TotalGiro.ClientApplicationLayer.Charts
{
    public class SeriesInfo
    {
        public SeriesInfo(int key, string seriesName, DateTime startDate, DateTime endDate)
        {
            Key = key;
            SeriesName = seriesName;
            StartDate = startDate;
            EndDate = endDate;
        }

        public int Key { get; private set; }
        public string SeriesName { get; private set; }
        public DateTime StartDate { get; set; }
        public DateTime EndDate { get; set; }
    }

    public static class ChartsAdapter
    {
        #region Utility methods (common between charts)

        public static double GetDateIncrement(DateTime startDate, DateTime endDate)
        {
            return GetDateIncrement(startDate, endDate, 100);
        }

        public static double GetDateIncrement(DateTime startDate, DateTime endDate, int maxDateCount)
        {
            return Math.Max(1, (endDate.Date - startDate.Date).TotalDays / maxDateCount + 1);
        }

        public static DateTime[] GenerateDates(DateTime startDate, DateTime endDate, double increment)
        {
            // maximum number of parameters supported by NHibernate in an SQL query (it is actually 2100)
            const int maxDateCount = 2000;
            
            startDate = startDate.Date;
            if (startDate < new DateTime(2000, 1, 1))
                startDate = new DateTime(2000, 1, 1);

            endDate = endDate.Date;

            if (increment == 0 || startDate > endDate)
                return new DateTime[] {};
            
            List<DateTime> dates = new List<DateTime>();
            for (DateTime date = startDate; date <= endDate.Date; date = date.AddDays(increment))
                dates.Add(date.Date);

            if (dates.Last() < endDate)
                dates.Add(endDate);

            return dates.Reverse<DateTime>().Take(maxDateCount).Reverse()
                        .ToArray();
        }

        #endregion


        #region Portfolio Chart

        public static List<SeriesInfo> GetAccountSeriesInfoList(int contactId)
        {
            using (IDalSession session = NHSessionFactory.CreateSession())
            {
                return SecurityLayerAdapter.GetOwnedContactAccounts(session, contactId, true)
                                           .ConvertAll(a => new SeriesInfo(a.Key, a.Number, a.FirstTransactionDate, a.LastValuationDate));
            }
        }

        public static DataTable GetValuationsTotalPortfolio(int accountId, DateTime[] dates)
        {
            using (IDalSession session = NHSessionFactory.CreateSession())
            {
                return SecurityLayerAdapter.GetOwnedValuationsTotalPortfolio(session, accountId, dates)
                                           .Select(v => new { v.Date, TotalValueQuantity = v.TotalValue.Quantity })
                                           .ToDataTable("Series");
            }
        }

        #endregion

        
        #region Positions Chart

        public static DataSet GetAccountPositions(int accountId)
        {
            using (IDalSession session = NHSessionFactory.CreateSession())
            {
                return getOwnedFundPositions(session, accountId)
                            .Select(p => new { InstrumentId = p.InstrumentOfPosition.Key, p.InstrumentDescription })
                            .ToDataSet();
            }
        }

        public static int[] GetHighestValueInstrumentIds(int accountId, int count)
        {
            using (IDalSession session = NHSessionFactory.CreateSession())
            {
                return (from p in getOwnedFundPositions(session, accountId)
                        orderby p.CurrentBaseValue.Quantity descending
                        select p)
                       .Take(count)
                       .Select(p => p.InstrumentOfPosition.Key)
                       .ToArray();
            }
        }

        public static List<SeriesInfo> GetPositionSeriesInfoList(int accountId)
        {
            using (IDalSession session = NHSessionFactory.CreateSession())
            {
                // StartDate and EndDate will be retrieved later (for selected positions only) by RetrieveStartEndDates()
                return getOwnedFundPositions(session, accountId)
                            .ConvertAll(p => new SeriesInfo(p.InstrumentOfPosition.Key, p.InstrumentOfPosition.DisplayName, 
                                                            DateTime.Today, DateTime.Today));
            }
        }

        public static void RetrieveStartEndDates(List<SeriesInfo> positionSeriesInfos, int accountId)
        {
            using (IDalSession session = NHSessionFactory.CreateSession())
            {
                if (positionSeriesInfos.Count() > 0)
                {
                    List<IFundPosition> relatedPositions = SecurityLayerAdapter.GetOwnedFundPositionsByParentInstrument(session, 
                                                                  accountId, positionSeriesInfos.Select(ps => ps.Key).ToArray(), PositionsView.All);

                    foreach (SeriesInfo positionSeriesInfo in positionSeriesInfos)
                    {
                        var currentInstrRelatedPositions = relatedPositions.Where(p =>
                                p.Size.Underlying.TopParentInstrument.Key == positionSeriesInfo.Key);

                        if (currentInstrRelatedPositions.Count() > 0)
                        {
                            positionSeriesInfo.StartDate = currentInstrRelatedPositions.Min(p => p.OpenDate.Date);
                            positionSeriesInfo.EndDate = currentInstrRelatedPositions.Max(p => p.LastValuation != null ? p.LastValuation.Date.Date : DateTime.Today);
                        }
                    }
                }
            }
        }

        public static DataTable GetPositionValuations(int accountId, int instrumentId, DateTime[] dates)
        {
            using (IDalSession session = NHSessionFactory.CreateSession())
            {
                // Get valuations by parent instrument
                List<IValuation> valuations = dates.Length > 0 ?
                                              SecurityLayerAdapter.GetOwnedValuations(session, accountId, instrumentId, dates, true, true) :
                                              new List<IValuation>();

                return (from v in valuations
                        where v.BaseMarketValue.Quantity != 0m
                        orderby v.Date
                        group v by v.Date into grp
                        select new { Date = grp.Key, BaseMarketValueQuantity = grp.Sum(val => val.BaseMarketValue.Quantity) })
                        .ToDataTable("Series");
            }
        }

        #endregion


        #region Allocation by Instrument Chart

        public static DataSet GetAllocationByInstrument(int accountId)
        {
            using (IDalSession session = NHSessionFactory.CreateSession())
            {
                return getOwnedFundPositions(session, accountId)
                            .Select(p => new { InstrumentName = p.InstrumentOfPosition.Name,
                                               CurrentBaseValueQuantity = p.CurrentBaseValue.Quantity })
                            .ToDataSet();
            }
        }

        #endregion

        private static List<IFundPosition> getOwnedFundPositions(IDalSession session, int accountId)
        {
            return SecurityLayerAdapter.GetOwnedFundPositions(session, accountId, PositionsView.NotZero)
                                       .OrderBy(p => p.InstrumentOfPosition.DisplayName)
                                       .ToList();
        }
    }
}
