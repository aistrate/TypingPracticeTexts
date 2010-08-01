using System;
using System.Collections.Generic;
using System.Linq;
using B4F.TotalGiro.ClientApplicationLayer.Common;

namespace B4F.TotalGiro.ClientApplicationLayer.Planning
{
    /// <summary>
    /// Calculated properties are calculated only once, then cached.
    /// </summary>
    public class InvestmentScenario
    {
        public InvestmentScenario(decimal totalPresentValue, decimal totalDepositPerYear, int yearsLeft, decimal targetValue,
                          decimal expectedReturn, decimal standardDeviation)
            : this(null, expectedReturn, standardDeviation)
        {
            CashFlow = (new decimal[] { totalPresentValue })
                .Concat(Enumerable.Repeat(totalDepositPerYear, yearsLeft))
                .Concat(new decimal[] { -targetValue })
                .ToArray();
        }

        public InvestmentScenario(IEnumerable<decimal> cashFlow, decimal expectedReturn, decimal standardDeviation)
        {
            CashFlow = cashFlow != null ? cashFlow.ToArray() : new decimal[] { };
            
            ExpectedReturn = expectedReturn;
            StandardDeviation = standardDeviation;
        }

        public decimal[] CashFlow { get; private set; }
        public decimal ExpectedReturn { get; private set; }
        public decimal StandardDeviation { get; private set; }

        public bool Autocorrelation { get { return true; } }
        public int DepositsPerYear { get { return 12; } }
        public int WithdrawalsPerYear { get { return 12; } }


        public decimal ChanceOfMeetingTarget
        {
            get
            {
                if (chanceOfMeetingTarget == null)
                {
                    if (CashFlow.Where(amount => amount > 0m).Count() == 0)
                        chanceOfMeetingTarget = CashFlow.Where(amount => amount < 0m).Count() == 0
                                                    ? 1m
                                                    : 0m;
                    else
                        chanceOfMeetingTarget = 1m - FinancialMath.CumulativeNormalDistribution(InternalRateOfReturn,
                                                                                                ExpectedReturn, StandardDeviationOfTheMean);
                }

                return (decimal)chanceOfMeetingTarget;
            }
        }

        public decimal InternalRateOfReturn
        {
            get
            {
                if (internalRateOfReturn == null)
                    internalRateOfReturn = FinancialMath.InternalRateOfReturn(CashFlow);

                return (decimal)internalRateOfReturn;
            }
        }

        public decimal StandardDeviationOfTheMean
        {
            get
            {
                if (standardDeviationOfTheMean == null)
                    standardDeviationOfTheMean = (Autocorrelation ? 4m / 3m : 1m)
                                               * StandardDeviation / FinancialMath.Sqrt(EffectivePeriod);

                return (decimal)standardDeviationOfTheMean;
            }
        }

        public decimal EffectivePeriod
        {
            get
            {
                if (effectivePeriod == null)
                {
                    List<decimal> cashFlowList = CashFlow.ToList();
                    int firstDepositIndex = cashFlowList.FindIndex(amount => amount > 0m);
                    int firstWithdrawalIndex = cashFlowList.FindIndex(amount => amount < 0m);

                    int depositCount = CashFlow.Count(amount => amount > 0m);
                    int withdrawalCount = CashFlow.Count(amount => amount < 0m);

                    decimal periodicLumpsumRatio = (decimal)(depositCount - 1) / (decimal)(firstWithdrawalIndex - 1);

                    decimal lumpsumBuildupPeriod = (decimal)(firstWithdrawalIndex - firstDepositIndex);
                    decimal periodicBuildupPeriod = 0.75m * lumpsumBuildupPeriod
                                                  - 0.1m * (decimal)Math.Min(9, DepositsPerYear)
                                                  + 0.75m;

                    decimal buildupPeriod = periodicLumpsumRatio * periodicBuildupPeriod
                                          + (1m - periodicLumpsumRatio) * lumpsumBuildupPeriod;
                    decimal reductionPeriod = 0.75m * (decimal)(withdrawalCount - 1)
                                            + 0.1m * (decimal)Math.Min(9, WithdrawalsPerYear);

                    effectivePeriod = buildupPeriod + reductionPeriod;
                }

                return (decimal)effectivePeriod;
            }
        }

        private decimal? chanceOfMeetingTarget;
        private decimal? internalRateOfReturn;
        private decimal? standardDeviationOfTheMean;
        private decimal? effectivePeriod;
    }
}
