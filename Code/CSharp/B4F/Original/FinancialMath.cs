using System;
using System.Linq;

namespace B4F.TotalGiro.ClientApplicationLayer.Common
{
    public static class FinancialMath
    {
        #region System.Decimal Variants

        public static decimal InternalRateOfReturn(decimal[] values)
        {
            return (decimal)InternalRateOfReturn(values.Select(v => (double)v).ToArray());
        }

        public static decimal CumulativeNormalDistribution(decimal x, decimal mean, decimal standardDeviation)
        {
            return (decimal)CumulativeNormalDistribution((double)x, (double)mean, (double)standardDeviation);
        }

        public static decimal ErrorFunction(decimal x)
        {
            return (decimal)ErrorFunction((double)x);
        }

        public static decimal GoalSeek(Func<decimal, decimal> func, decimal goalY, decimal guessX)
        {
            return (decimal)GoalSeek((double x) => (double)func((decimal)x), (double)goalY, (double)guessX);
        }

        public static decimal Sqrt(decimal x)
        {
            return (decimal)Math.Sqrt((double)x);
        }

        #endregion


        #region System.Double Variants

        public static double InternalRateOfReturn(double[] values)
        {
            double[] valuesCopy = values.ToArray();
            double irrGuess = 0.06;

            while (true)
            {
                try
                {
                    return Microsoft.VisualBasic.Financial.IRR(ref valuesCopy, irrGuess);
                }
                catch (ArgumentException)
                {
                    irrGuess -= 0.02;
                    if (irrGuess < -1.0)
                        throw new ApplicationException("Internal Rate of Return (IRR) could not be calculated.");
                }
            }
        }

        public static double CumulativeNormalDistribution(double x, double mean, double standardDeviation)
        {
            return 0.5 * (1.0 + ErrorFunction((x - mean) / (standardDeviation * Math.Sqrt(2.0))));
        }

        /// <summary>
        /// Error function (erf). The maximum error is below 1.5e-7.
        /// Adapted from Python code at http://www.johndcook.com/blog/2009/01/19/stand-alone-error-function-erf.
        /// </summary>
        public static double ErrorFunction(double x)
        {
            // Constants
            const double a1 = 0.254829592,
                         a2 = -0.284496736,
                         a3 = 1.421413741,
                         a4 = -1.453152027,
                         a5 = 1.061405429,
                         p = 0.3275911;

            double signX = (x >= 0.0) ? 1d : -1d;
            x = Math.Abs(x);

            // Abramowitz & Stegun (ISBN 0486612724), formula 7.1.26
            double t = 1.0 / (1.0 + p * x);
            double y = 1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * Math.Exp(-x * x);

            return signX * y;
        }

        /// <summary>
        /// Simple solver implementing a greedy algorithm.
        /// It assumes 'func' is strictly monotonous over its entire domain.
        /// </summary>
        public static double GoalSeek(Func<double, double> func, double goalY, double guessX)
        {
            double aX = guessX,
                   aY = func(aX);
            double bX, bY;

            if (IsZero(aY - goalY))
                return aX;

            double deltaX = Math.Abs(guessX / 4);
            if (IsZero(deltaX))
                deltaX = 0.25;

            // get aY and bY to be on each side of goalY
            int cycles = 0;
            while (true)
            {
                cycles = incCycles(cycles);

                bX = aX + deltaX;
                bY = func(bX);

                if (IsZero(bY - goalY))
                    return bX;

                if (IsZero(bY - aY))
                {
                    deltaX *= 2;
                    continue;
                }

                if ((aY - goalY) * (bY - goalY) < 0)
                    // goalY is between aY and bY
                    break;

                if (Math.Abs(aY - goalY) < Math.Abs(bY - goalY))
                    // we are getting farther from goalY
                    deltaX *= -1;
                else
                    deltaX *= 2;
            }

            while (true)
            {
                cycles = incCycles(cycles);

                double cX = (aX + bX) / 2,
                       cY = func(cX);

                if (IsZero(cY - goalY))
                    return cX;

                if ((aY - goalY) * (cY - goalY) < 0)
                {
                    // goalY is between aY and cY
                    bX = cX;
                    bY = cY;
                }
                else
                {
                    // goalY is between cY and bY
                    aX = cX;
                    aY = cY;
                }
            }
        }

        private static int incCycles(int cycles)
        {
            if (cycles >= maxCycles)
                throw new ApplicationException(string.Format("GoalSeek could not find a solution in {0} cycles.", maxCycles));

            return cycles + 1;
        }
        private const int maxCycles = 2000;

        public static bool IsZero(double x)
        {
            return IsZero(x, 1e-3);
        }

        public static bool IsZero(double x, double epsilon)
        {
            return Math.Abs(x) < epsilon;
        }

        #endregion
    }
}
