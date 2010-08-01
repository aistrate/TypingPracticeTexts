using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Web.UI;
using System.Web.UI.WebControls;
using B4F.TotalGiro.ClientApplicationLayer.Common;
using B4F.TotalGiro.ClientApplicationLayer.Planning;
using B4F.TotalGiro.Stichting.Login;
using B4F.TotalGiro.Utils.Tuple;
using B4F.Web.WebControls;
using Dundas.Charting.WebControl;

public partial class FinancialPlanner : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        pnlErrorFutureValue.Visible = false;
        elbGivenFields.Text = "";
        elbFutureValue.Text = "";

        Utility.EnableScrollToBottom(this, hdnScrollToBottom);

        try
        {
            if (!IsPostBack)
            {
                ((TotalGiroClient)Master).HeaderText = "Monitor Portefeuille";

                ctlPortfolioNavigationBar.Visible = Initialize && CurrentLoginType != LoginTypes.Customer;

                initializeGivenFields();
            }
        }
        catch (Exception ex) { displayError(ex); }
    }

    // Behavior toggles
    protected bool AccountList_HasEmptyLine { get { return true; } }
    protected bool ModelList_HasEmptyLine { get { return CurrentLoginType != LoginTypes.Customer; } }

    protected bool ModelMissingExpectedReturn_IsAllowed { get { return true; } }

    
    protected void Page_Init(object sender, EventArgs e)
    {
        odsAccount.SelectParameters["hasEmptyFirstRow"].DefaultValue = AccountList_HasEmptyLine.ToString();
        odsModelPortfolio.SelectParameters["hasEmptyFirstRow"].DefaultValue = ModelList_HasEmptyLine.ToString();

        Utility.AddParameterContactId(odsAccount, CurrentLoginType == LoginTypes.Customer);
    }

    private void initializeGivenFields()
    {
        txtYearsLeft.Text = "20";
        txtTargetValue.Text = "300000";

        if (Initialize || CurrentLoginType == LoginTypes.Customer)
        {
            pnlAccountList.Visible = true;
            ddlAccount.DataBind();

            selectFirstNonEmptyLine(ddlAccount, AccountList_HasEmptyLine);

            retrieveAccountDependant();
        }

        FocusCandidates.FocusOnFirstVisible();
    }

    private void selectFirstNonEmptyLine(DropDownList dropDownList, bool hasEmptyLine)
    {
        int emptyCount = hasEmptyLine ? 1 : 0;
        
        if (dropDownList.Items.Count > emptyCount)
            dropDownList.SelectedIndex = emptyCount;
    }

    protected void ddlAccount_SelectedIndexChanged(object sender, EventArgs e)
    {
        try
        {
            resetPanelFutureValue(false);
            retrieveAccountDependant();
            FocusCandidates.FocusOnFirstVisible();
        }
        catch (Exception ex) { displayError(ex); }
    }

    protected void ddlModelPortfolio_SelectedIndexChanged(object sender, EventArgs e)
    {
        try
        {
            resetPanelFutureValue(false);
            retrieveModelDependant();
            FocusCandidates.FocusOnFirstVisible();
        }
        catch (Exception ex) { displayError(ex); }
    }

    private void resetPanelFutureValue(bool visible)
    {
        pnlFutureValue.Visible = visible;

        btnCalculate.Text = visible ? "Recalculate" : "Calculate";

        ExtrasSelectedIndex = 0;
        ExtraPeriodical = 0m;
        ExtraInitial = 0m;
        IsSliderReset = true;
    }

    protected bool IsSliderReset
    {
        get
        {
            object b = ViewState["IsSliderReset"];
            return (b == null ? true : (bool)b);
        }
        set { ViewState["IsSliderReset"] = value; }
    }

    private void retrieveAccountDependant()
    {
        if (FinancialDataView != null)
        {
            mvwModelPortfolio.ActiveViewIndex = 1;
            lblModelName.Text = FinancialDataView.ModelName;

            mvwPresentValue.ActiveViewIndex = 1;
            txtPresentValue.Text = "";
            lblPresentValue.Text = FinancialDataView.TotalPositionDisplayString;
        }
        else
        {
            mvwModelPortfolio.ActiveViewIndex = 0;
            mvwModelPortfolio.DataBind();

            mvwPresentValue.ActiveViewIndex = 0;
        }
        
        retrieveModelDependant();
    }

    private void retrieveModelDependant()
    {
        if (FinancialDataView != null && FinancialDataView.ExpectedReturn != 0m)
        {
            // Labels
            mvwInterestRatePerYear.ActiveViewIndex = 1;
            txtInterestRatePerYear.Text = "";
            lblInterestRatePerYear.Text = string.Format("{0:0.0##} %", FinancialDataView.ExpectedReturn * 100m);

            pnlStandardDeviation.Visible = false;
            txtStandardDeviation.Text = "";
        }
        else
        {
            // TextBoxes
            mvwInterestRatePerYear.ActiveViewIndex = 0;
            pnlStandardDeviation.Visible = true;
        }
    }

    protected void txtExtra_TextChanged(object sender, EventArgs e)
    {
        try
        {
            displayChanceOfMeetingTarget();

            if (ExtrasSelectedIndex == 1)
                displayFutureValueAfterAdjust();

            chPortfolioFutureValueDataBind();
        }
        catch (Exception ex) { displayError(ex); }
    }

    protected void lnkChoiceOfExtras_Command(object sender, CommandEventArgs e)
    {
        try
        {
            ExtrasSelectedIndex = 1 - ExtrasSelectedIndex;

            displayChanceOfMeetingTarget();
            displayExtras();
            
            chPortfolioFutureValueDataBind();
        }
        catch (Exception ex) { displayError(ex); }
    }

    protected void btnCalculate_Click(object sender, EventArgs e)
    {
        try
        {
            resetPanelFutureValue(true);

            displayFutureValueBeforeAdjust();
            displayChanceOfMeetingTarget();
            displayExtras();
            
            chPortfolioFutureValueDataBind();

            Utility.ScrollToBottom(hdnScrollToBottom);
        }
        catch (Exception ex) { displayError(ex); }
    }

    private void displayFutureValueBeforeAdjust()
    {
        lblFutureValueBeforeAdjust.Text = FinancialPlannerAdapter.GetFutureValueDisplayString(
                                                                InterestRatePerYear, PresentValue, DepositPerYear, YearsLeft);
        lblEndOfPeriod.Text = DateTime.Today.AddYears(YearsLeft).ToString("MMMM yyyy");
    }

    private void displayChanceOfMeetingTarget()
    {
        lblChanceOfMeetingTarget.Text = string.Format("{0:0.0#} %", InvestmentScenario.ChanceOfMeetingTarget * 100m);
        
        pnlTrafficLight.Visible = true;
        TrafficLightValue = InvestmentScenario.ChanceOfMeetingTarget;
    }

    private void displayExtras()
    {
        switch (ExtrasSelectedIndex)
        {
            case 0:
                lblProposedPeriodical.Text = FinancialPlannerAdapter.FormatCurrency(ProposedPeriodical);
                lblProposedInitial.Text = FinancialPlannerAdapter.FormatCurrency(ProposedInitial);
                break;

            case 1:
                decimal proposedPeriodical = Math.Ceiling(ProposedPeriodical);
                decimal proposedInitial = Math.Ceiling(ProposedInitial);

                lblExtraPeriodicalMax.Text = proposedPeriodical.ToString("#,##0");
                lblExtraInitialMax.Text = proposedInitial.ToString("#,##0");
                sldExtraPeriodical.Maximum = (double)proposedPeriodical;
                sldExtraInitial.Maximum = (double)proposedInitial;

                if (IsSliderReset)
                {
                    //ExtraPeriodical = proposedPeriodical;
                    IsSliderReset = false;
                }
                displayFutureValueAfterAdjust();
                break;
        }
    }

    private void displayFutureValueAfterAdjust()
    {
        lblFutureValueAfterAdjust.Text = FinancialPlannerAdapter.GetFutureValueDisplayString(
                                                    InterestRatePerYear,
                                                    PresentValue + ExtraInitial,
                                                    DepositPerYear + ExtraPeriodical,
                                                    YearsLeft);
        lblTargetValueExtras.Text = FinancialPlannerAdapter.FormatCurrency(TargetValue);
    }

    private void chPortfolioFutureValueDataBind()
    {
        if (!isChartDataBound)
        {
            isChartDataBound = true;

            decimal[] cashFlow = Enumerable.Repeat(100m, 1)
                         .Concat(Enumerable.Repeat(0m, 5))
                         .Concat(Enumerable.Repeat(40m, 20))
                         .Concat(Enumerable.Repeat(0m, 4))
                         .Concat(Enumerable.Repeat(-1000m, 1))
                         .Concat(Enumerable.Repeat(-300m, 13))
                         .ToArray();

            DateTime[] futureDates = FinancialPlannerAdapter.GetRangeOfDates(YearsLeft);

            var uncertaintyValues = new decimal[]
                                    {
                                        InterestRatePerYear * 0.85m,
                                        InterestRatePerYear * 1.15m
                                    }
                                    .Select(interestRate => FinancialPlannerAdapter.GetFutureValueSeries(
                                                                YearsLeft, interestRate, PresentValue, DepositPerYear))
                                    .ToArray();
            
            // TODO: translate
            addSeries(futureDates, uncertaintyValues, "Market uncertainty", SeriesChartType.SplineRange, Color.FromArgb(0xEC, 0xEC, 0xEC),
                      MarkerStyle.None);

            if (TargetValue > 0)
            {
                DateTime[] futureDates2 = FinancialPlannerAdapter.GetRangeOfDates(YearsLeft + 1);

                var targetValues = FinancialPlannerAdapter.GetFutureValueSeries(YearsLeft + 1, InvestmentScenario.InternalRateOfReturn, PresentValue,
                                                                                DepositPerYear);
                addSeries(futureDates2, targetValues, "Doelvermogen", SeriesChartType.Spline, Color.FromArgb(0xE0, 0x40, 0x0A), 
                          MarkerStyle.Circle);

                //displayError(InvestmentScenario.InternalRateOfReturn.ToString("p"));
                //displayError(targetValues.Last().ToString("c"));
            }

            var futureValuesBeforeAdjust = FinancialPlannerAdapter.GetFutureValueSeries(YearsLeft, InterestRatePerYear, PresentValue, 
                                                                                        DepositPerYear);
            addSeries(futureDates, futureValuesBeforeAdjust, "Zonder aanpassing", SeriesChartType.Spline, Color.FromArgb(0x41, 0x8C, 0xF0),
                      MarkerStyle.Circle);

            if (ExtrasSelectedIndex == 1 && (ExtraPeriodical > 0m || ExtraInitial > 0m))
            {
                var futureValuesAfterAdjust = FinancialPlannerAdapter.GetFutureValueSeries(
                                                    YearsLeft, InterestRatePerYear, PresentValue + ExtraInitial, 
                                                    DepositPerYear + ExtraPeriodical);
                addSeries(futureDates, futureValuesAfterAdjust, "Met aanpassing", SeriesChartType.Spline, Color.FromArgb(0x1A, 0x3B, 0x69),
                          MarkerStyle.Circle);
            }

            if (SelectedAccountId != 0)
            {
                DateTime[] valuationDates = FinancialPlannerAdapter.GetValuationDates(SelectedAccountId, YearsLeft);

                if (valuationDates.Length > 0)
                {
                    List<Tuple<DateTime, decimal>> valuations = FinancialPlannerAdapter.GetValuationsTotalPortfolio(SelectedAccountId,
                                                                                                                    valuationDates);
                    valuations.Add(Tuple.Create(DateTime.Today, PresentValue));

                    // TODO: translate
                    addSeries(valuations.Select(v => v.Item1).ToArray(),
                              valuations.Select(v => v.Item2).ToArray(),
                              "History", SeriesChartType.Spline, Color.FromArgb(0xFC, 0xB4, 0x41), MarkerStyle.None);
                }
            }

            chPortfolioFutureValue.ChartAreas[0].AxisX.LabelStyle.Format = "MMM\nyyyy";
            chPortfolioFutureValue.ChartAreas[0].AxisX.IntervalAutoMode = IntervalAutoMode.VariableCount;
            chPortfolioFutureValue.ChartAreas[0].AxisX.LabelStyle.Interval = 1300;
            
            // Color.FromArgb(0x1A, 0x3B, 0x69) = dark blue (~= Navy)
            // Color.FromArgb(0x31, 0x34, 0x57) = dark blue (Paerel house style)
            // Color.FromArgb(0x41, 0x8C, 0xF0) = light blue (~= CornflowerBlue)
            // Color.FromArgb(0xFC, 0xB4, 0x41) = yellow
            // Color.FromArgb(0xD0, 0xD0, 0xD0) = light gray
        }
    }
    private bool isChartDataBound = false;

    private Series addSeries(IEnumerable<DateTime> dates, IEnumerable<decimal> valueList,
                             string seriesName, SeriesChartType chartType, Color color, MarkerStyle markerStyle)
    {
        return addSeries(dates, new IEnumerable<decimal>[] { valueList }, seriesName, chartType, color, markerStyle);
    }

    private Series addSeries(IEnumerable<DateTime> dates, IEnumerable<decimal>[] valueLists,
                             string seriesName, SeriesChartType chartType, Color color, MarkerStyle markerStyle)
    {
        Series series = new Series(seriesName);
        chPortfolioFutureValue.Series.Add(series);
       
        series.LegendText = seriesName;
        series.XValueType = ChartValueTypes.DateTime;
        series.EmptyPointStyle.Color = Color.Transparent;
        series.BorderWidth = 2;
        series.Color = color;
        series.MarkerStyle = markerStyle;

        if (dates.Count() == 1 && chartType != SeriesChartType.SplineRange)
        {
            series.Type = SeriesChartType.Point;
            series.MarkerSize = 8;
        }
        else
            series.Type = chartType;

        series.Points.DataBindXY(dates, valueLists);

        return series;
    }

    protected decimal TrafficLightValue
    {
        set { imgTrafficLight.ImageUrl = string.Format("~/Images/TrafficLight/Set2/{0}-light.jpg",
                                                       CommonAdapter.GetTrafficLightColor(value).Name.ToLower()); }
    }

    protected int ExtrasSelectedIndex
    {
        get { return mvwExtras.ActiveViewIndex; }
        set
        {
            int newIndex = value,
                oldIndex = 1 - newIndex;

            string[] choiceLabels = new string[] { "Voorstel aanpassing",
                                                   "Aanpassingen combineren" };

            lblChoiceOfExtras.Text = choiceLabels[newIndex] + ":";
            lnkChoiceOfExtras.Text = choiceLabels[oldIndex];

            mvwExtras.ActiveViewIndex = newIndex;
        }
    }

    protected Control[] FocusCandidates
    {
        get { return new Control[] { txtPresentValue, txtInterestRatePerYear, txtTargetValue }; }
    }

    protected int SelectedAccountId
    {
        get
        {
            return pnlAccountList.Visible && ddlAccount.SelectedValue != int.MinValue.ToString()
                      ? int.Parse(ddlAccount.SelectedValue)
                      : 0;
        }
    }

    protected int SelectedModelId
    {
        get
        {
            return ddlModelPortfolio.Visible && ddlModelPortfolio.SelectedValue != int.MinValue.ToString()
                      ? int.Parse(ddlModelPortfolio.SelectedValue)
                      : 0;
        }
    }

    protected FinancialDataView FinancialDataView
    {
        get
        {
            if (financialDataView == null)
            {
                if (SelectedAccountId != 0)
                    financialDataView = FinancialPlannerAdapter.GetFinancialDataFromAccount(SelectedAccountId, ModelMissingExpectedReturn_IsAllowed);
                else if (SelectedModelId != 0)
                    financialDataView = FinancialPlannerAdapter.GetFinancialDataFromModel(SelectedModelId, ModelMissingExpectedReturn_IsAllowed);
            }

            return financialDataView;
        }
    }
    private FinancialDataView financialDataView;

    protected decimal InterestRatePerYear
    {
        get { return mvwInterestRatePerYear.ActiveViewIndex == 0 ? readDecimal(txtInterestRatePerYear, 3) / 100m : FinancialDataView.ExpectedReturn; }
    }

    protected decimal StandardDeviation
    {
        get { return pnlStandardDeviation.Visible ? readDecimal(txtStandardDeviation, 3) / 100m : FinancialDataView.StandardDeviation; }
    }

    protected decimal PresentValue
    {
        get { return mvwPresentValue.ActiveViewIndex == 0 ? readDecimal(txtPresentValue) : FinancialDataView.TotalPositionQuantity; }
    }

    protected decimal TargetValue { get { return readDecimal(txtTargetValue); } }
    protected int YearsLeft { get { return readInt(txtYearsLeft); } }
    protected decimal DepositPerYear { get { return readDecimal(txtDepositPerYear); } }

    protected decimal ExtraPeriodical
    {
        get { return ExtrasSelectedIndex == 1 ? readInt(txtExtraPeriodical_BoundControl) : 0m; }
        set
        {
            string formattedValue = value.ToString("###0");
            txtExtraPeriodical.Text = formattedValue;
            txtExtraPeriodical_BoundControl.Text = formattedValue;
        }
    }
    
    protected decimal ExtraInitial
    {
        get { return ExtrasSelectedIndex == 1 ? readInt(txtExtraInitial_BoundControl) : 0m; }
        set
        {
            string formattedValue = value.ToString("###0");
            txtExtraInitial.Text = formattedValue;
            txtExtraInitial_BoundControl.Text = formattedValue;
        }
    }

    protected decimal TotalPresentValue
    {
        get { return ExtrasSelectedIndex == 0 ? PresentValue : (PresentValue + ExtraInitial); }
    }

    protected decimal TotalDepositPerYear
    {
        get { return ExtrasSelectedIndex == 0 ? DepositPerYear : (DepositPerYear + ExtraPeriodical); }
    }

    protected decimal ProposedPeriodical
    {
        get
        {
            decimal proposedPeriodical = FinancialPlannerAdapter.GetProposedPeriodical(
                                            InterestRatePerYear, PresentValue, DepositPerYear, YearsLeft, TargetValue);

            return Math.Max(0, proposedPeriodical - DepositPerYear);
        }
    }

    protected decimal ProposedInitial
    {
        get
        {
            decimal proposedInitial = FinancialPlannerAdapter.GetProposedInitial(
                                            InterestRatePerYear, PresentValue, DepositPerYear, YearsLeft, TargetValue);

            return Math.Max(0, proposedInitial - PresentValue);
        }
    }

    protected InvestmentScenario InvestmentScenario
    {
        get
        {
            if (investmentScenario == null)
                investmentScenario = new InvestmentScenario(TotalPresentValue, TotalDepositPerYear, YearsLeft, TargetValue, 
                                                            InterestRatePerYear, StandardDeviation);

            return investmentScenario;
        }
    }
    private InvestmentScenario investmentScenario;

    private decimal readDecimal(TextBox textBox)
    {
        return readDecimal(textBox, 2);
    }

    private decimal readDecimal(TextBox textBox, int decimals)
    {
        decimal d = 0m;
        decimal.TryParse(textBox.Text, out d);
        d = Math.Round(Math.Abs(d), decimals);

        textBox.Text = d.ToString();
        return d;
    }

    private int readInt(TextBox textBox)
    {
        int i = (int)readDecimal(textBox, 0);
        textBox.Text = i.ToString();
        return i;
    }

    protected bool Initialize
    {
        get { return Utility.GetQueryParameters().GetBoolValue("initialize", false); }
    }

    protected LoginTypes CurrentLoginType
    {
        get
        {
            if (currentLoginType == null)
                currentLoginType = CommonAdapter.GetCurrentLoginType();
            return (LoginTypes)currentLoginType;
        }
    }
    private LoginTypes? currentLoginType = null;

    private void displayError(Exception ex)
    {
        displayError(Utility.GetCompleteExceptionMessage(ex));
    }

    private void displayError(string errorMessage)
    {
        ErrorLabel errorLabel = pnlFutureValue.Visible ? elbFutureValue : elbGivenFields;

        pnlErrorFutureValue.Visible = true;
        errorLabel.Text += (errorLabel.Text != "" ? "<br />" : "ERROR: ") +
                            errorMessage;
    }
}
