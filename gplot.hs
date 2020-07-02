import Graphics.Gnuplot.Simple
l = Points
p = plotListStyle [XRange ((-1),8), YRange((-1),8)] (PlotStyle Impulses (DefaultStyle 1)) [1..5]
