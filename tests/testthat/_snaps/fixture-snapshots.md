# fixture output: demo

    Code
      print(run())
    Output
                   method intensity lactate heart_rate
      1           Log-log        78    1.20        136
      2          OBLA 2.0       107    2.00        158
      3          OBLA 2.5       120    2.50        167
      4          OBLA 3.0       130    3.00        172
      5          OBLA 3.5       138    3.50        176
      6          OBLA 4.0       146    4.00        180
      7        Bsln + 0.5        85    1.43        142
      8        Bsln + 1.0       105    1.93        157
      9        Bsln + 1.5       118    2.43        166
      10             Dmax       132    3.10        173
      11          ModDmax       140    3.60        177
      12         Exp-Dmax       135    3.30        175
      13 Log-Poly-ModDmax       149    4.20        182
      14  Log-Exp-ModDmax       152    4.40        183
      15             LTP1        89    1.30        145
      16             LTP2       157    4.50        185
      17          LTratio        71    1.20        131
      18              IAT       122    2.70        168

# fixture output: noisy

    Code
      print(run())
    Output
                   method intensity lactate heart_rate
      1           Log-log        78    1.30        136
      2          OBLA 2.0       105    2.00        157
      3          OBLA 2.5       119    2.50        167
      4          OBLA 3.0       130    3.00        173
      5          OBLA 3.5       138    3.50        177
      6          OBLA 4.0       146    4.00        181
      7        Bsln + 0.5        81    1.43        138
      8        Bsln + 1.0       103    1.93        155
      9        Bsln + 1.5       117    2.43        165
      10             Dmax       132    3.10        174
      11          ModDmax       141    3.60        178
      12         Exp-Dmax       135    3.30        176
      13 Log-Poly-ModDmax       158    4.90        187
      14  Log-Exp-ModDmax       159    5.00        187
      15             LTP1       114    2.00        163
      16             LTP2       168    5.50        191
      17          LTratio        74    1.30        133
      18              IAT       125    2.80        171

# fixture output: plateau

    Code
      print(run())
    Output
                   method intensity lactate
      1           Log-log       148     1.3
      2          OBLA 2.0       189     2.0
      3          OBLA 2.5       210     2.5
      4          OBLA 3.0       229     3.0
      5          OBLA 3.5       247     3.5
      6          OBLA 4.0       264     4.0
      7        Bsln + 0.5       165     1.5
      8        Bsln + 1.0       189     2.0
      9        Bsln + 1.5       210     2.5
      10             Dmax       176     1.5
      11          ModDmax       188     1.8
      12         Exp-Dmax       256     3.7
      13 Log-Poly-ModDmax       194     1.9
      14  Log-Exp-ModDmax       244     3.4
      15             LTP1       183     1.4
      16             LTP2       335     6.2
      17          LTratio       156     1.4
      18              IAT       235     2.9

# fixture output: plateau_x3

    Code
      print(run())
    Output
                   method intensity lactate
      1           Log-log       148     3.8
      2          OBLA 2.0        NA     2.0
      3          OBLA 2.5       113     2.5
      4          OBLA 3.0       133     3.0
      5          OBLA 3.5       146     3.5
      6          OBLA 4.0       156     4.0
      7        Bsln + 0.5       146     3.5
      8        Bsln + 1.0       156     4.0
      9        Bsln + 1.5       165     4.5
      10             Dmax       176     4.6
      11          ModDmax       176     4.6
      12         Exp-Dmax       256    11.1
      13 Log-Poly-ModDmax       194     5.7
      14  Log-Exp-ModDmax       244    10.3
      15             LTP1       183     4.1
      16             LTP2       335    18.7
      17          LTratio       156     4.1
      18              IAT       195     5.6

# fixture output: early_dip

    Code
      print(run())
    Output
                   method intensity lactate
      1           Log-log       208     1.4
      2          OBLA 2.0       244     2.0
      3          OBLA 2.5       264     2.5
      4          OBLA 3.0       278     3.0
      5          OBLA 3.5       290     3.5
      6          OBLA 4.0       300     4.0
      7        Bsln + 0.5       210     1.5
      8        Bsln + 1.0       244     2.0
      9        Bsln + 1.5       264     2.5
      10             Dmax       244     2.0
      11          ModDmax       280     3.1
      12         Exp-Dmax       265     2.6
      13 Log-Poly-ModDmax       281     3.2
      14  Log-Exp-ModDmax       290     3.4
      15             LTP1       200     1.4
      16             LTP2       281     2.7
      17          LTratio       209     1.4
      18              IAT       275     2.9

# fixture output: no_rest

    Code
      print(run())
    Output
                   method intensity lactate heart_rate
      1           Log-log       101    1.80        154
      2          OBLA 2.0       106    2.00        158
      3          OBLA 2.5       119    2.50        166
      4          OBLA 3.0       130    3.00        172
      5          OBLA 3.5       139    3.50        177
      6          OBLA 4.0       146    4.00        180
      7        Bsln + 0.5        85    1.48        142
      8        Bsln + 1.0       105    1.98        157
      9        Bsln + 1.5       119    2.48        166
      10             Dmax       132    3.10        173
      11          ModDmax       140    3.60        177
      12         Exp-Dmax       135    3.30        175
      13 Log-Poly-ModDmax       149    4.20        182
      14  Log-Exp-ModDmax       152    4.40        183
      15             LTP1       111    2.00        161
      16             LTP2       160    4.70        186
      17          LTratio        77    1.30        136
      18              IAT       125    2.80        170

# fixture output: negative_rest

    Code
      print(run())
    Output
                   method intensity lactate
      1           Log-log       140     1.5
      2          OBLA 2.0       168     2.0
      3          OBLA 2.5       190     2.5
      4          OBLA 3.0       207     3.0
      5          OBLA 3.5       222     3.5
      6          OBLA 4.0       235     4.0
      7        Bsln + 0.5       150     1.7
      8        Bsln + 1.0       177     2.2
      9        Bsln + 1.5       197     2.7
      10             Dmax       185     2.4
      11          ModDmax       201     2.8
      12         Exp-Dmax       195     2.7
      13 Log-Poly-ModDmax       221     3.4
      14  Log-Exp-ModDmax       226     3.6
      15             LTP1       116     1.2
      16             LTP2       219     3.0
      17          LTratio       144     1.7
      18              IAT       215     3.2

# fixture output: running

    Code
      print(run())
    Output
                   method intensity  pace lactate heart_rate
      1           Log-log      10.2 353.9     1.2        143
      2          OBLA 2.0      12.3 292.5     2.0        158
      3          OBLA 2.5      13.2 273.6     2.5        164
      4          OBLA 3.0      13.8 260.4     3.0        169
      5          OBLA 3.5      14.4 250.2     3.5        172
      6          OBLA 4.0      14.9 242.0     4.0        175
      7        Bsln + 0.5      11.4 316.7     1.6        152
      8        Bsln + 1.0      12.5 288.0     2.1        159
      9        Bsln + 1.5      13.3 270.6     2.6        165
      10             Dmax      13.8 261.1     3.0        169
      11          ModDmax      14.4 249.5     3.5        173
      12         Exp-Dmax      14.1 255.2     3.3        171
      13 Log-Poly-ModDmax      15.2 236.4     4.4        177
      14  Log-Exp-ModDmax      15.4 233.6     4.5        178
      15             LTP1      11.0 328.2     1.3        149
      16             LTP2      15.0 239.3     3.7        176
      17          LTratio       9.0 400.1     1.1        135
      18              IAT      13.3 270.3     2.6        165

# fixture output: swimming

    Code
      print(run())
    Output
                   method intensity  pace lactate
      1           Log-log      1.19  84.2     1.8
      2          OBLA 2.0      1.20  83.4     2.0
      3          OBLA 2.5      1.25  80.2     2.5
      4          OBLA 3.0      1.28  77.9     3.0
      5          OBLA 3.5      1.31  76.1     3.5
      6          OBLA 4.0      1.34  74.6     4.0
      7        Bsln + 0.5      1.12  89.1     1.5
      8        Bsln + 1.0      1.20  83.4     2.0
      9        Bsln + 1.5      1.25  80.2     2.5
      10             Dmax      1.28  78.1     3.0
      11          ModDmax      1.32  75.9     3.6
      12         Exp-Dmax      1.30  76.7     3.4
      13 Log-Poly-ModDmax      1.36  73.7     4.4
      14  Log-Exp-ModDmax      1.37  73.1     4.5
      15             LTP1      1.16  86.2     1.5
      16             LTP2      1.35  74.2     3.8
      17          LTratio      1.00 100.0     1.2
      18              IAT      1.27  79.0     2.7

