# fixture output: demo

    Code
      print(run())
    Output
                   method intensity lactate heart_rate
      1           Log-log      85.3    1.40        142
      2          OBLA 2.0     106.8    2.00        158
      3          OBLA 2.5     119.7    2.50        167
      4          OBLA 3.0     129.9    3.00        172
      5          OBLA 3.5     138.4    3.50        176
      6          OBLA 4.0     145.8    4.00        180
      7        Bsln + 0.5      84.5    1.43        142
      8        Bsln + 1.0     104.6    1.93        157
      9        Bsln + 1.5     118.1    2.43        166
      10             Dmax     131.7    3.10        173
      11          ModDmax     139.7    3.60        177
      12         Exp-Dmax     134.8    3.30        175
      13 Log-Poly-ModDmax     153.2    4.50        183
      14  Log-Exp-ModDmax     155.4    4.70        184
      15             LTP1      88.9    1.50        145
      16             LTP2     157.3    4.90        185
      17          LTratio      71.2    1.20        131
      18              IAT     122.2    2.70        168

# fixture output: noisy

    Code
      print(run())
    Output
                   method intensity lactate heart_rate
      1           Log-log      86.7    1.50        142
      2          OBLA 2.0     105.3    2.00        157
      3          OBLA 2.5     118.9    2.50        167
      4          OBLA 3.0     129.5    3.00        173
      5          OBLA 3.5     138.3    3.50        177
      6          OBLA 4.0     146.0    4.00        181
      7        Bsln + 0.5      81.2    1.43        138
      8        Bsln + 1.0     103.0    1.93        155
      9        Bsln + 1.5     117.2    2.43        165
      10             Dmax     132.1    3.10        174
      11          ModDmax     139.9    3.60        178
      12         Exp-Dmax     134.8    3.30        176
      13 Log-Poly-ModDmax     154.7    4.60        185
      14  Log-Exp-ModDmax     156.6    4.70        186
      15             LTP1     113.7    2.30        163
      16             LTP2     167.9    5.80        191
      17          LTratio      74.4    1.30        133
      18              IAT     125.0    2.80        171

# fixture output: plateau

    Code
      print(run())
    Output
                   method intensity lactate
      1           Log-log     140.9     1.1
      2          OBLA 2.0     189.2     2.0
      3          OBLA 2.5     209.7     2.5
      4          OBLA 3.0     228.6     3.0
      5          OBLA 3.5     246.5     3.5
      6          OBLA 4.0     264.1     4.0
      7        Bsln + 0.5     165.4     1.5
      8        Bsln + 1.0     189.2     2.0
      9        Bsln + 1.5     209.7     2.5
      10             Dmax     176.0     1.5
      11          ModDmax     190.7     1.8
      12         Exp-Dmax     256.4     3.7
      13 Log-Poly-ModDmax     191.0     1.9
      14  Log-Exp-ModDmax     280.1     4.2
      15             LTP1     182.8     1.9
      16             LTP2     334.8     5.9
      17          LTratio     156.4     1.4
      18              IAT     234.7     2.9

# fixture output: plateau_x3

    Code
      print(run())
    Output
                   method intensity lactate
      1           Log-log     140.9     3.3
      2          OBLA 2.0        NA     2.0
      3          OBLA 2.5     113.1     2.5
      4          OBLA 3.0     132.6     3.0
      5          OBLA 3.5     145.5     3.5
      6          OBLA 4.0     156.1     4.0
      7        Bsln + 0.5     145.5     3.5
      8        Bsln + 1.0     156.1     4.0
      9        Bsln + 1.5     165.4     4.5
      10             Dmax     176.0     4.6
      11          ModDmax     176.0     4.6
      12         Exp-Dmax     256.4    11.1
      13 Log-Poly-ModDmax     191.0     5.6
      14  Log-Exp-ModDmax     280.1    12.7
      15             LTP1     182.8     5.6
      16             LTP2     334.8    17.7
      17          LTratio     156.4     4.1
      18              IAT     194.8     5.6

# fixture output: early_dip

    Code
      print(run())
    Output
                   method intensity lactate
      1           Log-log     224.8     1.7
      2          OBLA 2.0     244.1     2.0
      3          OBLA 2.5     263.5     2.5
      4          OBLA 3.0     278.0     3.0
      5          OBLA 3.5     290.0     3.5
      6          OBLA 4.0     300.2     4.0
      7        Bsln + 0.5     209.8     1.5
      8        Bsln + 1.0     244.1     2.0
      9        Bsln + 1.5     263.5     2.5
      10             Dmax     244.4     2.0
      11          ModDmax     280.3     3.2
      12         Exp-Dmax     265.1     2.6
      13 Log-Poly-ModDmax     287.1     3.5
      14  Log-Exp-ModDmax     295.6     3.6
      15             LTP1     200.0     1.4
      16             LTP2     281.3     3.1
      17          LTratio     209.2     1.4
      18              IAT     275.5     2.9

# fixture output: no_rest

    Code
      print(run())
    Output
                   method intensity lactate heart_rate
      1           Log-log     110.0    2.10        160
      2          OBLA 2.0     105.7    2.00        158
      3          OBLA 2.5     119.3    2.50        166
      4          OBLA 3.0     129.9    3.00        172
      5          OBLA 3.5     138.6    3.50        177
      6          OBLA 4.0     146.1    4.00        180
      7        Bsln + 0.5      85.1    1.48        142
      8        Bsln + 1.0     105.1    1.98        157
      9        Bsln + 1.5     118.8    2.48        166
      10             Dmax     131.7    3.10        173
      11          ModDmax     139.7    3.60        177
      12         Exp-Dmax     134.8    3.30        175
      13 Log-Poly-ModDmax     153.2    4.50        183
      14  Log-Exp-ModDmax     155.4    4.70        184
      15             LTP1     111.0    2.20        161
      16             LTP2     159.6    5.10        186
      17          LTratio      77.3    1.30        136
      18              IAT     124.8    2.80        170

# fixture output: negative_rest

    Code
      print(run())
    Output
                   method intensity lactate
      1           Log-log     156.8     1.8
      2          OBLA 2.0     167.6     2.0
      3          OBLA 2.5     189.9     2.5
      4          OBLA 3.0     207.3     3.0
      5          OBLA 3.5     221.9     3.5
      6          OBLA 4.0     234.6     4.0
      7        Bsln + 0.5     149.9     1.7
      8        Bsln + 1.0     177.4     2.2
      9        Bsln + 1.5     197.3     2.7
      10             Dmax     184.7     2.4
      11          ModDmax     200.9     2.8
      12         Exp-Dmax     195.2     2.7
      13 Log-Poly-ModDmax     227.5     3.7
      14  Log-Exp-ModDmax     232.5     3.8
      15             LTP1     115.9     1.3
      16             LTP2     218.9     3.4
      17          LTratio     144.1     1.7
      18              IAT     214.7     3.2

# fixture output: running

    Code
      print(run())
    Output
                   method intensity  pace lactate heart_rate
      1           Log-log     10.85 331.7     1.4        148
      2          OBLA 2.0     12.31 292.5     2.0        158
      3          OBLA 2.5     13.16 273.6     2.5        164
      4          OBLA 3.0     13.83 260.4     3.0        169
      5          OBLA 3.5     14.39 250.2     3.5        172
      6          OBLA 4.0     14.87 242.0     4.0        175
      7        Bsln + 0.5     11.37 316.7     1.6        152
      8        Bsln + 1.0     12.50 288.0     2.1        159
      9        Bsln + 1.5     13.30 270.6     2.6        165
      10             Dmax     13.79 261.1     3.0        169
      11          ModDmax     14.44 249.4     3.5        173
      12         Exp-Dmax     14.11 255.2     3.3        171
      13 Log-Poly-ModDmax     15.46 232.8     4.7        179
      14  Log-Exp-ModDmax     15.63 230.3     4.8        180
      15             LTP1     10.97 328.2     1.5        149
      16             LTP2     15.04 239.3     4.2        176
      17          LTratio      9.00 400.1     1.1        135
      18              IAT     13.32 270.3     2.6        165

# fixture output: swimming

    Code
      print(run())
    Output
                   method intensity  pace lactate
      1           Log-log     1.157  86.4     1.7
      2          OBLA 2.0     1.199  83.4     2.0
      3          OBLA 2.5     1.248  80.2     2.5
      4          OBLA 3.0     1.284  77.9     3.0
      5          OBLA 3.5     1.314  76.1     3.5
      6          OBLA 4.0     1.340  74.6     4.0
      7        Bsln + 0.5     1.123  89.1     1.5
      8        Bsln + 1.0     1.199  83.4     2.0
      9        Bsln + 1.5     1.248  80.2     2.5
      10             Dmax     1.281  78.1     3.0
      11          ModDmax     1.318  75.9     3.6
      12         Exp-Dmax     1.305  76.7     3.4
      13 Log-Poly-ModDmax     1.340  74.6     4.0
      14  Log-Exp-ModDmax     1.355  73.8     4.3
      15             LTP1     1.160  86.2     1.7
      16             LTP2     1.348  74.2     4.2
      17          LTratio     1.000 100.0     1.2
      18              IAT     1.265  79.0     2.7

