# fixture output: demo

    Code
      print(run())
    Output
                   method intensity lactate heart_rate
      1           Log-log       101     1.8        154
      2          OBLA 2.0       107     2.0        158
      3          OBLA 2.5       120     2.5        167
      4          OBLA 3.0       130     3.0        172
      5          OBLA 3.5       138     3.5        176
      6          OBLA 4.0       146     4.0        180
      7        Bsln + 0.5        85     1.4        142
      8        Bsln + 1.0       105     1.9        157
      9        Bsln + 1.5       118     2.4        166
      10             Dmax       132     3.1        173
      11          ModDmax       140     3.6        177
      12         Exp-Dmax       135     3.3        175
      13 Log-Poly-ModDmax       149     4.2        182
      14  Log-Exp-ModDmax       152     4.4        183
      15             LTP1       111     2.0        161
      16             LTP2       160     4.7        186
      17          LTratio        71     1.2        131
      18              IAT       122     2.7        168

# fixture output: noisy

    Code
      print(run())
    Output
                   method intensity lactate heart_rate
      1           Log-log       120     2.4        168
      2          OBLA 2.0       105     2.0        157
      3          OBLA 2.5       119     2.5        167
      4          OBLA 3.0       130     3.0        173
      5          OBLA 3.5       138     3.5        177
      6          OBLA 4.0       146     4.0        181
      7        Bsln + 0.5        81     1.4        138
      8        Bsln + 1.0       103     1.9        155
      9        Bsln + 1.5       117     2.4        165
      10             Dmax       132     3.1        174
      11          ModDmax       141     3.6        178
      12         Exp-Dmax       135     3.3        176
      13 Log-Poly-ModDmax       158     4.9        187
      14  Log-Exp-ModDmax       159     5.0        187
      15             LTP1       117     2.2        165
      16             LTP2       168     5.5        191
      17          LTratio        74     1.3        133
      18              IAT       125     2.8        171

# fixture output: plateau

    Code
      print(run())
    Output
                   method intensity lactate
      1           Log-log       171     1.6
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
      15             LTP1       185     1.4
      16             LTP2       335     6.2
      17          LTratio       156     1.4
      18              IAT       235     2.9

# fixture output: plateau_x3

    Code
      print(run())
    Output
                   method intensity lactate
      1           Log-log       171     4.9
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
      15             LTP1       185     4.3
      16             LTP2       335    18.7
      17          LTratio       156     4.1
      18              IAT       195     5.6

# fixture output: early_dip

    Code
      print(run())
    Output
                   method intensity lactate
      1           Log-log       201     1.3
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
      15             LTP1       175     1.0
      16             LTP2       281     2.7
      17          LTratio       209     1.4
      18              IAT       275     2.9

# fixture output: no_rest

    Code
      print(run())
    Output
                   method intensity lactate heart_rate
      1           Log-log       101     1.8        154
      2          OBLA 2.0       106     2.0        158
      3          OBLA 2.5       119     2.5        166
      4          OBLA 3.0       130     3.0        172
      5          OBLA 3.5       139     3.5        177
      6          OBLA 4.0       146     4.0        180
      7        Bsln + 0.5        85     1.5        142
      8        Bsln + 1.0       105     2.0        157
      9        Bsln + 1.5       119     2.5        166
      10             Dmax       132     3.1        173
      11          ModDmax       140     3.6        177
      12         Exp-Dmax       135     3.3        175
      13 Log-Poly-ModDmax       149     4.2        182
      14  Log-Exp-ModDmax       152     4.4        183
      15             LTP1       111     2.0        161
      16             LTP2       160     4.7        186
      17          LTratio        77     1.3        136
      18              IAT       125     2.8        170

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
      15             LTP1       121     1.3
      16             LTP2       219     3.0
      17          LTratio       144     1.7
      18              IAT       215     3.2

# fixture output: running

    Code
      print(run())
    Output
                   method intensity  pace lactate heart_rate
      1           Log-log      12.0 298.9     1.8        156
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
      15             LTP1      11.3 317.6     1.5        151
      16             LTP2      15.0 239.3     3.7        176
      17          LTratio       9.0 400.1     1.1        135
      18              IAT      13.3 270.3     2.6        165

# fixture output: swimming

    Code
      print(run())
    Output
                   method intensity  pace lactate
      1           Log-log      1.20  83.6     1.9
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

