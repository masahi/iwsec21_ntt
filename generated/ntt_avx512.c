
#include <immintrin.h>
#include <stdint.h>

__m512i csub_vec(__m512i arg0) {
  __m512i v_8 = _mm512_sub_epi16(arg0, _mm512_set1_epi16(12289));
  return _mm512_add_epi16(v_8, _mm512_and_si512(_mm512_srai_epi16(v_8, 15),
                                                _mm512_set1_epi16(12289)));
}

__m512i barret_reduce_vec(__m512i arg0) {
  return _mm512_sub_epi16(
      arg0, _mm512_mullo_epi16(_mm512_mulhi_epu16(arg0, _mm512_set1_epi16(5)),
                               _mm512_set1_epi16(12289)));
}

uint16_t v_9[1152] = {
    4091,  4091,  4091,  4091,  4091,  4091,  4091,  4091,  4091,  4091,  4091,
    4091,  4091,  4091,  4091,  4091,  4091,  4091,  4091,  4091,  4091,  4091,
    4091,  4091,  4091,  4091,  4091,  4091,  4091,  4091,  4091,  4091,  4091,
    7888,  4091,  7888,  4091,  7888,  4091,  7888,  4091,  7888,  4091,  7888,
    4091,  7888,  4091,  7888,  4091,  7888,  4091,  7888,  4091,  7888,  4091,
    7888,  4091,  7888,  4091,  7888,  4091,  7888,  4091,  7888,  4091,  11060,
    7888,  11208, 4091,  11060, 7888,  11208, 4091,  11060, 7888,  11208, 4091,
    11060, 7888,  11208, 4091,  11060, 7888,  11208, 4091,  11060, 7888,  11208,
    4091,  11060, 7888,  11208, 4091,  11060, 7888,  11208, 4091,  6960,  11060,
    6275,  7888,  4342,  11208, 9759,  4091,  6960,  11060, 6275,  7888,  4342,
    11208, 9759,  4091,  6960,  11060, 6275,  7888,  4342,  11208, 9759,  4091,
    6960,  11060, 6275,  7888,  4342,  11208, 9759,  4091,  1591,  6960,  586,
    11060, 9477,  6275,  7538,  7888,  6399,  4342,  5825,  11208, 5266,  9759,
    9710,  4091,  1591,  6960,  586,   11060, 9477,  6275,  7538,  7888,  6399,
    4342,  5825,  11208, 5266,  9759,  9710,  4091,  1134,  1591,  10414, 6960,
    7099,  586,   1364,  11060, 1711,  9477,  1885,  6275,  3743,  7538,  10164,
    7888,  6407,  6399,  8100,  4342,  7674,  5825,  10329, 11208, 965,   5266,
    1688,  9759,  6442,  9710,  9180,  4091,  12210, 1134,  12189, 1591,  2829,
    10414, 2181,  6960,  4783,  7099,  7610,  586,   7144,  1364,  4843,  11060,
    997,   1711,  10751, 9477,  4431,  1885,  8720,  6275,  1549,  3743,  3983,
    7538,  5664,  10164, 14,    7888,  6240,  6407,  432,   6399,  6458,  8100,
    6308,  4342,  4407,  7674,  1534,  5825,  2564,  10329, 1690,  11208, 117,
    965,   1237,  5266,  8877,  1688,  6570,  9759,  7072,  6442,  7863,  9710,
    4042,  9180,  3872,  4091,  5569,  12210, 11061, 1134,  3316,  12189, 9179,
    1591,  1553,  2829,  730,   10414, 9277,  2181,  7613,  6960,  7543,  4783,
    4970,  7099,  10637, 7610,  3180,  586,   1020,  7144,  5892,  1364,  10469,
    4843,  6836,  11060, 12163, 997,   2742,  1711,  5285,  10751, 3782,  9477,
    8401,  4431,  3854,  1885,  3323,  8720,  834,   6275,  4673,  1549,  10078,
    3743,  9493,  3983,  4668,  7538,  10772, 5664,  9020,  10164, 1502,  14,
    5351,  7888,  9368,  6240,  9729,  6407,  11236, 432,   3604,  6399,  1156,
    6458,  1762,  8100,  6130,  6308,  9386,  4342,  2315,  4407,  10481, 7674,
    10086, 1534,  3467,  5825,  2967,  2564,  10922, 10329, 489,   1690,  3403,
    11208, 2019,  117,   12241, 965,   11578, 1237,  10206, 5266,  11389, 8877,
    2030,  1688,  883,   6570,  7703,  9759,  7340,  7072,  1195,  6442,  6180,
    7863,  2446,  9710,  7045,  4042,  5274,  9180,  2851,  3872,  12276, 4091,
    3580,  5569,  737,   12210, 6945,  11061, 932,   1134,  6865,  3316,  10733,
    12189, 5680,  9179,  4602,  1591,  10401, 1553,  5186,  2829,  8901,  730,
    7692,  10414, 8188,  9277,  6409,  2181,  1467,  7613,  5070,  6960,  10209,
    7543,  9046,  4783,  6057,  4970,  351,   7099,  12145, 10637, 2895,  7610,
    10156, 3180,  3711,  586,   6040,  1020,  3509,  7144,  9589,  5892,  2053,
    1364,  6090,  10469, 5064,  4843,  2649,  6836,  7421,  11060, 10820, 12163,
    4699,  997,   9731,  2742,  8927,  1711,  3585,  5285,  7037,  10751, 6251,
    3782,  11300, 9477,  7338,  8401,  4552,  4431,  8846,  3854,  12126, 1885,
    3533,  3323,  2962,  8720,  8553,  834,   11616, 6275,  12250, 4673,  16,
    1549,  7871,  10078, 237,   3743,  3684,  9493,  8887,  3983,  2341,  4668,
    300,   7538,  9330,  10772, 7516,  5664,  7630,  9020,  3802,  10164, 10099,
    1502,  5625,  14,    9036,  5351,  5746,  7888,  1739,  9368,  3698,  6240,
    1949,  9729,  10229, 6407,  9668,  11236, 3281,  432,   4956,  3604,  1748,
    6399,  2749,  1156,  10531, 6458,  9229,  1762,  3146,  8100,  6902,  6130,
    8197,  6308,  5460,  9386,  10049, 4342,  4070,  2315,  3687,  4407,  378,
    10481, 9298,  7674,  4063,  10086, 7156,  1534,  8723,  3467,  4614,  5825,
    943,   2967,  8436,  2564,  11664, 10922, 11285, 10329, 727,   489,   6634,
    1690,  2320,  3403,  10707, 11208, 9787,  2019,  5753,  117,   10559, 12241,
    7642,  965,   6633,  11578, 1060,  1237,  8388,  10206, 340,   5266,  10574,
    11389, 1964,  8877,  4551,  2030,  7586,  1688,  9807,  883,   6375,  6570,
    7783,  7703,  12247, 9759,  8525,  7340,  914,   7072,  8763,  1195,  5858,
    6442,  7680,  6180,  5357,  7863,  3159,  2446,  10993, 9710,  1477,  7045,
    5381,  4042,  8821,  5274,  5204,  9180,  7003,  2851,  278,   3872,  6188,
    12276, 5654,  4091,  3835,  3580,  3374,  5569,  2523,  737,   11535, 12210,
    8418,  6945,  8502,  11061, 1273,  932,   8801,  1134,  6410,  6865,  4582,
    3316,  2727,  10733, 9779,  12189, 7389,  5680,  7962,  9179,  7367,  4602,
    4296,  1591,  4225,  10401, 5800,  1553,  2363,  5186,  8334,  2829,  3442,
    8901,  6034,  730,   11192, 7692,  8238,  10414, 6437,  8188,  7964,  9277,
    12169, 6409,  6816,  2181,  8557,  1467,  10438, 7613,  4367,  5070,  2650,
    6960,  9237,  10209, 8681,  7543,  937,   9046,  850,   4783,  876,   6057,
    1857,  4970,  10039, 351,   4910,  7099,  3759,  12145, 5233,  10637, 5075,
    2895,  6676,  7610,  4220,  10156, 6084,  3180,  8352,  3711,  9793,  586,
    4136,  6040,  1024,  1020,  824,   3509,  12184, 7144,  5964,  9589,  2879,
    5892,  6061,  2053,  2285,  1364,  5391,  6090,  3474,  10469, 9132,  5064,
    2356,  4843,  3816,  2649,  6911,  6836,  3161,  7421,  7248,  11060, 1224,
    10820, 1753,  12163, 6115,  4699,  9049,  997,   11986, 9731,  9837,  2742,
    11468, 8927,  7308,  1711,  10105, 3585,  3619,  5285,  896,   7037,  721,
    10751, 10661, 6251,  11363, 3782,  983,   11300, 695,   9477,  9680,  7338,
    3181,  8401,  6112,  4552,  1846,  4431,  8206,  8846,  3339,  3854,  4511,
    12126, 4302,  1885,  6342,  3533,  1071,  3323,  3070,  2962,  9959,  8720,
    9454,  8553,  1271,  834,   3999,  11616, 3890,  6275,  250,   12250, 10378,
    4673,  7775,  16,    784,   1549,  2167,  7871,  4720,  10078, 2262,  237,
    11613, 3743,  11361, 3684,  8470,  9493,  10464, 8887,  5348,  3983,  10832,
    2341,  4108,  4668,  7530,  300,   2411,  7538,  692,   9330,  2477,  10772,
    11690, 7516,  11903, 5664,  7178,  7630,  5200,  9020,  11865, 3802,  1963,
    10164, 6476,  10099, 3291,  1502,  12153, 5625,  5267,  14,    686,   9036,
    360,   5351,  4130,  5746,  11196, 7888,  5553,  1739,  11477, 9368,  4339,
    3698,  9156,  6240,  10824, 1949,  9478,  9729,  9739,  10229, 9661,  6407,
    6718,  9668,  6750,  11236, 9848,  3281,  1012,  432,   8879,  4956,  9353,
    3604,  4550,  1748,  11918, 6399,  6326,  2749,  11811, 1156,  7488,  10531,
    12170, 6458,  9217,  9229,  9817,  1762,  315,   3146,  6686,  8100,  3652,
    6902,  6395,  6130,  5434,  8197,  8405,  6308,  1867,  5460,  9471,  9386,
    5221,  10049, 841,   4342,  3845,  4070,  2806,  2315,  2834,  3687,  8617,
    4407,  7030,  378,   6233,  10481, 9720,  9298,  909,   7674,  7356,  4063,
    2463,  10086, 2654,  7156,  6552,  1534,  1432,  8723,  9601,  3467,  10126,
    4614,  4884,  5825,  2778,  943,   9340,  2967,  10204, 8436,  7827,  2564,
    2746,  11664, 6242,  10922, 6751,  11285, 12249, 10329, 2272,  727,   11045,
    489,   11672, 6634,  5552,  1690,  9076,  2320,  3079,  3403,  6990,  10707,
    8505,  11208, 8476,  9787,  292,   2019,  619,   5753,  11539, 117,   5733,
    10559, 1253,  12241, 9937,  7642,  5788,  965,   10418, 6633,  5503,  11578,
    2028,  1060,  2784,  1237,  11457, 8388,  5475,  10206, 8534,  340,   4371,
    5266,  12254, 10574, 1988,  11389, 5056,  1964,  10213, 8877,  4858,  4551,
    1797,  2030,  1158,  7586,  3044,  1688,  8978,  9807,  1272,  883,   6400,
    6375,  5150,  6570,  2416,  7783,  408,   7703,  8777,  12247, 10231, 9759,
    11209, 8525,  12188, 7340,  3279,  914,   7919,  7072,  2436,  8763,  11561,
    1195,  9399,  5858,  4395,  6442,  8433,  7680,  7650,  6180,  7884,  5357,
    4424,  7863,  4328,  3159,  7323,  2446,  9253,  10993, 10230, 9710,  8808,
    1477,  10928, 7045,  1113,  5381,  5600,  4042,  1434,  8821,  2114,  5274,
    357,   5204,  9216,  9180,  7416,  7003,  11344, 2851,  4520,  278,   1333,
    3872,  5393,  6188,  8276,  12276, 11652, 5654,  6688};

int v_10[1024] = {
    0,   512,  256, 768,  128, 640,  384, 896,  64,  576,  320, 832,  192, 704,
    448, 960,  32,  544,  288, 800,  160, 672,  416, 928,  96,  608,  352, 864,
    224, 736,  480, 992,  16,  528,  272, 784,  144, 656,  400, 912,  80,  592,
    336, 848,  208, 720,  464, 976,  48,  560,  304, 816,  176, 688,  432, 944,
    112, 624,  368, 880,  240, 752,  496, 1008, 8,   520,  264, 776,  136, 648,
    392, 904,  72,  584,  328, 840,  200, 712,  456, 968,  40,  552,  296, 808,
    168, 680,  424, 936,  104, 616,  360, 872,  232, 744,  488, 1000, 24,  536,
    280, 792,  152, 664,  408, 920,  88,  600,  344, 856,  216, 728,  472, 984,
    56,  568,  312, 824,  184, 696,  440, 952,  120, 632,  376, 888,  248, 760,
    504, 1016, 4,   516,  260, 772,  132, 644,  388, 900,  68,  580,  324, 836,
    196, 708,  452, 964,  36,  548,  292, 804,  164, 676,  420, 932,  100, 612,
    356, 868,  228, 740,  484, 996,  20,  532,  276, 788,  148, 660,  404, 916,
    84,  596,  340, 852,  212, 724,  468, 980,  52,  564,  308, 820,  180, 692,
    436, 948,  116, 628,  372, 884,  244, 756,  500, 1012, 12,  524,  268, 780,
    140, 652,  396, 908,  76,  588,  332, 844,  204, 716,  460, 972,  44,  556,
    300, 812,  172, 684,  428, 940,  108, 620,  364, 876,  236, 748,  492, 1004,
    28,  540,  284, 796,  156, 668,  412, 924,  92,  604,  348, 860,  220, 732,
    476, 988,  60,  572,  316, 828,  188, 700,  444, 956,  124, 636,  380, 892,
    252, 764,  508, 1020, 2,   514,  258, 770,  130, 642,  386, 898,  66,  578,
    322, 834,  194, 706,  450, 962,  34,  546,  290, 802,  162, 674,  418, 930,
    98,  610,  354, 866,  226, 738,  482, 994,  18,  530,  274, 786,  146, 658,
    402, 914,  82,  594,  338, 850,  210, 722,  466, 978,  50,  562,  306, 818,
    178, 690,  434, 946,  114, 626,  370, 882,  242, 754,  498, 1010, 10,  522,
    266, 778,  138, 650,  394, 906,  74,  586,  330, 842,  202, 714,  458, 970,
    42,  554,  298, 810,  170, 682,  426, 938,  106, 618,  362, 874,  234, 746,
    490, 1002, 26,  538,  282, 794,  154, 666,  410, 922,  90,  602,  346, 858,
    218, 730,  474, 986,  58,  570,  314, 826,  186, 698,  442, 954,  122, 634,
    378, 890,  250, 762,  506, 1018, 6,   518,  262, 774,  134, 646,  390, 902,
    70,  582,  326, 838,  198, 710,  454, 966,  38,  550,  294, 806,  166, 678,
    422, 934,  102, 614,  358, 870,  230, 742,  486, 998,  22,  534,  278, 790,
    150, 662,  406, 918,  86,  598,  342, 854,  214, 726,  470, 982,  54,  566,
    310, 822,  182, 694,  438, 950,  118, 630,  374, 886,  246, 758,  502, 1014,
    14,  526,  270, 782,  142, 654,  398, 910,  78,  590,  334, 846,  206, 718,
    462, 974,  46,  558,  302, 814,  174, 686,  430, 942,  110, 622,  366, 878,
    238, 750,  494, 1006, 30,  542,  286, 798,  158, 670,  414, 926,  94,  606,
    350, 862,  222, 734,  478, 990,  62,  574,  318, 830,  190, 702,  446, 958,
    126, 638,  382, 894,  254, 766,  510, 1022, 1,   513,  257, 769,  129, 641,
    385, 897,  65,  577,  321, 833,  193, 705,  449, 961,  33,  545,  289, 801,
    161, 673,  417, 929,  97,  609,  353, 865,  225, 737,  481, 993,  17,  529,
    273, 785,  145, 657,  401, 913,  81,  593,  337, 849,  209, 721,  465, 977,
    49,  561,  305, 817,  177, 689,  433, 945,  113, 625,  369, 881,  241, 753,
    497, 1009, 9,   521,  265, 777,  137, 649,  393, 905,  73,  585,  329, 841,
    201, 713,  457, 969,  41,  553,  297, 809,  169, 681,  425, 937,  105, 617,
    361, 873,  233, 745,  489, 1001, 25,  537,  281, 793,  153, 665,  409, 921,
    89,  601,  345, 857,  217, 729,  473, 985,  57,  569,  313, 825,  185, 697,
    441, 953,  121, 633,  377, 889,  249, 761,  505, 1017, 5,   517,  261, 773,
    133, 645,  389, 901,  69,  581,  325, 837,  197, 709,  453, 965,  37,  549,
    293, 805,  165, 677,  421, 933,  101, 613,  357, 869,  229, 741,  485, 997,
    21,  533,  277, 789,  149, 661,  405, 917,  85,  597,  341, 853,  213, 725,
    469, 981,  53,  565,  309, 821,  181, 693,  437, 949,  117, 629,  373, 885,
    245, 757,  501, 1013, 13,  525,  269, 781,  141, 653,  397, 909,  77,  589,
    333, 845,  205, 717,  461, 973,  45,  557,  301, 813,  173, 685,  429, 941,
    109, 621,  365, 877,  237, 749,  493, 1005, 29,  541,  285, 797,  157, 669,
    413, 925,  93,  605,  349, 861,  221, 733,  477, 989,  61,  573,  317, 829,
    189, 701,  445, 957,  125, 637,  381, 893,  253, 765,  509, 1021, 3,   515,
    259, 771,  131, 643,  387, 899,  67,  579,  323, 835,  195, 707,  451, 963,
    35,  547,  291, 803,  163, 675,  419, 931,  99,  611,  355, 867,  227, 739,
    483, 995,  19,  531,  275, 787,  147, 659,  403, 915,  83,  595,  339, 851,
    211, 723,  467, 979,  51,  563,  307, 819,  179, 691,  435, 947,  115, 627,
    371, 883,  243, 755,  499, 1011, 11,  523,  267, 779,  139, 651,  395, 907,
    75,  587,  331, 843,  203, 715,  459, 971,  43,  555,  299, 811,  171, 683,
    427, 939,  107, 619,  363, 875,  235, 747,  491, 1003, 27,  539,  283, 795,
    155, 667,  411, 923,  91,  603,  347, 859,  219, 731,  475, 987,  59,  571,
    315, 827,  187, 699,  443, 955,  123, 635,  379, 891,  251, 763,  507, 1019,
    7,   519,  263, 775,  135, 647,  391, 903,  71,  583,  327, 839,  199, 711,
    455, 967,  39,  551,  295, 807,  167, 679,  423, 935,  103, 615,  359, 871,
    231, 743,  487, 999,  23,  535,  279, 791,  151, 663,  407, 919,  87,  599,
    343, 855,  215, 727,  471, 983,  55,  567,  311, 823,  183, 695,  439, 951,
    119, 631,  375, 887,  247, 759,  503, 1015, 15,  527,  271, 783,  143, 655,
    399, 911,  79,  591,  335, 847,  207, 719,  463, 975,  47,  559,  303, 815,
    175, 687,  431, 943,  111, 623,  367, 879,  239, 751,  495, 1007, 31,  543,
    287, 799,  159, 671,  415, 927,  95,  607,  351, 863,  223, 735,  479, 991,
    63,  575,  319, 831,  191, 703,  447, 959,  127, 639,  383, 895,  255, 767,
    511, 1023};

void bit_reverse(uint16_t *arg0) {
  for (int v_11 = 0; v_11 < 1024; v_11 += 1) {
    if ((v_11 < v_10[v_11])) {
      uint16_t v_12 = arg0[v_11];
      arg0[v_11] = arg0[v_10[v_11]];
      arg0[v_10[v_11]] = v_12;
    }
  }
}

__m512i vmul(__m512i arg0, __m512i arg1) {
  __m512i v_19 = _mm512_mullo_epi16(arg0, arg1);
  __m512i v_20 = _mm512_mulhi_epu16(arg0, arg1);
  __m512i v_21 = _mm512_set1_epi16(12289);
  __m512i v_22 = _mm512_set1_epi16(12287);
  __m512i v_23 = _mm512_mullo_epi16(v_19, v_22);
  __m512i v_24 = _mm512_mulhi_epu16(v_23, v_21);
  __m512i v_25 = _mm512_add_epi16(
      _mm512_movm_epi16(_mm512_cmpeq_epi16_mask(v_19, _mm512_set1_epi16(0))),
      _mm512_set1_epi16(1));
  __m512i v_26 = _mm512_add_epi16(_mm512_add_epi16(v_20, v_24), v_25);
  return csub_vec(v_26);
}

__m512i vadd(__m512i arg0, __m512i arg1) {
  return _mm512_add_epi16(arg0, arg1);
}

__m512i vsub(__m512i arg0, __m512i arg1) {
  return barret_reduce_vec(
      _mm512_sub_epi16(_mm512_add_epi16(arg0, _mm512_set1_epi16(24578)), arg1));
}

void fft1(uint16_t *arg0) {
  __m512i v_13 = _mm512_loadu_si512((__m512i *)(v_9 + 0));
  for (int v_14 = 0; v_14 < 1024; v_14 += 64) {
    __m512i v_15 = _mm512_loadu_si512((__m512i *)(arg0 + v_14));
    __m512i v_16 = _mm512_loadu_si512((__m512i *)(arg0 + (v_14 + 32)));
    __m512i v_17 = _mm512_mask_blend_epi16(_cvtu32_mask32(0xAAAAAAAA), v_15,
                                           _mm512_slli_epi32(v_16, 16));
    __m512i v_18 = _mm512_mask_blend_epi16(_cvtu32_mask32(0xAAAAAAAA),
                                           _mm512_srli_epi32(v_15, 16), v_16);
    __m512i v_27 = vmul(v_18, v_13);
    __m512i v_28 = vadd(v_17, v_27);
    __m512i v_29 = vsub(v_17, v_27);
    __m512i v_30 = _mm512_mask_blend_epi16(_cvtu32_mask32(0xAAAAAAAA), v_28,
                                           _mm512_slli_epi32(v_29, 16));
    __m512i v_31 = _mm512_mask_blend_epi16(_cvtu32_mask32(0xAAAAAAAA),
                                           _mm512_srli_epi32(v_28, 16), v_29);
    _mm512_storeu_si512((__m512i *)(arg0 + v_14), v_30);
    _mm512_storeu_si512((__m512i *)(arg0 + (v_14 + 32)), v_31);
  }
}

void fft2(uint16_t *arg0) {
  __m512i v_32 = _mm512_loadu_si512((__m512i *)(v_9 + 32));
  for (int v_33 = 0; v_33 < 1024; v_33 += 64) {
    __m512i v_34 = _mm512_loadu_si512((__m512i *)(arg0 + v_33));
    __m512i v_35 = _mm512_loadu_si512((__m512i *)(arg0 + (v_33 + 32)));
    __m512i v_36 = _mm512_mask_blend_epi32(_cvtu32_mask16(0xAAAA), v_34,
                                           _mm512_slli_epi64(v_35, 32));
    __m512i v_37 = _mm512_mask_blend_epi32(_cvtu32_mask16(0xAAAA),
                                           _mm512_srli_epi64(v_34, 32), v_35);
    __m512i v_46 = vmul(v_37, v_32);
    __m512i v_47 = barret_reduce_vec(vadd(v_36, v_46));
    __m512i v_48 = vsub(v_36, v_46);
    __m512i v_49 = _mm512_mask_blend_epi32(_cvtu32_mask16(0xAAAA), v_47,
                                           _mm512_slli_epi64(v_48, 32));
    __m512i v_50 = _mm512_mask_blend_epi32(_cvtu32_mask16(0xAAAA),
                                           _mm512_srli_epi64(v_47, 32), v_48);
    _mm512_storeu_si512((__m512i *)(arg0 + v_33), v_49);
    _mm512_storeu_si512((__m512i *)(arg0 + (v_33 + 32)), v_50);
  }
}

void fft3(uint16_t *arg0) {
  __m512i v_51 = _mm512_loadu_si512((__m512i *)(v_9 + 64));
  for (int v_52 = 0; v_52 < 1024; v_52 += 64) {
    __m512i v_53 = _mm512_loadu_si512((__m512i *)(arg0 + v_52));
    __m512i v_54 = _mm512_loadu_si512((__m512i *)(arg0 + (v_52 + 32)));
    __m512i v_55 = _mm512_unpacklo_epi64(v_53, v_54);
    __m512i v_56 = _mm512_unpackhi_epi64(v_53, v_54);
    __m512i v_65 = vmul(v_56, v_51);
    __m512i v_66 = vadd(v_55, v_65);
    __m512i v_67 = vsub(v_55, v_65);
    __m512i v_68 = _mm512_unpacklo_epi64(v_66, v_67);
    __m512i v_69 = _mm512_unpackhi_epi64(v_66, v_67);
    _mm512_storeu_si512((__m512i *)(arg0 + v_52), v_68);
    _mm512_storeu_si512((__m512i *)(arg0 + (v_52 + 32)), v_69);
  }
}

void fft4(uint16_t *arg0) {
  __m512i v_70 = _mm512_loadu_si512((__m512i *)(v_9 + 96));
  for (int v_71 = 0; v_71 < 1024; v_71 += 64) {
    __m512i v_72 = _mm512_loadu_si512((__m512i *)(arg0 + v_71));
    __m512i v_73 = _mm512_loadu_si512((__m512i *)(arg0 + (v_71 + 32)));
    __m512i v_74 = _mm512_permutex2var_epi16(
        v_72,
        _mm512_set_epi16(55, 54, 53, 52, 51, 50, 49, 48, 23, 22, 21, 20, 19, 18,
                         17, 16, 39, 38, 37, 36, 35, 34, 33, 32, 7, 6, 5, 4, 3,
                         2, 1, 0),
        v_73);
    __m512i v_75 = _mm512_permutex2var_epi16(
        v_72,
        _mm512_set_epi16(63, 62, 61, 60, 59, 58, 57, 56, 31, 30, 29, 28, 27, 26,
                         25, 24, 47, 46, 45, 44, 43, 42, 41, 40, 15, 14, 13, 12,
                         11, 10, 9, 8),
        v_73);
    __m512i v_84 = vmul(v_75, v_70);
    __m512i v_85 = barret_reduce_vec(vadd(v_74, v_84));
    __m512i v_86 = vsub(v_74, v_84);
    __m512i v_87 = _mm512_permutex2var_epi16(
        v_85,
        _mm512_set_epi16(55, 54, 53, 52, 51, 50, 49, 48, 23, 22, 21, 20, 19, 18,
                         17, 16, 39, 38, 37, 36, 35, 34, 33, 32, 7, 6, 5, 4, 3,
                         2, 1, 0),
        v_86);
    __m512i v_88 = _mm512_permutex2var_epi16(
        v_85,
        _mm512_set_epi16(63, 62, 61, 60, 59, 58, 57, 56, 31, 30, 29, 28, 27, 26,
                         25, 24, 47, 46, 45, 44, 43, 42, 41, 40, 15, 14, 13, 12,
                         11, 10, 9, 8),
        v_86);
    _mm512_storeu_si512((__m512i *)(arg0 + v_71), v_87);
    _mm512_storeu_si512((__m512i *)(arg0 + (v_71 + 32)), v_88);
  }
}

void fft5(uint16_t *arg0) {
  __m512i v_89 = _mm512_loadu_si512((__m512i *)(v_9 + 128));
  for (int v_90 = 0; v_90 < 1024; v_90 += 64) {
    __m512i v_91 = _mm512_loadu_si512((__m512i *)(arg0 + v_90));
    __m512i v_92 = _mm512_loadu_si512((__m512i *)(arg0 + (v_90 + 32)));
    __m512i v_93 = _mm512_permutex2var_epi16(
        v_91,
        _mm512_set_epi16(47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34,
                         33, 32, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2,
                         1, 0),
        v_92);
    __m512i v_94 = _mm512_permutex2var_epi16(
        v_91,
        _mm512_set_epi16(63, 62, 61, 60, 59, 58, 57, 56, 55, 54, 53, 52, 51, 50,
                         49, 48, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20,
                         19, 18, 17, 16),
        v_92);
    __m512i v_103 = vmul(v_94, v_89);
    __m512i v_104 = vadd(v_93, v_103);
    __m512i v_105 = vsub(v_93, v_103);
    __m512i v_106 = _mm512_permutex2var_epi16(
        v_104,
        _mm512_set_epi16(47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34,
                         33, 32, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2,
                         1, 0),
        v_105);
    __m512i v_107 = _mm512_permutex2var_epi16(
        v_104,
        _mm512_set_epi16(63, 62, 61, 60, 59, 58, 57, 56, 55, 54, 53, 52, 51, 50,
                         49, 48, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20,
                         19, 18, 17, 16),
        v_105);
    _mm512_storeu_si512((__m512i *)(arg0 + v_90), v_106);
    _mm512_storeu_si512((__m512i *)(arg0 + (v_90 + 32)), v_107);
  }
}

void fft6(uint16_t *arg0) {
  for (int v_108 = 0; v_108 < 1024; v_108 += 64) {
    for (int v_109 = 0; v_109 < ((32 - 0) / 32); v_109 += 1) {
      __m512i v_118 = vmul(
          _mm512_loadu_si512(
              (__m512i *)(arg0 + ((v_108 + (0 + (v_109 * 32))) + 32))),
          _mm512_loadu_si512((__m512i *)(v_9 + (160 + (0 + (v_109 * 32))))));
      __m512i v_119 =
          _mm512_loadu_si512((__m512i *)(arg0 + (v_108 + (0 + (v_109 * 32)))));
      _mm512_storeu_si512((__m512i *)(arg0 + (v_108 + (0 + (v_109 * 32)))),
                          barret_reduce_vec(vadd(v_119, v_118)));
      _mm512_storeu_si512(
          (__m512i *)(arg0 + ((v_108 + (0 + (v_109 * 32))) + 32)),
          vsub(v_119, v_118));
    }
  }
}

void fft7(uint16_t *arg0) {
  for (int v_120 = 0; v_120 < 1024; v_120 += 128) {
    for (int v_121 = 0; v_121 < ((64 - 0) / 32); v_121 += 1) {
      __m512i v_130 = vmul(
          _mm512_loadu_si512(
              (__m512i *)(arg0 + ((v_120 + (0 + (v_121 * 32))) + 64))),
          _mm512_loadu_si512((__m512i *)(v_9 + (192 + (0 + (v_121 * 32))))));
      __m512i v_131 =
          _mm512_loadu_si512((__m512i *)(arg0 + (v_120 + (0 + (v_121 * 32)))));
      _mm512_storeu_si512((__m512i *)(arg0 + (v_120 + (0 + (v_121 * 32)))),
                          vadd(v_131, v_130));
      _mm512_storeu_si512(
          (__m512i *)(arg0 + ((v_120 + (0 + (v_121 * 32))) + 64)),
          vsub(v_131, v_130));
    }
  }
}

void fft8(uint16_t *arg0) {
  for (int v_132 = 0; v_132 < 1024; v_132 += 256) {
    for (int v_133 = 0; v_133 < ((128 - 0) / 32); v_133 += 1) {
      __m512i v_142 = vmul(
          _mm512_loadu_si512(
              (__m512i *)(arg0 + ((v_132 + (0 + (v_133 * 32))) + 128))),
          _mm512_loadu_si512((__m512i *)(v_9 + (256 + (0 + (v_133 * 32))))));
      __m512i v_143 =
          _mm512_loadu_si512((__m512i *)(arg0 + (v_132 + (0 + (v_133 * 32)))));
      _mm512_storeu_si512((__m512i *)(arg0 + (v_132 + (0 + (v_133 * 32)))),
                          barret_reduce_vec(vadd(v_143, v_142)));
      _mm512_storeu_si512(
          (__m512i *)(arg0 + ((v_132 + (0 + (v_133 * 32))) + 128)),
          vsub(v_143, v_142));
    }
  }
}

void fft9(uint16_t *arg0) {
  for (int v_144 = 0; v_144 < 1024; v_144 += 512) {
    for (int v_145 = 0; v_145 < ((256 - 0) / 32); v_145 += 1) {
      __m512i v_154 = vmul(
          _mm512_loadu_si512(
              (__m512i *)(arg0 + ((v_144 + (0 + (v_145 * 32))) + 256))),
          _mm512_loadu_si512((__m512i *)(v_9 + (384 + (0 + (v_145 * 32))))));
      __m512i v_155 =
          _mm512_loadu_si512((__m512i *)(arg0 + (v_144 + (0 + (v_145 * 32)))));
      _mm512_storeu_si512((__m512i *)(arg0 + (v_144 + (0 + (v_145 * 32)))),
                          vadd(v_155, v_154));
      _mm512_storeu_si512(
          (__m512i *)(arg0 + ((v_144 + (0 + (v_145 * 32))) + 256)),
          vsub(v_155, v_154));
    }
  }
}

void fft10(uint16_t *arg0) {
  for (int v_156 = 0; v_156 < 1024; v_156 += 1024) {
    for (int v_157 = 0; v_157 < ((512 - 0) / 32); v_157 += 1) {
      __m512i v_166 = vmul(
          _mm512_loadu_si512(
              (__m512i *)(arg0 + ((v_156 + (0 + (v_157 * 32))) + 512))),
          _mm512_loadu_si512((__m512i *)(v_9 + (640 + (0 + (v_157 * 32))))));
      __m512i v_167 =
          _mm512_loadu_si512((__m512i *)(arg0 + (v_156 + (0 + (v_157 * 32)))));
      _mm512_storeu_si512((__m512i *)(arg0 + (v_156 + (0 + (v_157 * 32)))),
                          barret_reduce_vec(vadd(v_167, v_166)));
      _mm512_storeu_si512(
          (__m512i *)(arg0 + ((v_156 + (0 + (v_157 * 32))) + 512)),
          vsub(v_167, v_166));
    }
  }
}

void fft(uint16_t *arg0) {
  bit_reverse(arg0);
  fft1(arg0);
  fft2(arg0);
  fft3(arg0);
  fft4(arg0);
  fft5(arg0);
  fft6(arg0);
  fft7(arg0);
  fft8(arg0);
  fft9(arg0);
  fft10(arg0);
}
