testthat::test_that("annotate peaks", {
   ann_params <- AnnotationParam(
        da_tol = .015,
        rt_tol = 10,
        abd_tol = 25,
        instrument = "QTOF_XevoG2-S_R25000@200",
        database = "test",
        polarity = "positive"
    )
    camera_params <- CameraParam(
        ann_params = ann_params,
        cores = 1,
        sigma = 6,
        perfwhm = .6,
        cor_eic_th = .75,
        pval = .05,
        graph_method = "hcs"
    )
    ann <- data.frame(
        pcgroup_id = c(1, 2, 3, 3, 3, 3, 3, 3, 4, 4, 5, 6, 7, 8),
        basepeak_group_id = c(4, 5, 1, 1, 2, 2, 12, 12, 13, 14, 6, 7, 15, 11),
        formula = c(NA, NA, "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                    "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                    "C30H59N1O3", "C30H59N1O3", NA, NA, NA, NA),
        class = c(NA, NA, "LPC", "LPC", "LPC", "LPC", "LPC", "LPC", "Cer",
                  "Cer", NA, NA, NA, NA),
        name = c(NA, NA, "LPC 11:0", "LPC 11a:0", "LPC 11:0", "LPC 11a:0",
                 "LPC 11:0", "LPC 11a:0", "Cer (d18:1/C12:0)",
                 "Cer (d18:1/C12:0)", NA, NA, NA, NA),
        referent_adduct = c(NA, NA, "[M+H]+", "[M+H]+", "[M+H]+", "[M+H]+",
                         "[M+H]+", "[M+H]+", "[M+H-H2O]+", "[M+H-H2O]+", NA, NA,
                         NA, NA),
        adduct = c(NA, NA, "[M+H-H2O]+", "[M+H-H2O]+", "[M+H]+", "[M+H]+",
                   "[M+Na]+", "[M+Na]+", "[M+H-H2O]+", "[M+Na]+", NA, NA, NA,
                   NA),
        ion_formula = c(NA, NA, "C19H39N1O6P1", "C19H39N1O6P1", "C19H41N1O7P1",
                        "C19H41N1O7P1", "C19H40N1O7P1Na1", "C19H40N1O7P1Na1",
                        "C30H58N1O2", "C30H59N1O3Na1", NA, NA, NA, NA),
        rtdiff = c(NA, NA, 8.99149999999997, 4.19150000000002, 8.99149999999997,
                   4.19150000000002, 8.99149999999997, 4.19150000000002,
                   2.37300000000002, 2.37300000000002, NA, NA, NA, NA),
        rt = c(279.141, 259.046, 286.8085, 286.8085, 286.8085, 286.8085,
               286.8085, 286.8085, 197.973, 197.973, 297.915, 308.494, 197.444,
               306.904),
        rtmin = c(265.656, 250.85, 284.692, 284.692, 284.692, 284.692, 280.99,
                  280.99, 195.858, 182.11, 293.684, 304.789, 196.386, 301.084),
        rtmax = c(293.156, 264.598, 287.864, 287.864, 293.152, 293.152, 292.095,
                  292.095, 200.617, 208.548, 303.202, 310.081, 201.145,
                  309.549),
        nsamples = c(2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0),
        best_score = c(0, 0, 79.8211975097656, 79.8211975097656,
                       95.0912628173828, 95.0912628173828, 79.6432037353516,
                       79.6432037353516, 71.3979721069336, 71.1946487426758, 0,
                       0, 0, 0),
        best_deviation_mz = c(NA, NA, .0003662109375, .0003662109375,
                              .00048828125, .00048828125, .00018310546875,
                              .00018310546875, .0010986328125, .001312255859375,
                              NA, NA, NA, NA),
        best_npeak = c(0, 0, 1, 1, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0),
        `220221CCM_global_POS_01_ssleu_filtered` = c(1, 3, NA, NA, 6, 6, 8, 8,
                                                     10, 12, 14, 15, 16, NA),
        `220221CCM_global_POS_02_ssleu_filtered` = c(2, 4, 5, 5, 7, 7, 9, 9, 11,
                                                     13, NA, NA, 17, 18),
        check.names = FALSE
    )
    spectra_infos <- data.frame(
        spectra_id =  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
                        17, 18),
        score = c(0, 0, 0, 0, 79.8211975097656, 95.0912628173828,
                  95.0683822631836, 79.6432037353516, 79.6432037353516,
                  71.3979721069336, 71.3979721069336, 71.1946487426758,
                  71.1946487426758, 0, 0, 0, 0, 0),
        deviation_mz = c(NaN, NaN, NaN, NaN, .0003662109375, .00048828125,
                         .0008392333984375, .00018310546875, .000701904296875,
                         .0010986328125, .001251220703125, .001312255859375,
                         .00177001953125, NaN, NaN, NaN, NaN, NaN),
        npeak = c(0, 0, 0, 0, 1, 2, 2, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
        basepeak_mz = c(428.267772595199, 428.268471709284, 428.267855601901,
                        428.268539638095, 408.251325886322, 426.261908233279,
                        426.262343531217, 448.243644005027, 448.244163142448,
                        464.447304014051, 464.447454557257, 504.440032161331,
                        504.44048100644, 428.2675574767, 428.267840674347,
                        505.443534603, 505.44383474773, 429.270782294993),
        basepeak_int = c(21634957.3317308, 19992518.2568646, 7556081.77126924,
                         7375409.9176154, 88824.635233072, 6139220.0505469,
                         6234084.85605467, 260064.992761365, 288524.169413714,
                         4945601.93026269, 5689144.27927454, 1287181.56877954,
                         1458245.19191226, 2235868.3566111, 753309.518850004,
                         401071.227087501, 444013.097852865, 323001.699462891),
        sum_int = c(0, 0, 0, 0, 88824.635233072, 7309860.00925784,
                    7420749.7462611, 260064.992761365, 288524.169413714,
                    4945601.93026269, 5689144.27927454, 1287181.56877954,
                    1458245.19191226, 0, 0, 0, 0, 0),
        sample = c("220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered"),
        rt  = c(279.407, 278.875, 258.782, 259.31, 286.278, 286.81, 286.807,
                286.81, 286.807, 197.973, 197.973, 197.444, 197.973, 297.915,
                308.494, 197.444, 197.444, 306.904)
    )
    spectras <- data.frame(
        spectra_id = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7,
                       7, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10, 10, 11, 11, 11,
                       11, 12, 12, 12, 12, 13, 13, 13, 13, 14, 14, 15, 16, 17,
                       18),
        group_id = c(4, 9, 4, 9, 5, 8, 5, 8, 1, NA, NA, NA, 2, 3, NA, NA, 2, 3,
                     NA, NA, 12, NA, NA, NA, 12, NA, NA, NA, 13, NA, NA, NA, 13,
                     NA, NA, NA, 14, NA, NA, NA, 14, NA, NA, NA, 6, 10, 7, 15,
                     15, 11),
        feature_id = c(7, 9, 22, 24, 8, 10, 23, 17, 18, NA, NA, NA, 6, 4, NA,
                       NA, 21, 20, NA, NA, 5, NA, NA, NA, 19, NA, NA, NA, 1, NA,
                       NA, NA, 15, NA, NA, NA, 2, NA, NA, NA, 16, NA, NA, NA,
                       12, 11, 13, 3, 14, 25),
        mz = c(428.267772595199, 429.270423563444, 428.268471709284,
               429.271192885151, 428.267855601901, 429.270339913969,
               428.268539638095, 429.270885645465, 408.251325886321, NA, NA, NA,
               426.261908233279, 427.265397484755, NA, NA, 426.262343531217,
               427.265671264404, NA, NA, 448.243644005027, NA, NA, NA,
               448.244163142448, NA, NA, NA, 464.447304014051, NA, NA, NA,
               464.447454557257, NA, NA, NA, 504.440032161331, NA, NA, NA,
               504.44048100644, NA, NA, NA, 428.2675574767, 429.269607476748,
               428.267840674347, 505.443534603, 505.44383474773,
               429.270782294993),
        mzmin = c(428.267364501953, 429.269836425781, 428.268035888672,
                  429.270721435547, 428.267425537109, 429.269653320312,
                  428.267822265625, 429.270324707031, 408.251037597656, NA, NA,
                  NA, 426.261444091797, 427.264739990234, NA, NA,
                  426.262023925781, 427.264801025391, NA, NA, 448.243011474609,
                  NA, NA, NA, 448.242279052734, NA, NA, NA, 464.447021484375,
                  NA, NA, NA, 464.446746826172, NA, NA, NA, 504.439697265625,
                  NA, NA, NA, 504.439544677734, NA, NA, NA, 428.267242431641,
                  429.269256591797, 428.267364501953, 505.443084716797,
                  505.440704345703, 429.270416259766),
        mzmax = c(428.268310546875, 429.271362304688, 428.269195556641,
                  429.271636962891, 428.268615722656, 429.270904541016,
                  428.269287109375, 429.271575927734, 408.25146484375, NA, NA,
                  NA, 426.262420654297, 427.266052246094, NA, NA,
                  426.262786865234, 427.266052246094, NA, NA, 448.245391845703,
                  NA, NA, NA, 448.245422363281, NA, NA, NA, 464.447662353516,
                  NA, NA, NA, 464.447967529297, NA, NA, NA, 504.440490722656,
                  NA, NA, NA, 504.441101074219, NA, NA, NA, 428.268035888672,
                  429.269958496094, 428.2685546875, 505.443664550781,
                  505.444122314453, 429.271636962891),
        rt = c(279.407, 279.407, 278.875, 278.875, 258.782, 258.782, 259.31,
               258.782, 286.278, NA, NA, NA, 286.81, 286.81, NA, NA, 286.807,
               286.807, NA, NA, 286.81, NA, NA, NA, 286.807, NA, NA, NA,
               197.973, NA, NA, NA, 197.973, NA, NA, NA, 197.444, NA, NA, NA,
               197.973, NA, NA, NA, 297.915, 296.857, 308.494, 197.444, 197.444,
               306.904),
        rtmin = c(265.656, 265.656, 265.656, 265.127, 250.85, 250.85, 250.85,
                  250.85, 284.692, NA, NA, NA, 284.695, 284.695, NA, NA,
                  284.692, 283.105, NA, NA, 285.224, NA, NA, NA, 280.99, NA, NA,
                  NA, 196.386, NA, NA, NA, 195.858, NA, NA, NA, 193.743, NA, NA,
                  NA, 182.11, NA, NA, NA, 293.684, 295.271, 304.789, 196.386,
                  196.387, 301.084),
        rtmax = c(293.156, 294.742, 291.037, 291.037, 264.598, 264.598, 264.598,
                  264.069, 287.864, NA, NA, NA, 291.04, 291.04, NA, NA, 293.152,
                  292.623, NA, NA, 291.04, NA, NA, NA, 292.095, NA, NA, NA,
                  200.617, NA, NA, NA, 199.559, NA, NA, NA, 199.03, NA, NA, NA,
                  208.548, NA, NA, NA, 303.202, 307.966, 310.081, 199.03,
                  201.145, 309.549),
        int = c(21634957.3317308, 5360632.29847273, 19992518.2568646,
                4939357.04715561, 7556081.77126924, 1854836.50349039,
                7375409.9176154, 1621835.871345, 88824.635233072, NA, NA, NA,
                6139220.0505469, 1170639.95871094, NA, NA, 6234084.85605467,
                1186664.89020643, NA, NA, 260064.992761365, NA, NA, NA,
                288524.169413714, NA, NA, NA, 4945601.93026269, NA, NA, NA,
                5689144.27927454, NA, NA, NA, 1287181.56877954, NA, NA, NA,
                1458245.19191226, NA, NA, NA, 2235868.3566111, 450077.636764323,
                753309.518850004, 401071.227087501, 444013.097852865,
                323001.699462891),
        abd = c(100, 24.7776421107639, 100, 24.7060274433394, 100,
                24.5475970170559, 100, 21.9897726290631, 100, NA, NA, NA, 100,
                19.0682195632759, NA, NA, 100, 19.0351096850072, NA, NA, 100,
                NA, NA, NA, 100, NA, NA, NA, 100, NA, NA, NA, 100, NA, NA, NA,
                100, NA, NA, NA, 100, NA, NA, NA, 100, 20.1298808775354, 100,
                100, 100, 100),
        mz_theo = c(NA, NA, NA, NA, NA, NA, NA, NA, 408.25095, 409.25427,
                    410.25672, 411.25935, 426.26152, 427.26484, 428.26719,
                    429.26984, 426.26152, 427.26484, 428.26719, 429.26984,
                    448.24346, 449.24678, 450.24914, 451.25178, 448.24346,
                    449.24678, 450.24914, 451.25178, 464.44621, 465.44955,
                    466.45272, 467.45576, 464.44621, 465.44955, 466.45272,
                    467.45576, 504.43872, 505.44206, 506.44516, 507.44809,
                    504.43872, 505.44206, 506.44516, 507.44809, NA, NA, NA, NA,
                    NA, NA),
        abd_theo = c(NA, NA, NA, NA, NA, NA, NA, NA, 100, 21.65, 3.25, 0.38,
                     100, 21.7, 3.46, 0.42, 100, 21.7, 3.46, 0.42, 100, 21.69,
                     3.45, 0.42, 100, 21.69, 3.45, 0.42, 100, 33.55, 5.86, 0.65,
                     100, 33.55, 5.86, 0.65, 100, 33.67, 6.07, 0.72, 100, 33.67,
                     6.07, 0.72, NA, NA, NA, NA, NA, NA),
        iso_theo = c(NA, NA, NA, NA, NA, NA, NA, NA, "M", "M+1", "M+2", "M+3",
                     "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M",
                     "M+1", "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M", "M+1",
                     "M+2", "M+3", "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2",
                     "M+3", "M", "M+1", "M+2", "M+3", NA, NA, NA, NA, NA, NA)
    )
    peakgroups <- data.frame(
        group_id = 1:15,
        pcgroup_id = c(3, 3, 3, 1, 2, 5, 6, 2, 1, 5, 8, 3, 4, 4, 7),
        adduct = c("[M+H-H2O]+", "[M+H]+", NA, NA, NA, NA, NA, NA, NA, NA, NA,
                   "[M+Na]+", "[M+H-H2O]+", "[M+Na]+", NA),
        cluster_id = c(1, 2, 2, 4, 5, 6, 7, 5, 4, 6, 11, 12, 13, 14, 15),
        iso = c("M", "M", "M+*", "M", "M", "M", "M", "M+*", "M+*", "M+*", "M",
                "M", "M", "M", "M"),
        mzmed = c(408.251325886321, 426.262125882248, 427.265534374579,
                  428.268122152242, 428.268197619998, 428.2675574767,
                  428.267840674347, 429.270612779717, 429.270808224298,
                  429.269607476748, 429.270782294993, 448.243903573737,
                  464.447379285654, 504.440256583885, 505.443684675365),
        mzmin = c(408.251325886321, 426.261908233279, 427.265397484755,
                  428.267772595199, 428.267855601901, 428.2675574767,
                  428.267840674347, 429.270339913969, 429.270423563444,
                  429.269607476748, 429.270782294993, 448.243644005027,
                  464.447304014051, 504.440032161331, 505.443534603),
        mzmax = c(408.251325886321, 426.262343531217, 427.265671264404,
                  428.268471709284, 428.268539638095, 428.2675574767,
                  428.267840674347, 429.270885645465, 429.271192885151,
                  429.269607476748, 429.270782294993, 448.244163142448,
                  464.447454557257, 504.44048100644, 505.44383474773),
        rtmed = c(286.278, 286.8085, 286.8085, 279.141, 259.046, 297.915,
                  308.494, 258.782, 279.141, 296.857, 306.904, 286.8085,
                  197.973, 197.7085, 197.444),
        rtmin = c(286.278, 286.807, 286.807, 278.875, 258.782, 297.915, 308.494,
                  258.782, 278.875, 296.857, 306.904, 286.807, 197.973, 197.444,
                  197.444),
        rtmax = c(286.278, 286.81, 286.81, 279.407, 259.31, 297.915, 308.494,
                  258.782, 279.407, 296.857, 306.904, 286.81, 197.973, 197.973,
                  197.444),
        npeaks = c(1, 2, 2, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 2, 2),
        `220221CCM_global_POS_01_ssleu_filtered` = c(NA, 6, 4, 7, 8, 12, 13, 10,
                                                     9, 11, NA, 5, 1, 2, 3),
        `220221CCM_global_POS_02_ssleu_filtered` = c(18, 21, 20, 22, 23, NA, NA,
                                                     17, 24, NA, 25, 19, 15, 16,
                                                     14),
        check.names = FALSE
    )
    peaks_mat <- matrix(
        c(464.447304014051, 504.440032161331, 505.443534603, 427.265397484755,
          448.243644005027, 426.261908233279, 428.267772595199,
          428.267855601901, 429.270423563444, 429.270339913969,
          429.269607476748, 428.2675574767, 428.267840674347, 505.44383474773,
          464.447454557257, 504.44048100644, 429.270885645465, 408.251325886321,
          448.244163142448, 427.265671264404, 426.262343531217,
          428.268471709284, 428.268539638095, 429.271192885151,
          429.270782294993, 464.447021484375, 504.439697265625,
          505.443084716797, 427.264739990234, 448.243011474609,
          426.261444091797, 428.267364501953, 428.267425537109,
          429.269836425781, 429.269653320312, 429.269256591797,
          428.267242431641, 428.267364501953, 505.440704345703,
          464.446746826172, 504.439544677734, 429.270324707031,
          408.251037597656, 448.242279052734, 427.264801025391,
          426.262023925781, 428.268035888672, 428.267822265625,
          429.270721435547, 429.270416259766, 464.447662353516,
          504.440490722656, 505.443664550781, 427.266052246094,
          448.245391845703, 426.262420654297, 428.268310546875,
          428.268615722656, 429.271362304688, 429.270904541016,
          429.269958496094, 428.268035888672, 428.2685546875, 505.444122314453,
          464.447967529297, 504.441101074219, 429.271575927734, 408.25146484375,
          448.245422363281, 427.266052246094, 426.262786865234,
          428.269195556641, 428.269287109375, 429.271636962891,
          429.271636962891, 197.973, 197.444, 197.444, 286.81, 286.81, 286.81,
          279.407, 258.782, 279.407, 258.782, 296.857, 297.915, 308.494,
          197.444, 197.973, 197.973, 258.782, 286.278, 286.807, 286.807,
          286.807, 278.875, 259.31, 278.875, 306.904, 196.386, 193.743, 196.386,
          284.695, 285.224, 284.695, 265.656, 250.85, 265.656, 250.85, 295.271,
          293.684, 304.789, 196.387, 195.858, 182.11, 250.85, 284.692, 280.99,
          283.105, 284.692, 265.656, 250.85, 265.127, 301.084, 200.617, 199.03,
          199.03, 291.04, 291.04, 291.04, 293.156, 264.598, 294.742, 264.598,
          307.966, 303.202, 310.081, 201.145, 199.559, 208.548, 264.069,
          287.864, 292.095, 292.623, 293.152, 291.037, 264.598, 291.037,
          309.549, 4945601.93026269, 1287181.56877954, 401071.227087501,
          1170639.95871094, 260064.992761365, 6139220.0505469, 21634957.3317308,
          7556081.77126924, 5360632.29847273, 1854836.50349039,
          450077.636764323, 2235868.3566111, 753309.518850004, 444013.097852865,
          5689144.27927454, 1458245.19191226, 1621835.871345, 88824.635233072,
          288524.169413714, 1186664.89020643, 6234084.85605467,
          19992518.2568646, 7375409.9176154, 4939357.04715561, 323001.699462891,
          4945487.79875439, 1287161.27013964, 401069.111887501,
          1170297.42409805, 251531.954604928, 6137759.54643873, 20459048.501706,
          6979304.74283053, 5183447.80457555, 1771087.19378474,
          359253.245444391, 1369338.9068287, 436642.365357695, 443940.884671369,
          5689141.10698883, 1458054.42380843, 1621823.181105, 88821.9918997387,
          288157.380699577, 1185672.72711171, 6233652.48268746,
          18599650.9700816, 6636339.71966301, 4303464.43650285,
          210066.951190656, 4130684, 1395886, 434808.75, 676902.5, 183977.625,
          3501824, 1096165, 647081.5, 266683.75, 158095.875, 90547.25, 387221,
          271995.5, 524308, 5734716, 1723138, 150635.375, 70041.5625,
          186253.875, 673140, 3489368, 1078367, 654868, 260907.75, 68280.5,
          9148, 24876, 434808, 2254, 2, 3876, 10, 6, 13, 8, 2, 1, 1, 4708,
          5734715, 14733, 150634, 70041, 719, 1279, 10826, 10, 6, 6, 1, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, 2, 3, 4, 5, 6,
          7, 7, 8, 8, 9, 10, 13, 1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 10, 14, 1, 2, 1,
          2, 4, 1, 1, 1, 0, 0, 1, 1, 3, 7, 2, 3, 1, 1, 2, 1, 1, 0, 1, 1, 3, 4,
          4, 4, 4, 6, 4, 10, 14, 10, 14, 12, 4, 4, 4, 14, 18, 14, 4, 6, 6, 6,
          10, 14, 10, 6, 68, 67, 67, 236, 236, 236, 222, 183, 222, 183, 255,
          257, 277, 67, 68, 68, 183, 235, 236, 236, 236, 221, 184, 221, 274, 64,
          63, 63, 232, 230, 232, 212, 169, 212, 169, 243, 253, 273, 63, 54, 50,
          169, 231, 230, 230, 230, 211, 170, 211, 268, 72, 71, 71, 240, 242,
          240, 232, 197, 232, 197, 267, 261, 281, 71, 82, 86, 197, 239, 242,
          242, 242, 231, 198, 231, 280, 65, 60, 65, 108, 104, 108, 136, 108,
          136, 108, 108, 104, 103, 65, 64, 38, 108, 108, 101, 105, 108, 136,
          108, 110, 100, 73, 70, 70, 120, 115, 120, 188, 134, 191, 134, 132,
          122, 113, 74, 71, 88, 133, 114, 122, 123, 124, 184, 134, 159, 116, 1,
          1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
          2),
        nrow = 25, ncol = 23, dimnames = list(
            c(), c("mz", "mzmin", "mzmax", "rt", "rtmin", "rtmax", "into",
                   "intb", "maxo", "sn", "egauss", "mu", "sigma", "h", "f",
                   "dppm", "scale", "scpos", "scmin", "scmax", "lmin", "lmax",
                   "sample")
        )
    )
    peaks <- as.data.frame(peaks_mat)
    groupidx <- list(
        18,
        c(6, 21),
        c(4, 20),
        c(7, 22),
        c(8, 23),
        12,
        13,
        c(10, 17),
        c(9, 24),
        11,
        25,
        c(5, 19),
        c(1, 15),
        c(2, 16),
        c(3, 14)
    )
    groups_mat <- matrix(
        c(408.251325886321, 426.262125882248, 427.265534374579,
          428.268122152242, 428.268197619998, 428.2675574767,
          428.267840674347, 429.270612779717, 429.270808224298,
          429.269607476748, 429.270782294993, 448.243903573737,
          464.447379285654, 504.440256583885, 505.443684675365,
          408.251325886321, 426.261908233279, 427.265397484755,
          428.267772595199, 428.267855601901, 428.2675574767,
          428.267840674347, 429.270339913969, 429.270423563444,
          429.269607476748, 429.270782294993, 448.243644005027,
          464.447304014051, 504.440032161331, 505.443534603,
          408.251325886321, 426.262343531217, 427.265671264404,
          428.268471709284, 428.268539638095, 428.2675574767,
          428.267840674347, 429.270885645465, 429.271192885151,
          429.269607476748, 429.270782294993, 448.244163142448,
          464.447454557257, 504.44048100644, 505.44383474773, 286.278,
          286.8085, 286.8085, 279.141, 259.046, 297.915, 308.494, 258.782,
          279.141, 296.857, 306.904, 286.8085, 197.973, 197.7085, 197.444,
          286.278, 286.807, 286.807, 278.875, 258.782, 297.915, 308.494,
          258.782, 278.875, 296.857, 306.904, 286.807, 197.973, 197.444,
          197.444, 286.278, 286.81, 286.81, 279.407, 259.31, 297.915,
          308.494, 258.782, 279.407, 296.857, 306.904, 286.81, 197.973,
          197.973, 197.444, 1, 2, 2, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 2, 2, 0,
          1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1,
          1, 0, 1, 1, 1, 1, 1),
        nrow = 15, ncol = 9, dimnames = list(
            c(), c("mzmed", "mzmin", "mzmax", "rtmed", "rtmin", "rtmax",
                   "npeaks", "1", "2")
        )
    )

    xset <- methods::new("xcmsSet")
    sample_names <- c("220221CCM_global_POS_01_ssleu_filtered",
                      "220221CCM_global_POS_02_ssleu_filtered")
    xset@phenoData <- data.frame(
        class = rep(runif(1), 2),
        row.names = sample_names
    )
    xcms:::filepaths(xset) <- c(
        system.file(
            "testdata",
            "220221CCM_global_POS_01_ssleu_filtered.mzML",
            package = "SPRING"
        ),
        system.file(
            "testdata",
            "220221CCM_global_POS_02_ssleu_filtered.mzML",
            package = "SPRING"
        )
    )

    # 1st test: with no peaks
    xset@groups <- matrix(, nrow = 0, ncol = 9, dimnames = list(
        c(), c("mzmed", "mzmin", "mzmax", "rtmed", "rtmin", "rtmax",
               "npeaks", "1", "2"))
    )
    xset@groupidx <- list()
    testthat::expect_error(
        CAMERA::annotate(
            xset,
            sigma = camera_params@sigma,
            perfwhm = camera_params@perfwhm,
            cor_eic_th = camera_params@cor_eic_th,
            graphMethod = camera_params@graph_method,
            pval = camera_params@pval,
            calcCiS = camera_params@calcCiS,
            calcIso = camera_params@calcIso,
            calcCaS = camera_params@calcCaS,
            maxcharge = camera_params@maxcharge,
            maxiso = camera_params@maxiso,
            minfrac = camera_params@minfrac,
            ppm = camera_params@ppm,
            mzabs = camera_params@mzabs,
            quick = FALSE,
            rules = camera_params@rules,
            polarity = camera_params@polarity,
            multiplier = camera_params@multiplier,
            max_peaks = camera_params@max_peaks,
            intval = camera_params@intval
        ),
        paste0("First argument must be a xcmsSet with group information or ",
               "contain only one sample.")
    )

    xset@peaks <- peaks_mat
    xset@groupidx <- groupidx
    xset@groups <- groups_mat
    invisible(utils::capture.output(xsa <- CAMERA::annotate(
        xset,
        sigma = camera_params@sigma,
        perfwhm = camera_params@perfwhm,
        cor_eic_th = camera_params@cor_eic_th,
        graphMethod = camera_params@graph_method,
        pval = camera_params@pval,
        calcCiS = camera_params@calcCiS,
        calcIso = camera_params@calcIso,
        calcCaS = camera_params@calcCaS,
        maxcharge = camera_params@maxcharge,
        maxiso = camera_params@maxiso,
        minfrac = camera_params@minfrac,
        ppm = camera_params@ppm,
        mzabs = camera_params@mzabs,
        quick = FALSE,
        rules = camera_params@rules,
        polarity = camera_params@polarity,
        multiplier = camera_params@multiplier,
        max_peaks = camera_params@max_peaks,
        intval = camera_params@intval
    )))

    # 2nd test: too restrictive on m/z tolerance
    empty_ann <- data.frame(ann[-c(4, 6, 7), ], row.names = NULL,
                            check.names = FALSE)
    empty_ann[, c("class", "name", "referent_adduct")] <- as.character(NA)
    empty_ann$rtdiff <- as.numeric(NA)
    empty_ann[, c("formula", "adduct", "ion_formula")] <- NA
    empty_ann[, c("best_score", "best_npeak")] <- 0
    empty_ann$best_deviation_mz <- NA_real_
    empty_spectra_infos <- spectra_infos
    empty_spectra_infos[, c("score", "npeak", "sum_int")] <- 0
    empty_spectra_infos$deviation_mz <- NaN
    empty_spectras <- data.frame(spectras[!is.na(spectras$mz), ],
                                 row.names = NULL)
    empty_spectras[, c("mz_theo", "abd_theo", "iso_theo")] <- NA
    ann_params_no_hits <- ann_params
    ann_params_no_hits@da_tol <- 10**-9
    xsf <- annotate_pcgroups(xsa, ann_params_no_hits)
    testthat::expect_equal(
        xsf$ann,
        empty_ann
    )
    testthat::expect_equal(
        xsf$spectra_infos,
        empty_spectra_infos
    )
    testthat::expect_equal(
        xsf$spectras,
        empty_spectras
    )
    testthat::expect_equal(
        xsf$peakgroups,
        peakgroups
    )
    testthat::expect_equal(
        xsf$peaks,
        peaks
    )

    # 3rd test : with a rT tolerance too restrictive
    ann_params_no_hits <- ann_params
    ann_params_no_hits@rt_tol <- 10**-9
    xsf <- annotate_pcgroups(xsa, ann_params_no_hits)
    testthat::expect_equal(
        xsf$ann,
        empty_ann
    )
    testthat::expect_equal(
        xsf$spectra_infos,
        empty_spectra_infos
    )
    testthat::expect_equal(
        xsf$spectras,
        empty_spectras
    )
    testthat::expect_equal(
        xsf$peakgroups,
        peakgroups
    )
    testthat::expect_equal(
        xsf$peaks,
        peaks
    )

    # 4th test : with a restriction on compound class
    ann_cer <- data.frame(ann[-c(4, 6, 7), ], row.names = NULL,
                            check.names = FALSE)
    ann_cer[-which(ann_cer$class == "Cer"),
              c("formula", "class", "name", "referent_adduct", "adduct",
                "ion_formula", "rtdiff")] <- NA
    ann_cer[-which(ann_cer$class == "Cer"), c("best_score", "best_npeak")] <- 0
    ann_cer[-which(ann_cer$class == "Cer"), "best_deviation_mz"] <- NA
    spectra_infos_cer <- spectra_infos
    spectra_infos_cer[
        !spectra_infos_cer$spectra_id %in% na.omit(unlist(
            ann[which(ann$class == "Cer"), 15:ncol(ann)])),
        c("score", "npeak", "sum_int")] <- 0
    spectra_infos_cer[
        !spectra_infos_cer$spectra_id %in% na.omit(unlist(
            ann[which(ann$class == "Cer"), 15:ncol(ann)])),
        "deviation_mz"] <- NaN
    spectras_cer <- data.frame(spectras[
        !is.na(spectras$mz) | spectras$spectra_id %in% na.omit(unlist(
            ann[which(ann$class == "Cer"), 15:ncol(ann)])), ], row.names = NULL)
    spectras_cer[
        !spectras_cer$spectra_id %in% na.omit(unlist(
            ann[which(ann$class == "Cer"), 15:ncol(ann)])),
        c("mz_theo", "abd_theo", "iso_theo")] <- NA

    ann_params_no_hits <- ann_params
    ann_params_no_hits@cpd_classes <- "Cer"
    xsf <- annotate_pcgroups(xsa, ann_params_no_hits)
    testthat::expect_equal(
        xsf$ann,
        ann_cer
    )
    testthat::expect_equal(
        xsf$spectra_infos,
        spectra_infos_cer
    )
    testthat::expect_equal(
        xsf$spectras,
        spectras_cer
    )
    testthat::expect_equal(
        xsf$peakgroups,
        peakgroups
    )
    testthat::expect_equal(
        xsf$peaks,
        peaks
    )

    # 5th test: normal
    xsf <- annotate_pcgroups(xsa, ann_params)
    testthat::expect_equal(
        xsf$ann,
        ann
    )
    testthat::expect_equal(
        xsf$spectra_infos,
        spectra_infos
    )
    testthat::expect_equal(
        xsf$spectras,
        spectras
    )
    testthat::expect_equal(
        xsf$peakgroups,
        peakgroups
    )
    testthat::expect_equal(
        xsf$peaks,
        peaks
    )
})

testthat::test_that("split conflicts", {
    ann <- data.frame(
        pcgroup_id = c(1, 2, 3, 3, 3, 3, 3, 3, 4, 4, 5, 6, 7, 8),
        basepeak_group_id = c(4, 5, 1, 1, 2, 2, 12, 12, 13, 14, 6, 7, 15, 11),
        formula = c(NA, NA, "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                    "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                    "C30H59N1O3", "C30H59N1O3", NA, NA, NA, NA),
        class = c(NA, NA, "LPC", "LPC", "LPC", "LPC", "LPC", "LPC", "Cer",
                  "Cer", NA, NA, NA, NA),
        name = c(NA, NA, "LPC 11:0", "LPC 11a:0", "LPC 11:0", "LPC 11a:0",
                 "LPC 11:0", "LPC 11a:0", "Cer (d18:1/C12:0)",
                 "Cer (d18:1/C12:0)", NA, NA, NA, NA),
        referent_adduct = c(NA, NA, "[M+H]+", "[M+H]+", "[M+H]+", "[M+H]+",
                         "[M+H]+", "[M+H]+", "[M+H-H2O]+", "[M+H-H2O]+", NA, NA,
                         NA, NA),
        adduct = c(NA, NA, "[M+H-H2O]+", "[M+H-H2O]+", "[M+H]+", "[M+H]+",
                   "[M+Na]+", "[M+Na]+", "[M+H-H2O]+", "[M+Na]+", NA, NA, NA,
                   NA),
        ion_formula = c(NA, NA, "C19H39N1O6P1", "C19H39N1O6P1", "C19H41N1O7P1",
                        "C19H41N1O7P1", "C19H40N1O7P1Na1", "C19H40N1O7P1Na1",
                        "C30H58N1O2", "C30H59N1O3Na1", NA, NA, NA, NA),
        rtdiff = c(NA, NA, 8.99149999999997, 4.19150000000002, 8.99149999999997,
                   4.19150000000002, 8.99149999999997, 4.19150000000002,
                   2.37300000000002, 2.37300000000002, NA, NA, NA, NA),
        rt = c(279.141, 259.046, 286.8085, 286.8085, 286.8085, 286.8085,
               286.8085, 286.8085, 197.973, 197.973, 297.915, 308.494, 197.444,
               306.904),
        rtmin = c(291.037, 264.598, 287.864, 287.864, 293.152, 293.152, 292.095,
                  292.095, 199.559, 208.548, 303.202, 310.081, 201.145,
                  309.549),
        rtmax = c(279.407, 259.31, 286.81, 286.81, 286.81, 286.81, 286.81,
                  286.81, 197.973, 197.973, 297.915, 308.494, 197.444, 306.904),
        nsamples = c(2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0),
        best_score = c(0, 0, 79.8211975097656, 79.8211975097656,
                       95.0912628173828, 95.0912628173828, 79.6432037353516,
                       79.6432037353516, 71.3979721069336, 71.1946487426758, 0,
                       0, 0, 0),
        best_deviation_mz = c(NA, NA, .0003662109375, .0003662109375,
                              .00048828125, .00048828125, .00018310546875,
                              .00018310546875, .0010986328125, .001312255859375,
                              NA, NA, NA, NA),
        best_npeak = c(0, 0, 1, 1, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0),
        `220221CCM_global_POS_01_ssleu_filtered` = c(1, 3, NA, NA, 6, 6, 8, 8,
                                                     10, 12, 14, 15, 16, NA),
        `220221CCM_global_POS_02_ssleu_filtered` = c(2, 4, 5, 5, 7, 7, 9, 9, 11,
                                                     13, NA, NA, 17, 18),
        check.names = FALSE
    )
    testthat::expect_equal(
        split_conflicts(ann[0, ]),
        list(
            no_conflicts = ann[0, ],
            conflicts = list()
        )
    )
    testthat::expect_equal(
        split_conflicts(ann[-which(ann$class == "LPC"), ]),
        list(
            no_conflicts = ann[-which(ann$class == "LPC"), ],
            conflicts = list()
        )
    )
    testthat::expect_equal(
        split_conflicts(ann[which(ann$class == "LPC"), ]),
        list(
            no_conflicts = ann[0, ],
            conflicts = list(
                split(
                    ann[which(ann$class == "LPC"), ],
                    ann[which(ann$class == "LPC"), "name"]
                )
            )
        )
    )
    testthat::expect_equal(
        split_conflicts(ann),
        list(
            no_conflicts = ann[-which(ann$class == "LPC"), ],
            conflicts = list(
                split(
                    ann[which(ann$class == "LPC"), ],
                    ann[which(ann$class == "LPC"), "name"]
                )
            )
        )
    )
})

testthat::test_that("get int in annotation df", {
    testthat::expect_equal(
        get_int_ann(
            data.frame(),
            data.frame(),
            nsamples = 0
        ),
        data.frame(matrix(, nrow = 0, ncol = 12,
            dimnames = list(c(),
                          c("PCGroup ID", "Group ID", "Class", "Name",
                            "rT (min)", "Diff rT (sec)", "Referent adduct",
                            "Adduct", "nSamples", "Best score (%)",
                            "Best m/z dev (mDa)", "Max iso")
            )
        ), check.names = FALSE)
    )
    testthat::expect_equal(
        get_int_ann(
            data.frame(
                pcgroup_id = 1,
                basepeak_group_id = 1,
                class = "LPC",
                name = "LPC 11:0",
                formula = "C19H40N1O7P1",
                referent_adduct = "[M+H]+",
                adduct = "[M+H-H2O]+",
                ion_formula = "C19H39N1O6P1",
                rtdiff = 9.52199999999993,
                rt = 286.278,
                rtmin = 284.692,
                rtmax = 287.864,
                nsamples = 1,
                best_score = 79.8211975097656,
                best_deviation_mz = 0.0003662109375,
                best_npeak = 1,
                `220221CCM_global_POS_01_ssleu_filtered` = NA,
                `220221CCM_global_POS_02_ssleu_filtered` = 1,
                check.names = FALSE
            ),
            data.frame(
                spectra_id = 1,
                score = 79.8211975097656,
                deviation_mz = 0.0003662109375,
                npeak = 1,
                basepeak_mz = 408.251325886322,
                basepeak_int = 88824.635233072,
                sum_int = 88824.635233072,
                sample = "220221CCM_global_POS_02_ssleu_filtered",
                rt = 286.278
            ),
            nsamples = 2
        ),
        data.frame(
            `PCGroup ID` = factor(1, levels = 1),
            `Group ID` = 1,
            Class = factor("LPC", levels = "LPC"),
            Name = "LPC 11:0",
            `rT (min)` = 4.77,
            `Diff rT (sec)` = 10,
            `Referent adduct` = "[M+H]+",
            Adduct = "[M+H-H2O]+",
            nSamples = 1,
            `Best score (%)` = 80,
            `Best m/z dev (mDa)` = 0,
            `Max iso` = 1,
            `220221CCM_global_POS_01_ssleu_filtered` = as.numeric(NA),
            `220221CCM_global_POS_02_ssleu_filtered` = 88824.635233072,
            check.names = FALSE
        )
    )

    testthat::expect_equal(
        get_int_ann(
            data.frame(
                pcgroup_id = 1,
                basepeak_group_id = 1,
                class = "LPC",
                name = "LPC 11:0",
                formula = "C19H40N1O7P1",
                referent_adduct = "[M+H]+",
                adduct = "[M+H-H2O]+",
                ion_formula = "C19H39N1O6P1",
                rtdiff = 9.52199999999993,
                rt = 286.278,
                rtmin = 284.692,
                rtmax = 287.864,
                nsamples = 1,
                best_score = 79.8211975097656,
                best_deviation_mz = 0.0003662109375,
                best_npeak = 1,
                `220221CCM_global_POS_01_ssleu_filtered` = NA,
                `220221CCM_global_POS_02_ssleu_filtered` = 1,
                check.names = FALSE
            ),
            data.frame(
                spectra_id = 1,
                score = 79.8211975097656,
                deviation_mz = 0.0003662109375,
                npeak = 1,
                basepeak_mz = 408.251325886322,
                basepeak_int = 88824.635233072,
                sum_int = 88824.635233072,
                sample = "220221CCM_global_POS_02_ssleu_filtered",
                rt = 286.278
            ),
            nsamples = 2,
            val = "mz"
        ),
        data.frame(
            `PCGroup ID` = factor(1, levels = 1),
            `Group ID` = 1,
            Class = factor("LPC", levels = "LPC"),
            Name = "LPC 11:0",
            `rT (min)` = 4.77,
            `Diff rT (sec)` = 10,
            `Referent adduct` = "[M+H]+",
            Adduct = "[M+H-H2O]+",
            nSamples = 1,
            `Best score (%)` = 80,
            `Best m/z dev (mDa)` = 0,
            `Max iso` = 1,
            `220221CCM_global_POS_01_ssleu_filtered` = as.numeric(NA),
            `220221CCM_global_POS_02_ssleu_filtered` = 408.251325886322,
            check.names = FALSE
        )
    )
})

testthat::test_that("summarise ann df", {
    ann <- data.frame(
        pcgroup_id = c(1, 2, 3, 3, 3, 3, 3, 3, 4, 4, 5, 6, 7, 8),
        basepeak_group_id = c(4, 5, 1, 1, 2, 2, 12, 12, 13, 14, 6, 7, 15, 11),
        formula = c(NA, NA, "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                    "C19H40N1O7P1", "C19H40N1O7P1", "C19H40N1O7P1",
                    "C30H59N1O3", "C30H59N1O3", NA, NA, NA, NA),
        class = c(NA, NA, "LPC", "LPC", "LPC", "LPC", "LPC", "LPC", "Cer",
                  "Cer", NA, NA, NA, NA),
        name = c(NA, NA, "LPC 11:0", "LPC 11a:0", "LPC 11:0", "LPC 11a:0",
                 "LPC 11:0", "LPC 11a:0", "Cer (d18:1/C12:0)",
                 "Cer (d18:1/C12:0)", NA, NA, NA, NA),
        referent_adduct = c(NA, NA, "[M+H]+", "[M+H]+", "[M+H]+", "[M+H]+",
                         "[M+H]+", "[M+H]+", "[M+H-H2O]+", "[M+H-H2O]+", NA, NA,
                         NA, NA),
        adduct = c(NA, NA, "[M+H-H2O]+", "[M+H-H2O]+", "[M+H]+", "[M+H]+",
                   "[M+Na]+", "[M+Na]+", "[M+H-H2O]+", "[M+Na]+", NA, NA, NA,
                   NA),
        ion_formula = c(NA, NA, "C19H39N1O6P1", "C19H39N1O6P1", "C19H41N1O7P1",
                        "C19H41N1O7P1", "C19H40N1O7P1Na1", "C19H40N1O7P1Na1",
                        "C30H58N1O2", "C30H59N1O3Na1", NA, NA, NA, NA),
        rtdiff = c(NA, NA, 8.99149999999997, 4.19150000000002, 8.99149999999997,
                   4.19150000000002, 8.99149999999997, 4.19150000000002,
                   2.37300000000002, 2.37300000000002, NA, NA, NA, NA),
        rt = c(279.141, 259.046, 286.8085, 286.8085, 286.8085, 286.8085,
               286.8085, 286.8085, 197.973, 197.973, 297.915, 308.494, 197.444,
               306.904),
        rtmin = c(291.037, 264.598, 287.864, 287.864, 293.152, 293.152, 292.095,
                  292.095, 199.559, 208.548, 303.202, 310.081, 201.145,
                  309.549),
        rtmax = c(279.407, 259.31, 286.81, 286.81, 286.81, 286.81, 286.81,
                  286.81, 197.973, 197.973, 297.915, 308.494, 197.444, 306.904),
        nsamples = c(2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0),
        best_score = c(0, 0, 79.8211975097656, 79.8211975097656,
                       95.0912628173828, 95.0912628173828, 79.6432037353516,
                       79.6432037353516, 71.3979721069336, 71.1946487426758, 0,
                       0, 0, 0),
        best_deviation_mz = c(NA, NA, .0003662109375, .0003662109375,
                              .00048828125, .00048828125, .00018310546875,
                              .00018310546875, .0010986328125, .001312255859375,
                              NA, NA, NA, NA),
        best_npeak = c(0, 0, 1, 1, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0),
        `220221CCM_global_POS_01_ssleu_filtered` = c(1, 3, NA, NA, 6, 6, 8, 8,
                                                     10, 12, 14, 15, 16, NA),
        `220221CCM_global_POS_02_ssleu_filtered` = c(2, 4, 5, 5, 7, 7, 9, 9, 11,
                                                     13, NA, NA, 17, 18),
        check.names = FALSE
    )
    spectra_infos <- data.frame(
        spectra_id =  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
                        17, 18),
        score = c(0, 0, 0, 0, 79.8211975097656, 95.0912628173828,
                  95.0683822631836, 79.6432037353516, 79.6432037353516,
                  71.3979721069336, 71.3979721069336, 71.1946487426758,
                  71.1946487426758, 0, 0, 0, 0, 0),
        deviation_mz = c(NaN, NaN, NaN, NaN, .0003662109375, .00048828125,
                         .0008392333984375, .00018310546875, .000701904296875,
                         .0010986328125, .001251220703125, .001312255859375,
                         .00177001953125, NaN, NaN, NaN, NaN, NaN),
        npeak = c(0, 0, 0, 0, 1, 2, 2, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
        basepeak_mz = c(428.267772595199, 428.268471709284, 428.267855601901,
                        428.268539638095, 408.251325886322, 426.261908233279,
                        426.262343531217, 448.243644005027, 448.244163142448,
                        464.447304014051, 464.447454557257, 504.440032161331,
                        504.44048100644, 428.2675574767, 428.267840674347,
                        505.443534603, 505.44383474773, 429.270782294993),
        basepeak_int = c(21634957.3317308, 19992518.2568646, 7556081.77126924,
                         7375409.9176154, 88824.635233072, 6139220.0505469,
                         6234084.85605467, 260064.992761365, 288524.169413714,
                         4945601.93026269, 5689144.27927454, 1287181.56877954,
                         1458245.19191226, 2235868.3566111, 753309.518850004,
                         401071.227087501, 444013.097852865, 323001.699462891),
        sum_int = c(0, 0, 0, 0, 88824.635233072, 7309860.00925784,
                    7420749.7462611, 260064.992761365, 288524.169413714,
                    4945601.93026269, 5689144.27927454, 1287181.56877954,
                    1458245.19191226, 0, 0, 0, 0, 0),
        sample = c("220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_01_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered",
                   "220221CCM_global_POS_02_ssleu_filtered"),
        rt  = c(279.407, 278.875, 258.782, 259.31, 286.278, 286.81, 286.807,
                286.81, 286.807, 197.973, 197.973, 197.444, 197.973, 297.915,
                308.494, 197.444, 197.444, 306.904)
    )

    # 1st test : no annotations !!!
    testthat::expect_equal(
        summarise_ann(ann[0, ], spectras[0, ], nsamples = 2),
        list(
            resume = data.frame(matrix(, nrow = 0, ncol = 10, dimnames = list(
                c(), c("PCGroup ID", "Class", "Name", "rT (min)",
                       "Diff rT (sec)", "Adducts", "nSamples", "Best score (%)",
                       "Best m/z dev (mDa)", "Max iso"))), check.names = FALSE),
            details = data.frame(matrix(, nrow = 0, ncol = 12, dimnames = list(
                c(), c("PCGroup ID", "Group ID", "Class", "Name", "rT (min)",
                       "Diff rT (sec)", "Referent adduct", "Adduct", "nSamples",
                       "Best score (%)", "Best m/z dev (mDa)", "Max iso"))),
                check.names = FALSE)
        )
    )

    # 2nd test : one annotation
    testthat::expect_equal(
        summarise_ann(
            ann[which(ann$name == "LPC 11:0"), ],
            spectra_infos,
            nsamples = 2
        ),
        list(
            resume = data.frame(
                `PCGroup ID` = factor(3, levels = 3),
                Class = factor("LPC", levels = "LPC"),
                Name = "LPC 11:0",
                `rT (min)` = 4.78,
                `Diff rT (sec)` = 9,
                Adducts = "[M+H-H2O]+ [M+H]+ [M+Na]+",
                nSamples = 2,
                `Best score (%)` = 95,
                `Best m/z dev (mDa)` = 0,
                `Max iso` = 2,
                `220221CCM_global_POS_01_ssleu_filtered` = 6139220.0505469,
                `220221CCM_global_POS_02_ssleu_filtered` = 6234084.85605467,
                check.names = FALSE
            ),
            details = data.frame(
                `PCGroup ID` = factor(c(3, 3, 3), levels = 3),
                Class = factor(c("LPC", "LPC", "LPC"), levels = "LPC"),
                Name = c("LPC 11:0", "LPC 11:0", "LPC 11:0"),
                `rT (min)` = c(4.78, 4.78, 4.78),
                `Diff rT (sec)` = c(9, 9, 9),
                Adduct = c("[M+H-H2O]+", "[M+H]+", "[M+Na]+"),
                nSamples = c(2, 2, 2),
                `Best score (%)` = c(80, 95, 80),
                `Best m/z dev (mDa)` = c(0, 0, 0),
                `Max iso` = c(1, 2, 1),
                `220221CCM_global_POS_01_ssleu_filtered` = c(NA,
                                                             6139220.0505469,
                                                             260064.992761365),
                `220221CCM_global_POS_02_ssleu_filtered` = c(88824.635233072,
                                                             6234084.85605467,
                                                             288524.169413714),
                check.names = FALSE
            )
        )
    )

    # 3rd test : one annotation but will all intensities summed
    testthat::expect_equal(
        summarise_ann(
            ann[which(ann$name == "LPC 11:0"), ],
            spectra_infos,
            nsamples = 2,
            by = "all"
        ),
        list(
            resume = data.frame(
                `PCGroup ID` = factor(3, levels = 3),
                Class = factor("LPC", levels = "LPC"),
                Name = "LPC 11:0",
                `rT (min)` = 4.78,
                `Diff rT (sec)` = 9,
                Adducts = "[M+H-H2O]+ [M+H]+ [M+Na]+",
                nSamples = 2,
                `Best score (%)` = 95,
                `Best m/z dev (mDa)` = 0,
                `Max iso` = 2,
                `220221CCM_global_POS_01_ssleu_filtered` = 6399285.043,
                `220221CCM_global_POS_02_ssleu_filtered` = 6611433.661,
                check.names = FALSE
            ),
            details = data.frame(
                `PCGroup ID` = factor(c(3, 3, 3), levels = 3),
                Class = factor(c("LPC", "LPC", "LPC"), levels = "LPC"),
                Name = c("LPC 11:0", "LPC 11:0", "LPC 11:0"),
                `rT (min)` = c(4.78, 4.78, 4.78),
                `Diff rT (sec)` = c(9, 9, 9),
                Adduct = c("[M+H-H2O]+", "[M+H]+", "[M+Na]+"),
                nSamples = c(2, 2, 2),
                `Best score (%)` = c(80, 95, 80),
                `Best m/z dev (mDa)` = c(0, 0, 0),
                `Max iso` = c(1, 2, 1),
                `220221CCM_global_POS_01_ssleu_filtered` = c(NA,
                                                             6139220.0505469,
                                                             260064.992761365),
                `220221CCM_global_POS_02_ssleu_filtered` = c(88824.635233072,
                                                             6234084.85605467,
                                                             288524.169413714),
                check.names = FALSE
            )
        )
    )

    # 4th test : with all possible annotations
    testthat::expect_equal(
        summarise_ann(ann, spectra_infos, nsamples = 2),
        list(
            resume = data.frame(
                `PCGroup ID` = factor(c(1:3, 3, 4:8), levels = 1:8),
                Class = factor(c(NA, NA, "LPC", "LPC", "Cer", NA, NA, NA, NA),
                               levels = c("Cer", "LPC")),
                Name = c(NA, NA, "LPC 11:0", "LPC 11a:0", "Cer (d18:1/C12:0)",
                         NA, NA, NA, NA),
                `rT (min)` = c(4.65, 4.32, 4.78, 4.78, 3.3, 4.97, 5.14, 3.29,
                               5.12),
                `Diff rT (sec)` = c(NA, NA, 9, 4, 2, NA, NA, NA, NA),
                Adducts = c(NA, NA, "[M+H-H2O]+ [M+H]+ [M+Na]+",
                            "[M+H-H2O]+ [M+H]+ [M+Na]+", "[M+H-H2O]+ [M+Na]+",
                            NA, NA, NA, NA),
                nSamples = c(2, 2, 2, 2, 2, 1, 1, 2, 1),
                `Best score (%)` = c(0, 0, 95, 95, 71, 0, 0, 0, 0),
                `Best m/z dev (mDa)` = c(NA, NA, 0, 0, 0, NA, NA, NA, NA),
                `Max iso` = c(0, 0, 2, 2, 1, 0, 0, 0, 0),
                `220221CCM_global_POS_01_ssleu_filtered` = c(21634957.3317308,
                                                             7556081.77126924,
                                                             6139220.0505469,
                                                             6139220.0505469,
                                                             4945601.93026269,
                                                             2235868.3566111,
                                                             753309.518850004,
                                                             401071.227087501,
                                                             NA),
                `220221CCM_global_POS_02_ssleu_filtered` = c(19992518.2568646,
                                                             7375409.9176154,
                                                             6234084.85605467,
                                                             6234084.85605467,
                                                             5689144.27927454,
                                                             NA, NA,
                                                             444013.097852865,
                                                             323001.699462891),
                check.names = FALSE
            ),
            details = data.frame(
                `PCGroup ID` = factor(c(1, 2, 3, 3, 3, 3, 3, 3, 4, 4, 5, 6, 7,
                                        8), levels = 1:8),
                Class = factor(c(NA, NA, "LPC", "LPC", "LPC", "LPC", "LPC",
                                 "LPC", "Cer", "Cer", NA, NA, NA, NA),
                               levels = c("Cer", "LPC")),
                Name = c(NA, NA, "LPC 11:0", "LPC 11a:0", "LPC 11:0",
                         "LPC 11a:0", "LPC 11:0", "LPC 11a:0",
                         "Cer (d18:1/C12:0)", "Cer (d18:1/C12:0)", NA, NA, NA,
                         NA),
                `rT (min)` = c(4.65, 4.32, 4.78, 4.78, 4.78, 4.78, 4.78, 4.78,
                               3.3, 3.3, 4.97, 5.14, 3.29, 5.12),
                `Diff rT (sec)` = c(NA, NA, 9, 4, 9, 4, 9, 4, 2, 2, NA, NA, NA,
                                    NA),
                Adduct = c(NA, NA, "[M+H-H2O]+", "[M+H-H2O]+", "[M+H]+",
                           "[M+H]+", "[M+Na]+", "[M+Na]+", "[M+H-H2O]+",
                           "[M+Na]+", NA, NA, NA, NA),
                nSamples = c(2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0),
                `Best score (%)` = c(0, 0, 80, 80, 95, 95, 80, 80, 71, 71, 0, 0,
                                     0, 0),
                `Best m/z dev (mDa)` = c(NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, NA,
                                         NA, NA, NA),
                `Max iso` = c(0, 0, 1, 1, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0),
                `220221CCM_global_POS_01_ssleu_filtered` = c(21634957.3317308,
                                                             7556081.77126924,
                                                             NA, NA,
                                                             6139220.0505469,
                                                             6139220.0505469,
                                                             260064.992761365,
                                                             260064.992761365,
                                                             4945601.93026269,
                                                             1287181.56877954,
                                                             2235868.3566111,
                                                             753309.518850004,
                                                             401071.227087501,
                                                             NA),
                `220221CCM_global_POS_02_ssleu_filtered` = c(19992518.2568646,
                                                             7375409.9176154,
                                                             88824.635233072,
                                                             88824.635233072,
                                                             6234084.85605467,
                                                             6234084.85605467,
                                                             288524.169413714,
                                                             288524.169413714,
                                                             5689144.27927454,
                                                             1458245.19191226,
                                                             NA, NA,
                                                             444013.097852865,
                                                             323001.699462891),
                check.names = FALSE
            )
        )
    )

    # 5th test : with all possible annotations but with all the ions intensities
        # summed
    testthat::expect_equal(
        summarise_ann(ann, spectra_infos, nsamples = 2, by = "all"),
        list(
            resume = data.frame(
                `PCGroup ID` = factor(c(1:3, 3, 4:8), levels = 1:8),
                Class = factor(c(NA, NA, "LPC", "LPC", "Cer", NA, NA, NA, NA),
                               levels = c("Cer", "LPC")),
                Name = c(NA, NA, "LPC 11:0", "LPC 11a:0", "Cer (d18:1/C12:0)",
                         NA, NA, NA, NA),
                `rT (min)` = c(4.65, 4.32, 4.78, 4.78, 3.3, 4.97, 5.14, 3.29,
                               5.12),
                `Diff rT (sec)` = c(NA, NA, 9, 4, 2, NA, NA, NA, NA),
                Adducts = c(NA, NA, "[M+H-H2O]+ [M+H]+ [M+Na]+",
                            "[M+H-H2O]+ [M+H]+ [M+Na]+", "[M+H-H2O]+ [M+Na]+",
                            NA, NA, NA, NA),
                nSamples = c(2, 2, 2, 2, 2, 1, 1, 2, 1),
                `Best score (%)` = c(0, 0, 95, 95, 71, 0, 0, 0, 0),
                `Best m/z dev (mDa)` = c(NA, NA, 0, 0, 0, NA, NA, NA, NA),
                `Max iso` = c(0, 0, 2, 2, 1, 0, 0, 0, 0),
                `220221CCM_global_POS_01_ssleu_filtered` = c(21634957.3317,
                                                             7556081.7713,
                                                             6399285.0433,
                                                             6399285.0433,
                                                             6232783.4990,
                                                             2235868.3566,
                                                             753309.5189,
                                                             401071.2271,
                                                             NA),
                `220221CCM_global_POS_02_ssleu_filtered` = c(19992518.2569,
                                                             7375409.9176,
                                                             6611433.6607,
                                                             6611433.6607,
                                                             7147389.4712,
                                                             NA, NA,
                                                             444013.097852865,
                                                             323001.699462891),
                check.names = FALSE
            ),
            details = data.frame(
                `PCGroup ID` = factor(c(1, 2, 3, 3, 3, 3, 3, 3, 4, 4, 5, 6, 7,
                                        8), levels = 1:8),
                Class = factor(c(NA, NA, "LPC", "LPC", "LPC", "LPC", "LPC",
                                 "LPC", "Cer", "Cer", NA, NA, NA, NA),
                               levels = c("Cer", "LPC")),
                Name = c(NA, NA, "LPC 11:0", "LPC 11a:0", "LPC 11:0",
                         "LPC 11a:0", "LPC 11:0", "LPC 11a:0",
                         "Cer (d18:1/C12:0)", "Cer (d18:1/C12:0)", NA, NA, NA,
                         NA),
                `rT (min)` = c(4.65, 4.32, 4.78, 4.78, 4.78, 4.78, 4.78, 4.78,
                               3.3, 3.3, 4.97, 5.14, 3.29, 5.12),
                `Diff rT (sec)` = c(NA, NA, 9, 4, 9, 4, 9, 4, 2, 2, NA, NA, NA,
                                    NA),
                Adduct = c(NA, NA, "[M+H-H2O]+", "[M+H-H2O]+", "[M+H]+",
                           "[M+H]+", "[M+Na]+", "[M+Na]+", "[M+H-H2O]+",
                           "[M+Na]+", NA, NA, NA, NA),
                nSamples = c(2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0),
                `Best score (%)` = c(0, 0, 80, 80, 95, 95, 80, 80, 71, 71, 0, 0,
                                     0, 0),
                `Best m/z dev (mDa)` = c(NA, NA, 0, 0, 0, 0, 0, 0, 0, 0, NA,
                                         NA, NA, NA),
                `Max iso` = c(0, 0, 1, 1, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0),
                `220221CCM_global_POS_01_ssleu_filtered` = c(21634957.3317308,
                                                             7556081.77126924,
                                                             NA, NA,
                                                             6139220.0505469,
                                                             6139220.0505469,
                                                             260064.992761365,
                                                             260064.992761365,
                                                             4945601.93026269,
                                                             1287181.56877954,
                                                             2235868.3566111,
                                                             753309.518850004,
                                                             401071.227087501,
                                                             NA),
                `220221CCM_global_POS_02_ssleu_filtered` = c(19992518.2568646,
                                                             7375409.9176154,
                                                             88824.635233072,
                                                             88824.635233072,
                                                             6234084.85605467,
                                                             6234084.85605467,
                                                             288524.169413714,
                                                             288524.169413714,
                                                             5689144.27927454,
                                                             1458245.19191226,
                                                             NA, NA,
                                                             444013.097852865,
                                                             323001.699462891),
                check.names = FALSE
            )
        )
    )
})

testthat::test_that("reintegrate ann", {
    temp_file <- tempfile(fileext = ".sqlite")
    invisible(file.copy(
        system.file("testdata", "220221CCM_global.sqlite", package = "SPRING"),
        temp_file
    ))
    db <- db_connect(temp_file)
    cpd_name <- "LPC 11:0"
    rtmin <- 283
    rtmax <- 294

    # 1st test : without db
    testthat::expect_error(
        reintegrate_ann(NULL),
        "db must be a connection to the sqlite database"
    )

    # 2nd test : with more than one compound
    testthat::expect_error(
        reintegrate_ann(db, c(1, 2)),
        "only one compound per reintegration !"
    )

    # 3rd test : cpd name is not a character
    testthat::expect_error(
        reintegrate_ann(db, 1),
        "cpd_name must be represent a compound name"
    )

    # 4th test : multiple rtmin
    testthat::expect_error(
        reintegrate_ann(db, cpd_name, c("a", "b")),
        "only one rtmin is required"
    )

    # 5th test : rtmin is not a numeric
    testthat::expect_error(
        reintegrate_ann(db, cpd_name, "a"),
        "rtmin must be a numeric"
    )

    # 6th test : multiple rtmax
    testthat::expect_error(
        reintegrate_ann(db, cpd_name, rtmin, c("a", "b")),
        "only one rtmax is required"
    )

    # 7th test : rtmax is not a numeric
    testthat::expect_error(
        reintegrate_ann(db, cpd_name, rtmin, "a"),
        "rtmax must be a numeric"
    )

    # 8th test : rtmax is lower than rtmin
    testthat::expect_error(
        reintegrate_ann(db, cpd_name, rtmin, 1),
        "rtmax must be upper than rtmin"
    )

    # 9th test : normal
    result <- reintegrate_ann(db, cpd_name, 283, 294)
    testthat::expect_equal(
        db_get_annotations(db, cpd_name),
        result$ann
    )
    testthat::expect_equal(
        result$ann,
        data.frame(
            pcgroup_id = rep(9, 3),
            basepeak_group_id = c(16, 17, 19),
            formula = rep("C19H40N1O7P1", 3),
            class = rep("LPC", 3),
            name = rep("LPC 11:0", 3),
            referent_adduct = rep("[M+H]+", 3),
            adduct = c("[M+H-H2O]+", "[M+H]+", "[M+Na]+"),
            ion_formula = c("C19H39N1O6P1", "C19H41N1O7P1", "C19H40N1O7P1Na1"),
            rtdiff = c(9.51899999999995, 9.51899999999995, 9.51899999999995),
            rt = rep(286.281, 3),
            rtmin = rep(283, 3),
            rtmax = rep(294, 3),
            nsamples = rep(2, 3),
            best_score = c(79.8211975097656, 95.6488800048828,
                           79.6432037353516),
            best_deviation_mz = c(0.000701904296875, 0.000823974609375,
                                  0.0003662109375),
            best_npeak = c(1, 2, 1),
            `220221CCM_global_POS_01_ssleu_filtered.mzXML` = c(19, 21, 23),
            `220221CCM_global_POS_02_ssleu_filtered.mzXML` = c(20, 22, 24),
            check.names = FALSE
        )
    )
    testthat::expect_equal(
        data.frame(result$spectra_infos, row.names = NULL),
        data.frame(
            spectra_id = 19:24,
            score = c(79.8211975097656, 79.8211975097656, 95.4294357299805,
                      95.6488800048828, 79.6432037353516, 79.6432037353516),
            deviation_mz = c(0.000701904296875, 0.0003662109375,
                             0.0004730224609375, 0.000823974609375,
                             0.0003662109375, 0.000823974609375),
            npeak = c(1, 1, 2, 2, 1, 1),
            basepeak_mz = c(408.251639652232, 408.251325886321,
                            426.261877129566, 426.262312513403,
                            448.243824189865, 448.24429856507),
            basepeak_int = c(86489.0094578489, 91433.9169842904,
                             5985640.48894737, 5970651.94634352,
                             249373.08941245, 276717.690733272),
            sum_int = c(86489.0094578489, 91433.9169842904, 7156280.44765831,
                        7157316.83654995, 249373.08941245, 276717.690733272),
            sample = c("220221CCM_global_POS_01_ssleu_filtered.mzXML",
                       "220221CCM_global_POS_02_ssleu_filtered.mzXML",
                       "220221CCM_global_POS_01_ssleu_filtered.mzXML",
                       "220221CCM_global_POS_02_ssleu_filtered.mzXML",
                       "220221CCM_global_POS_01_ssleu_filtered.mzXML",
                       "220221CCM_global_POS_02_ssleu_filtered.mzXML"),
            rt = rep(286.281, 6)
        )
    )
    testthat::expect_equal(
        result$spectras,
        data.frame(
            spectra_id = rep(19:24, each = 4),
            group_id = c(16, NA, NA, NA, 16, NA, NA, NA, 17, 3, NA, NA, 17, 3,
                         NA, NA, 19, NA, NA, NA, 19, NA, NA, NA),
            feature_id = c(26, NA, NA, NA, 27, NA, NA, NA, 28, 4, NA, NA, 30,
                           20, NA, NA, 32, NA, NA, NA, 33, NA, NA, NA),
            mz = c(408.251639652232, NA, NA, NA, 408.251325886321, NA, NA, NA,
                   426.261877129566, 427.265397484755, NA, NA, 426.262312513403,
                   427.265671264404, NA, NA, 448.243824189865, NA, NA, NA,
                   448.24429856507, NA, NA, NA),
            mzmin = c(408.251403808594, NA, NA, NA, 408.251037597656, NA, NA,
                      NA, 426.261444091797, 427.264739990234, NA, NA,
                      426.262023925781, 427.264801025391, NA, NA,
                      448.243377685547, NA, NA, NA, 448.244018554688, NA, NA,
                      NA),
            mzmax = c(408.251861572266, NA, NA, NA, 408.25146484375, NA, NA, NA,
                      426.262420654297, 427.266052246094, NA, NA,
                      426.262786865234, 427.266052246094, NA, NA,
                      448.244812011719, NA, NA, NA, 448.245422363281, NA, NA,
                      NA),
            rt = c(286.281, NA, NA, NA, 286.281, NA, NA, NA, 286.281, 286.81,
                   NA, NA, 286.281, 286.807, NA, NA, 286.281, NA, NA, NA,
                   286.281, NA, NA, NA),
            rtmin = c(283, NA, NA, NA, 283, NA, NA, NA, 283, 284.695, NA, NA,
                      283, 283.105, NA, NA, 283, NA, NA, NA, 283, NA, NA, NA),
            rtmax = c(294, NA, NA, NA, 294, NA, NA, NA, 294, 291.04, NA, NA,
                      294, 292.623, NA, NA, 294, NA, NA, NA, 294, NA, NA, NA),
            int = c(86489.0094578489, NA, NA, NA, 91433.9169842904, NA, NA, NA,
                    5985640.48894737, 1170639.95871094, NA, NA,
                    5970651.94634352, 1186664.89020643, NA, NA, 249373.08941245,
                    NA, NA, NA, 276717.690733272, NA, NA, NA),
            abd = c(100, NA, NA, NA, 100, NA, NA, NA, 100, 19.5574719342493, NA,
                    NA, 100, 19.8749634189137, NA, NA, 100, NA, NA, NA, 100, NA,
                    NA, NA),
            mz_theo = c(408.25095, 409.25427, 410.25672, 411.25935, 408.25095,
                        409.25427, 410.25672, 411.25935, 426.26152, 427.26484,
                        428.26719, 429.26984, 426.26152, 427.26484, 428.26719,
                        429.26984, 448.24346, 449.24678, 450.24914, 451.25178,
                        448.24346, 449.24678, 450.24914, 451.25178),
            abd_theo = c(100, 21.65, 3.25, 0.38, 100, 21.65, 3.25, 0.38, 100,
                         21.7, 3.46, 0.42, 100, 21.7, 3.46, 0.42, 100, 21.69,
                         3.45, 0.42, 100, 21.69, 3.45, 0.42),
            iso_theo = c("M", "M+1", "M+2", "M+3", "M", "M+1", "M+2", "M+3",
                         "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2", "M+3",
                         "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2", "M+3")
        )
    )
    testthat::expect_equal(
        result$peaks,
        data.frame(
            mz = c(408.251639652232, 408.251325886321, 426.261877129566,
                   426.262312513403, 448.243824189865, 448.24429856507),
            mzmin = c(408.251403808594, 408.251037597656, 426.261444091797,
                      426.262023925781, 448.243377685547, 448.244018554688),
            mzmax = c(408.251861572266, 408.25146484375, 426.262420654297,
                      426.262786865234, 448.244812011719, 448.245422363281),
            rt = c(286.281, 286.281, 286.281, 286.281, 286.281, 286.281),
            rtmin = c(283, 283, 283, 283, 283, 283),
            rtmax = c(294, 294, 294, 294, 294, 294),
            into = c(86489.0094578489, 91433.9169842904, 5985640.48894737,
                     5970651.94634352, 249373.08941245, 276717.690733272),
            intb = rep(NA, 6),
            maxo = c(76420.25, 70041.5625, 3501824, 3489368, 183977.625,
                     186253.875),
            sn = rep(NA, 6),
            egauss = rep(NA, 6),
            mu = rep(NA, 6),
            sigma = rep(NA, 6),
            h = rep(NA, 6),
            f = rep(NA, 6),
            dppm = rep(NA, 6),
            scale = rep(NA, 6),
            scpos = rep(NA, 6),
            scmin = rep(NA, 6),
            scmax = rep(NA, 6),
            lmin = rep(NA, 6),
            lmax = rep(NA, 6),
            sample = c(1, 2, 1, 2, 1, 2)
        )
    )
    testthat::expect_equal(
        result$peakgroups,
        data.frame(
            group_id = 16:19,
            pcgroup_id = rep(9, 4),
            adduct = rep(NA, 4),
            cluster_id = c(16, 17, 17, 18),
            iso = c("M", "M", "M+*", "M"),
            mzmed = c(408.251482769276, 426.262094821484, 427.26553437458,
                      448.244061377468),
            mzmin = c(408.251325886321, 426.261877129566, 427.265397484755,
                      448.243824189865),
            mzmax = c(408.251639652232, 426.262312513403, 427.265671264404,
                      448.24429856507),
            rtmed = c(286.281, 286.281, 286.8085, 286.281),
            rtmin = c(286.281, 286.281, 286.807, 286.281),
            rtmax = c(286.281, 286.281, 286.81, 286.281),
            npeaks = c(2, 2, 2, 2),
            `220221CCM_global_POS_01_ssleu_filtered.mzXML` = c(26, 28, 4, 32),
            `220221CCM_global_POS_02_ssleu_filtered.mzXML` = c(27, 30, 20, 33),
            check.names = FALSE
        )
    )

    # 10th test : partial annotation
    result <- reintegrate_ann(db, cpd_name, 319, 322)
    testthat::expect_equal(
        db_get_annotations(db, cpd_name),
        result$ann
    )
    testthat::expect_equal(
        result$ann,
        data.frame(
            pcgroup_id = rep(10, 3),
            basepeak_group_id = c(20, 21, 23),
            formula = rep("C19H40N1O7P1", 3),
            class = rep("LPC", 3),
            name = rep("LPC 11:0", 3),
            referent_adduct = rep("[M+H]+", 3),
            adduct = c("[M+H-H2O]+", "[M+H]+", "[M+Na]+"),
            ion_formula = c("C19H39N1O6P1", "C19H41N1O7P1", "C19H40N1O7P1Na1"),
            rtdiff = c(23.27, 24.328, 22.741),
            rt = c(319.07, 320.128, 319.8635),
            rtmin = c(319, 319, 319),
            rtmax = c(322, 322, 322),
            nsamples = c(1, 1, 2),
            best_score = c(79.8211975097656, 79.6305084228516,
                           79.6432037353516),
            best_deviation_mz = c(0.001708984375, -0.00042724609375,
                                  0.001739501953125),
            best_npeak = c(1, 1, 1),
            `220221CCM_global_POS_01_ssleu_filtered.mzXML` = c(25, NA, 27),
            `220221CCM_global_POS_02_ssleu_filtered.mzXML` = c(NA, 26, 28),
            check.names = FALSE
        )
    )
    testthat::expect_equal(
        data.frame(result$spectra_infos, row.names = NULL),
        data.frame(
            spectra_id = 25:28,
            score = c(79.8211975097656, 79.6305084228516, 79.6432037353516,
                      79.6432037353516),
            deviation_mz = c(0.001708984375, -0.00042724609375,
                             0.001739501953125, -0.000823974609375),
            npeak = rep(1, 4),
            basepeak_mz = c(408.252655029297, 426.261077784938,
                            448.245208740234, 448.242645263672),
            basepeak_int = c(114.037534790038, 2010.24735514322,
                             273.652672932941, 908.643385009759),
            sum_int = c(114.037534790038, 2010.24735514322, 273.652672932941,
                        908.643385009759),
            sample = c("220221CCM_global_POS_01_ssleu_filtered.mzXML",
                       "220221CCM_global_POS_02_ssleu_filtered.mzXML",
                       "220221CCM_global_POS_01_ssleu_filtered.mzXML",
                       "220221CCM_global_POS_02_ssleu_filtered.mzXML"),
            rt = c(319.07, 320.128, 318.541, 321.186)
        )
    )
    testthat::expect_equal(
        result$spectras,
        data.frame(
            spectra_id = c(25, 25, 25, 25, 26, 26, 26, 26, 27, 27, 27, 27, 28,
                           28, 28, 28),
            group_id = c(20, NA, NA, NA, 22, NA, NA, NA, 23, NA, NA, NA, 23, NA,
                         NA, NA),
            feature_id = c(32, NA, NA, NA, 34, NA, NA, NA, 35, NA, NA, NA, 36,
                           NA, NA, NA),
            mz = c(408.252655029297, NA, NA, NA, 426.261077784938, NA, NA, NA,
                   448.245208740234, NA, NA, NA, 448.242645263672, NA, NA, NA),
            mzmin = c(408.252655029297, NA, NA, NA, 426.261047363281, NA, NA,
                      NA, 448.245208740234, NA, NA, NA, 448.242645263672, NA,
                      NA, NA),
            mzmax = c(408.252655029297, NA, NA, NA, 426.261108398438, NA, NA,
                      NA, 448.245208740234, NA, NA, NA, 448.242645263672, NA,
                      NA, NA),
            rt = c(319.07, NA, NA, NA, 320.128, NA, NA, NA, 318.541, NA, NA, NA,
                   321.186, NA, NA, NA),
            rtmin = c(319, NA, NA, NA, 319, NA, NA, NA, 319, NA, NA, NA, 319,
                      NA, NA, NA),
            rtmax = c(322, NA, NA, NA, 322, NA, NA, NA, 322, NA, NA, NA, 322,
                      NA, NA, NA),
            int = c(114.037534790038, NA, NA, NA, 2010.24735514322, NA, NA, NA,
                    273.652672932941, NA, NA, NA, 908.643385009759, NA, NA, NA),
            abd = c(100, NA, NA, NA, 100, NA, NA, NA, 100, NA, NA, NA, 100, NA,
                    NA, NA),
            mz_theo = c(408.25095, 409.25427, 410.25672, 411.25935, 426.26152,
                        427.26484, 428.26719, 429.26984, 448.24346, 449.24678,
                        450.24914, 451.25178, 448.24346, 449.24678, 450.24914,
                        451.25178),
            abd_theo = c(100, 21.65, 3.25, 0.38, 100, 21.7, 3.46, 0.42, 100,
                         21.69, 3.45, 0.42, 100, 21.69, 3.45, 0.42),
            iso_theo = c("M", "M+1", "M+2", "M+3", "M", "M+1", "M+2", "M+3",
                         "M", "M+1", "M+2", "M+3", "M", "M+1", "M+2", "M+3")
        )
    )
    testthat::expect_equal(
        result$peaks,
        data.frame(
            mz = c(408.252655029297, 427.265825084667, 426.261077784938,
                   448.245208740234, 448.242645263672),
            mzmin = c(408.252655029297, 427.265533447266, 426.261047363281,
                      448.245208740234, 448.242645263672),
            mzmax = c(408.252655029297, 427.265991210938, 426.261108398438,
                      448.245208740234, 448.242645263672),
            rt = c(319.07, 320.657, 320.128, 318.541, 321.186),
            rtmin = c(319, 319, 319, 319, 319),
            rtmax = c(322, 322, 322, 322, 322),
            into = c(114.037534790038, 1021.8914880371, 2010.24735514322,
                     273.652672932941, 908.643385009759),
            intb = rep(NA, 5),
            maxo = c(258.686279296875, 1476.8359375, 1755.341796875,
                     620.76220703125, 1078.142578125),
            sn = rep(NA, 5),
            egauss = rep(NA, 5),
            mu = rep(NA, 5),
            sigma = rep(NA, 5),
            h = rep(NA, 5),
            f = rep(NA, 5),
            dppm = rep(NA, 5),
            scale = rep(NA, 5),
            scpos = rep(NA, 5),
            scmin = rep(NA, 5),
            scmax = rep(NA, 5),
            lmin = rep(NA, 5),
            lmax = rep(NA, 5),
            sample = c(1, 1, 2, 1, 2)
        )
    )
    testthat::expect_equal(
        result$peakgroups,
        data.frame(
            group_id = c(20, 21, 22, 23),
            pcgroup_id = c(10, 10, 10, 10),
            adduct = c(NA, NA, NA, NA),
            cluster_id = c(19, 20, 20, 21),
            iso = c("M", "M+*", "M", "M"),
            mzmed = c(408.252655029297, 427.265825084667, 426.261077784938,
                448.243927001953),
            mzmin = c(408.252655029297, 427.265825084667, 426.261077784938,
                448.242645263672),
            mzmax = c(408.252655029297, 427.265825084667, 426.261077784938,
                448.245208740234),
            rtmed = c(319.07, 320.657, 320.128, 319.8635),
            rtmin = c(319.07, 320.657, 320.128, 318.541),
            rtmax = c(319.07, 320.657, 320.128, 321.186),
            npeaks = c(1, 1, 1, 2),
            `220221CCM_global_POS_01_ssleu_filtered.mzXML` = c(32, 33, NA, 35),
            `220221CCM_global_POS_02_ssleu_filtered.mzXML` = c(NA, NA, 34, 36),
            check.names = FALSE
        )
    )

    # 11th test : no annotation cause no peaks
    result <- reintegrate_ann(db, cpd_name, 247, 250)
    testthat::expect_equal(
        nrow(db_get_annotations(db, cpd_name)),
        0
    )
    testthat::expect_equal(
        nrow(result$ann),
        nrow(db_get_annotations(db, cpd_name))
    )
    testthat::expect_identical(
        result$spectra_infos,
        data.frame()
    )
    testthat::expect_identical(
        result$spectras,
        data.frame()
    )
    testthat::expect_identical(
        result$peaks,
        data.frame()
    )
    testthat::expect_identical(
        result$peakgroups,
        data.frame()
    )

    RSQLite::dbDisconnect(db)
})
