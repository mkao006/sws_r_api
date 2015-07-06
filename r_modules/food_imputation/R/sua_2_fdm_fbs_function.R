
sua_2_fbs_fdm <- function(com_sua_cod){

  if(com_sua_cod %in% c(as.character(15:24), '41', '110', '114', '115')){
    
    return(c(foodFdm = 1, foodCommodityM = 2511))
  }

  if(com_sua_cod %in% c(as.character(44:50))){
    
    return(c(foodFdm = 4, foodCommodityM = 2513))
  }

  if(com_sua_cod %in% c(as.character(56:59), '63', '64', '846')){
    
    return(c(foodFdm = 3, foodCommodityM = 2514))
  }
  
  if(com_sua_cod %in% c(as.character(71:73))){
    
    return(c(foodFdm = 8, foodCommodityM = 3005))
  }
  
  if(com_sua_cod %in% c(as.character(75:77))){
    
    return(c(foodFdm = 5, foodCommodityM = 2516))
  }
  
  if(com_sua_cod %in% c(as.character(71:73))){
    
    return(c(foodFdm = 8, foodCommodityM = 3005))
  }
  
  if(com_sua_cod %in% c(as.character(79:81))){
    
    return(c(foodFdm = 6, foodCommodityM = 2517))
  }
  
  if(com_sua_cod %in% c(as.character(83:85))){
    
    return(c(foodFdm = 7, foodCommodityM = 2518))
  }
  
  if(com_sua_cod %in% c('68', as.character(89:92), as.character(94:99), 
                        '101', as.character(103:105), '108', as.character(111:113) )){
    
    return(c(foodFdm = 8, foodCommodityM = 3005))
  }

  if(com_sua_cod %in% c(as.character(116:119), '121')){
    
    return(c(foodFdm = 9, foodCommodityM = 2531))
  }
  
  if(com_sua_cod %in% c(as.character(125:129))){
    
    return(c(foodFdm = 11, foodCommodityM = 2532))
  }
  
  if(com_sua_cod %in% c('122')){
    
    return(c(foodFdm = 10, foodCommodityM = 2533))
  }
  
  if(com_sua_cod %in% c(as.character(135:136), as.character(149:151))){
    
    return(c(foodFdm = 14, foodCommodityM = 2534))
  }
  
  if(com_sua_cod %in% c('137')){
    
    return(c(foodFdm = 12, foodCommodityM = 2535))
  }
  
  if(com_sua_cod %in% c(as.character(156:157))){
    
    return(c(foodFdm = 17, foodCommodityM = 3006))
  }

  if(com_sua_cod %in% c('163')){
    
    return(c(foodFdm = 16, foodCommodityM = 2541))
  }
  
  if(com_sua_cod %in% c(as.character(158:159), '162', '164', '168', '171')){
    
    return(c(foodFdm = 15, foodCommodityM = 2542))
  }

  if(com_sua_cod %in% c(as.character(154:155), as.character(160:161), as.character(165:167),
                        as.character(172:173), '175', '633', '1182')){
    
    return(c(foodFdm = 17, foodCommodityM = 3006))
  }
  
  
  if(com_sua_cod %in% c('176', '187', '181', '191', '195', '197', '201', '203','205', as.character(210:213))){
    
    return(c(foodFdm = 18, foodCommodityM = 2911))
  }
  
  if(com_sua_cod %in% c('216', '217', as.character(220:226), as.character(229:235))){
    
    return(c(foodFdm = 19, foodCommodityM = 2912))
  }
  
  if(com_sua_cod %in% c('236', as.character(239:243), as.character(246:247), '267', '270', '292',
                        '295','329', as.character(249:251),'299', '254', '256', '260', '262',
                        '263', '265', '275', '277', '280', '296', '299', '305', as.character(310:312),
                        '333', '336', '339', '343')){
    
    return(c(foodFdm = 20, foodCommodityM = 2913))
  }
  
  if(com_sua_cod %in% c('237','244','268','271','293','331','258','257','1276','1277','252','290','261',
                        '274','36','60','264','266','276','278','281','297','306','307','313','334','337',
                        '340','664','1241','1242',as.character(1273:1275))){
    
    return(c(foodFdm = 40, foodCommodityM = 2914))
  }
  
  if(com_sua_cod %in% c(as.character(388:392),'403', '358','366','367','372','373','378','393',
                        '394','397','399','401',
                        '402','406','407','414','417','420','423','426','430',as.character(446:451),
                        '459','461',as.character(463:466),'469',as.character(471:476),'658','475','476','658')){
    
    return(c(foodFdm = 21, foodCommodityM = 2918))
  }
  
  if(com_sua_cod %in% c(as.character(490:492),'495', '496')){
    
    return(c(foodFdm = 22, foodCommodityM = 2611))
  }
  
  if(com_sua_cod %in% c(as.character(497:499))){
    
    return(c(foodFdm = 23, foodCommodityM = 2612))
  }

  if(com_sua_cod %in% c('507','509','510','512','513','514')){
    
    return(c(foodFdm = 24, foodCommodityM = 3000))
  }
  
  if(com_sua_cod %in% c('486')){
    
    return(c(foodFdm = 25, foodCommodityM = 2615))
  }
  
  if(com_sua_cod %in% c('489')){
    
    return(c(foodFdm = 13, foodCommodityM = 2616))
  }
  
  if(com_sua_cod %in% c(as.character(574:577),'580',as.character(560:563),'521','523','526','527','530','531',
                        '534',as.character(536:539),as.character(541:542),'544','547','549','550','552','554',
                        '558',as.character(567:572),'583','587','591','592','600','603','604','619','620',
                        as.character(622:626))){
    
    return(c(foodFdm = 26, foodCommodityM = 3007))
  }
  
  if(com_sua_cod %in% c('656','657','659')){
    
    return(c(foodFdm = 44, foodCommodityM = 2630))
  }
  
  if(com_sua_cod %in% c('667','671','672')){
    
    return(c(foodFdm = 45, foodCommodityM = 2635))
  }
  
  if(com_sua_cod %in% c('687','689','698','692','693','702','711','720','723')){
    
    return(c(foodFdm = 42, foodCommodityM = 2923))
  }

  if(com_sua_cod %in% c('564','565')){
    
    return(c(foodFdm = 47, foodCommodityM = 2655))
  }

  if(com_sua_cod %in% c('51')){
    
    return(c(foodFdm = 48, foodCommodityM = 2656))
  }

  if(com_sua_cod %in% c('26','29','66','82','86','517','634')){
    
    return(c(foodFdm = 49, foodCommodityM = 3004))
  }

  if(com_sua_cod %in% c('109')){
    
    return(c(foodFdm = 46, foodCommodityM = 2928))
  }

  if(com_sua_cod %in% c('867','870',as.character(872:877),'947')){
    
    return(c(foodFdm = 27, foodCommodityM = 2731))
  }

  if(com_sua_cod %in% c('977','1017')){
    
    return(c(foodFdm = 28, foodCommodityM = 2732))
  }
  
  if(com_sua_cod %in% c('1035','1038','1039','1041','1042')){
    
    return(c(foodFdm = 29, foodCommodityM = 2733))
  }
  
  
  if(com_sua_cod %in% c('1058','1060','1061','1069','1073','1080')){
    
    return(c(foodFdm = 30, foodCommodityM = 2734))
  }
  
  
  if(com_sua_cod %in% c('1089','1097','1108','1111','1127','1141','1151','1158','1163','1164','1166','1172',
                        '1176')){
    
    return(c(foodFdm = 31, foodCommodityM = 2735))
  }
  
  if(com_sua_cod %in% c('868','878','948','978','1018','1036','1059','1074','1075','1081','1098','1128',
                        '1159','1167')){
    
    return(c(foodFdm = 32, foodCommodityM = 2736))
  }
  

  if(com_sua_cod %in% c('869','871','949','979','994','1019','1037','1040','1043','1065','1066','1129','1160',
                        '1168','1221','1222','1225','1243')){
    
    return(c(foodFdm = 41, foodCommodityM = 2737))
  }
  
  if(com_sua_cod %in% c('886','887','952','953','983','1022')){
    
    return(c(foodFdm = 38, foodCommodityM = 2740))
  }

  if(com_sua_cod %in% c('885')){
    
    return(c(foodFdm = 39, foodCommodityM = 2743))
  }

  if(com_sua_cod %in% c('916','1062','1063','1064','1091')){
    
    return(c(foodFdm = 33, foodCommodityM = 2744))
  }


  if(com_sua_cod %in% c('1182 ')){
    
    return(c(foodFdm = 17, foodCommodityM = 3006))
  }
  
  if(com_sua_cod %in% c(as.character(1501:1508),as.character(1514:1521),as.character(1527:1534),as.character(1540:1547))){
    
    return(c(foodFdm = 34, foodCommodityM = 3001))
  }

  if(com_sua_cod %in% c(as.character(1553:1558),as.character(1570:1575),as.character(1562:1566))){
    
    return(c(foodFdm = 35, foodCommodityM = 3002))
  }

  if(com_sua_cod %in% c('1580','1583',as.character(1587:1590),as.character(1594:1596))){
    
    return(c(foodFdm = 36, foodCommodityM = 3003))
  }
  
  if(com_sua_cod %in% c('1509','1522','1535','1548','1582','1510','1523','1536','1549')){
    
    return(c(foodFdm = 41, foodCommodityM = 2737))
  }
  
  if(com_sua_cod %in% c(as.character(27:29),as.character(31:35),'38')){
    
    return(c(foodFdm = 2, foodCommodityM = 2805))
  }

  if(com_sua_cod %in% c('882',as.character(888:901),'903','904',as.character(907:910),'917','951','954',
                        '955','982','984','985',as.character(1020:1021),'1023','1130','905')){
    
    return(c(foodFdm = 37, foodCommodityM = 2848))
  }
  
}
