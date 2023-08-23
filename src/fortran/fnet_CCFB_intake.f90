module fnet_CCFB_intake

! a = 0.00076145
! b = 0.026005

intrinsic Reshape
real, dimension(8,126) :: input = &
  Reshape((/0.099114420081,0.025242080672,-0.030058174521,0.103682813005,0.114689691845,0.043221018284,0.171428174809,-0.002105394843,-0.045800661185,0.064377277909,0.161248744935,-0.026065832951 &
            ,-0.034324891475,0.017224074431,-0.131321202202,0.025661880778,-0.070581097389,-0.040682756785,-0.023290723901,-0.268881352000,0.037008258112,-0.085138291897,-0.154778198928,-0.336771726484 &
            ,0.060520314110,0.040065439285,0.133580806031,0.041408770763,0.097043139945,0.030456306679,-0.150260406853,-0.113873224793,-0.289037587678,0.119678822209,0.026936065543,-0.237667377709 &
            ,-0.226074122064,-0.045037609629,0.252701416544,-0.206217151615,0.090117307217,-0.088128512811,-0.089228577758,0.339389828639,0.305763470138,-0.030726190372,-0.067335689139,0.074453688215 &
            ,-0.045864736675,0.048036840180,0.187234147573,0.130228712364,-0.037981633429,0.051413907404,-0.201366804370,0.075366341257,-0.695084771078,-0.024369764555,0.126675591265,-0.311484567302 &
            ,-0.328556589968,0.010592661621,-0.237676297241,-0.202032449217,-0.129854746787,0.006182544079,0.021856801947,-0.148706528246,-0.164872199470,-0.035192574141,-0.036455214297,-0.111228473858 &
            ,-0.291705940511,-0.070166420963,-0.016817541392,0.248889038728,0.136989884649,0.140655851913,0.053303472981,0.330175057303,0.011823138918,-0.019428259317,0.016259541301,0.039811879402 &
            ,-0.121358067196,0.047248283932,-0.022056684371,0.104586068609,0.008804805946,-0.003103553497,-0.010034644261,0.000552400750,-0.030097389073,0.014344585416,0.031510248402,0.030744074119 &
            ,-0.009985702695,0.022369987356,-0.014426724539,-0.028107569312,-0.246267797805,-0.023592571021,0.081251106215,0.016917484436,0.022897355494,0.018472726332,0.016989031821,-0.249057387704 &
            ,-0.189895584537,0.010503518014,0.009545494325,0.010550786959,0.071171671693,0.063710631726,0.015219672251,-0.221030488465,-0.475694836728,0.040146378219,0.172771292906,0.042816899092 &
            ,-0.030606243296,0.036148907920,0.024296355647,-0.132129375610,-0.131589706698,-0.006113081198,0.029529743408,-0.116146273755,-0.146777855024,-0.044006182392,0.025197539905,0.243556552768 &
            ,0.060236691074,0.013896311412,-0.140758517598,0.079079093825,0.137435538105,0.079033255500,0.001632513210,0.014699536260,0.318115793704,-0.124579117258,0.053979061076,-0.291343111090 &
            ,1.233255647552,1.246503421474,1.102613362920,-0.036837179866,-0.034205135781,-0.033504417149,0.613302909018,-0.265997496526,1.688643904842,0.119408383158,-0.735362651030,-0.700360263378 &
            ,-0.665616873960,0.014269113138,1.714687270661,0.033245193890,0.305568866979,0.240119012453,1.119218166628,2.249523126613,0.074770657935,0.847151889153,-0.704896061648,2.472147925805 &
            ,0.204993624975,0.396747938814,-0.612615045079,-1.078034146266,-1.479547668396,-0.891256194450,1.567501315202,0.130465533284,1.705959546490,0.481968136294,-0.858889204698,-0.174027960429 &
            ,-0.580989653784,0.095745855869,3.220206366175,1.928976716024,0.589109709242,0.708426232185,1.208147119481,0.023787254293,-0.767874637608,0.387027789971,-0.655104845288,-0.081942119036 &
            ,1.101958063775,0.426179587302,-0.266230927403,-0.589644592754,-0.604912856187,-0.192425217081,1.280272558241,0.676642469617,-0.689697103753,-1.750400009685,-2.439735605483,0.824773856811 &
            ,-1.628636538892,1.817309753315,1.822680072884,1.655481475408,-0.795982685728,0.087637086438,0.043975124463,0.617401185998,-0.111086016177,0.209950241941,-0.097587937564,-0.412096358799 &
            ,3.596414448557,-0.391727855465,-0.301921951215,0.337309281313,-0.701763149722,0.987190284934,0.016202845272,2.470663731182,0.158766445321,-0.133773368902,-0.126328415881,0.267036557524 &
            ,0.320247994240,0.214534890955,0.093977560571,-0.236801346499,0.037854415126,-0.088804568328,0.034694642040,0.018172795310,0.034218454942,0.243640217745,-0.072606620954,0.118757597796 &
            ,0.441123759853,-0.150587184030,-0.040715850429,0.177303214582,-0.448033434592,0.211327783302,-0.133945809466,0.606869565346,0.164736950668,-0.186351403441,-0.024394456631,0.325097163750 &
            ,-0.620747988420,0.245887209489,-0.160466594770,0.777419058562,0.461546992796,-0.351795806710,-0.302331644629,-0.367370807763,-1.408692441138,0.040674882480,-0.237618705156,0.665207797393 &
            ,0.519241806771,-0.429849082129,-0.009726954627,0.840187022374,-0.457518083449,0.217460682747,-0.810860046337,0.746425150452,1.341532817927,-0.874742562374,-0.526761027288,0.612273908148 &
            ,0.247863376381,0.215360087687,-0.879902136455,0.590124983442,0.044017466984,-1.433488391421,-0.579953518711,2.345733451852,1.009242880925,0.436654207360,-1.962562739989,1.679362930686 &
            ,-1.220178064337,-0.993871307276,-0.484582047032,-1.049007392835,-2.320020139661,-0.206385179946,-1.515666710032,0.626304176960,-0.822521363410,-1.432074045610,-0.599838695036,-1.529515764398 &
            ,-3.162626260958,0.310637166626,-1.997768979226,1.932015724353,-0.995348105756,-2.085385928990,-1.847343915235,-2.450668660337,-3.976372720073,-0.333988635812,-1.413715190106,1.166088793294 &
            ,-2.242987820530,-0.229035892291,0.635591869587,-0.015464399173,-0.869594613868,1.084585855264,-0.554687507277,1.999378862182,-0.568429281236,0.308352230288,0.351640879944,0.064534864058 &
            ,0.500312212905,0.685961880041,1.067093370100,0.689006149954,0.106957013679,2.555179788351,2.447419698855,3.123707884671,4.486887775739,1.606781088195,3.082844151925,1.033734650529 &
            ,1.027783094260,2.285161217665,1.792765943528,2.043863453261,3.442056728372,0.341165583333,2.504484527844,-0.501520217176,2.595966366040,2.917521200382,2.903345635176,2.961908863117 &
            ,4.205772157133,0.678974607692,1.711726670018,0.670034868179,3.739166491420,-0.587224787404,-1.911714139023,-0.160716369743,-0.010172925321,-2.078989592618,0.159992525058,-1.832262664388 &
            ,0.845570524922,-0.655372557789,-0.982032849851,0.559387521635,-0.438541000872,-1.226901580787,-1.085309874025,-1.052248804843,-0.072448169651,-0.075697718264,-0.102287824526,-0.053875895171 &
            ,-0.017858577840,0.081152211253,0.074168021570,-0.237896368271,-0.001581908075,-0.170032903797,-0.261753932528,-0.337478501000,-0.356248315144,-0.238502859519,-0.129796584855,-0.217582522882 &
            ,0.080925611582,-0.263688448288,-0.266434708034,-0.279183325083,-0.596273332855,-0.053103277805,-0.218320363733,0.211715391305,0.006696292045,-0.170696155783,-0.342965697472,-0.061432841056 &
            ,-0.198225269615,-0.258959080082,0.001206355910,0.007023860106,0.555851321145,-0.234273112035,-0.323342278458,-0.310606046983,-0.211603282034,-0.166064867346,-0.116322834732,-0.356327207549 &
            ,-0.278401066636,-0.130305036031,-0.203281666939,-0.208925173234,-0.275488742128,-0.189809931311,-0.079891013585,-0.267367302689,-0.040103894820,-0.093238908645,-0.037479871210,-0.132066713592 &
            ,0.090527043964,0.040320015648,-0.118785195695,-0.002477983496,0.342928762497,-0.093370587649,-0.116443358382,-0.030225325110,-0.209663739202,-0.177369710633,-0.216731210136,-0.089871850302 &
            ,0.512400165906,1.871354701320,5.528801401389,-4.536501544004,0.860139417893,-1.868193375970,-10.089148028885,-8.585201095853,1.643046279482,-1.674497985540,-2.363188866540,1.652427735587 &
            ,0.422000564441,1.393676290165,3.153227932747,1.779975338805,6.845720637311,0.818229557899,-0.311120937885,1.258835462144,2.015424342194,-0.163470580233,3.171622386153,1.377416511035 &
            ,-5.579407782083,-2.246515778652,-0.570721646604,-0.995797187676,-2.221958553420,3.241204685958,-0.156682483331,0.905998385642,-7.980484629797,-2.868961083391,-2.719253798993,-1.293898637842 &
            ,-1.531822819812,1.431635914359,1.880572071791,0.547884513133,-6.394548997910,-2.188302843339,-2.325832746783,-0.330821788088,-1.238742672687,1.262379795750,1.333546394323,-1.150761865316 &
            ,0.967570337414,-1.105632271181,-2.027131586871,-2.539007886117,0.347609614554,-6.976187184388,-3.276343089476,-3.224226375564,7.941527343231,-0.449461601660,0.923351036560,-4.740163491641 &
            ,-1.069775764522,-7.572403239087,-4.259311370033,-6.032833114250,1.029787262019,3.315448643255,-0.346940181611,-0.309500981494,-1.307804000915,-10.374212708660,4.944526324071,2.575667713429 &
            ,-2.704238201898,1.204194951619,-4.425481755785,-0.838501466163,1.604338201460,-10.379541368536,6.604815954642,-3.355765311824,-0.459813333528,-1.112849185854,-0.732788208723,-3.508266876799 &
            ,-0.781227401848,2.033112573287,0.519066030972,-1.046438274348,-0.546028118735,-0.914548744067,0.152613277945,-0.225012895907,-0.683364862869,-0.176172145411,-1.420468399915,0.770709822754 &
            ,0.508456385393,0.447429213084,-1.526522693565,0.736892863963,0.474841973496,-2.138209683240,-1.025118861625,-0.025307569300,0.476510348896,-1.293302813309,-2.525728090630,0.630158261979 &
            ,1.427863552352,-4.640670915304,1.287923380799,-1.660786282218,-3.895547588640,0.270357516553,-5.517470424742,0.422914434958,1.443329339943,-4.553670306592,1.839700391434,-6.605789762920 &
            ,-6.102362639447,-6.152654561838,-6.253586752279,-3.259460919490,-4.229690156725,-1.930296433763,0.529479327321,-12.792266548760,-4.753818970774,-2.586107823993,-4.016823476936,-9.090193963615 &
            ,-5.688261819141,-3.000389360250,-1.238726506732,-5.342927461038,3.606589282559,4.331698097152,0.506489740201,-2.100929177167,-0.421180282302,-0.091596311268,10.181566438727,2.727677007938 &
            ,0.266684580010,-1.906085744562,-2.733155946740,2.042572987194,0.069264636922,-0.521653540771,1.344122762010,3.187327663000,1.698775688246,-1.717482547108,-1.716652743602,1.196884681174 &
            ,-0.032310577195,-0.844317915780,-1.231473988754,1.498918204967,-1.783925445892,-1.434451389199,-0.146380447585,0.745002281524,-0.043272770000,-2.234576462968,-3.885847965653,-0.525194056760 &
            ,-0.566459699110,0.754161268991,-0.516425756946,-0.452519788141,0.255436877210,-0.883587581975,2.536725804998,-1.484799491526,2.869176273804,2.523342443854,1.677933729807,0.231977906894 &
            ,-0.064074603600,-4.053103085492,3.165258868618,0.989937966031,1.262320238810,0.299525391547,2.127532757740,0.374834111262,0.145515824597,-7.907602895737,-1.985719540149,0.742501221535 &
            ,0.373348690637,0.134975781374,-0.086830843207,0.528689389209,-0.561880010990,-8.636928465761,1.513273009212,1.201274979498,-5.538156740274,-1.379339012265,-3.210852380697,-1.267011377254 &
            ,-0.777951728845,-17.461262368701,4.905933782775,-1.193948312181,-2.060817965934,-0.955143915521,-0.182086132891,-1.273475603422,-0.211015262360,-23.310847079834,-2.189467654820,-1.651224026920 &
            ,1.593288765049,-0.485649581085,0.580796448052,1.018752638375,-0.254370349701,-7.378953108644,-2.031358426520,1.252414145240,-0.752010538335,0.179566691334,-0.300307258216,-0.884770667417 &
            ,-0.246943071920,-1.003176756183,0.988972673189,-0.549106004581,-0.170368790709,0.141290872916,0.155099593646,0.607252392377,0.360602523560,-0.708121699284,-0.048620855612,-0.093782269027 &
            ,-0.185234429002,-0.050471173562,0.028029207576,0.020188123511,-0.286733593517,0.038790901270,0.092241524285,0.304992146619,0.066611677617,0.013510975628,0.072579375017,0.296133885126 &
            ,-0.398294672923,-0.337145742719,-0.172384643345,0.459293743933,-0.192446043658,-0.304531651773,-0.280436335793,0.329336636859,-1.260354923226,-0.095913076315,0.037144192692,0.815705381601 &
            ,0.896269011910,-0.033923328996,0.557676185073,1.655384361445,-0.467507102457,0.682196608887,-0.239427186252,2.359257143970,0.652809178194,1.163919817112,0.458770291381,-2.838709394999 &
            ,-2.671065493663,-1.402080530861,0.466702563409,-1.804983029564,-1.499557831439,-1.412062096256,-1.361721276429,-0.314095902373,1.957498672552,0.933266579586,-0.169091932748,-0.451949306446 &
            ,0.858615536341,0.319879256634,-0.380399495722,-0.380518080311,0.601778720409,-0.321167088858,1.320814655346,-0.920301461247,0.906946404975,0.308322063617,0.533385224019,-0.296417569588 &
            ,-0.513850392180,0.305227282413,-0.244025866394,0.035554145526,-1.136220612725,-0.866134445440,-0.216969825833,0.149979179237,-0.422257176591,0.263554235398,-1.497792192955,0.449896292766 &
            ,0.348247514760,0.006367449125,-0.541376983310,-0.504238728680,0.341504397687,-0.246081658447,0.922656407384,-0.760539416093,0.339090029848,0.016990132485,0.123049063645,0.657117271016 &
            ,0.767977242663,-0.012083191585,-0.527541875305,0.313252690830,1.180092790830,-0.098526918072,0.134014912128,0.350910014259,0.426838943935,0.452224699499,-0.858987195965,-0.134896490205 &
            ,-0.250538903935,-0.201086748250,-0.247757573591,-0.743301602583,-0.207655871267,0.688118245619,0.097871108149,-0.526175174733,-1.154699473555,0.389194514007,0.431530440676,1.489824302032 &
            ,0.831257233724,0.826929167737,-0.071765159753,0.401076379215,1.124427825225,0.132507159769,-0.014425342127,1.359137933151,0.715575712214,2.008966097972,0.687449891504,-0.152619903858 &
            ,2.259319154819,-0.432342639282,0.202755873868,-0.212051045910,-1.116966727258,1.822539624405,-0.759505068744,1.759592318220,-0.026534699699,-0.148939214542,0.006877880339,0.658530338494 &
            ,0.303776793644,0.124903656940,-0.299744146832,-0.268224334854,0.131368090590,0.200239579105,0.226036089494,-0.178132186713,-0.171445115925,0.309550105134,0.334156886137,0.214887928213 &
            ,0.216841947533,0.072208283882,0.167704499602,-0.355191675568,-0.260428980902,0.141774478923,-0.081622602061,-0.054830264393,0.067542493158,0.158653248999,0.210386574484,-0.581602341214 &
            ,-0.007638001797,0.137059308564,0.068989292085,0.107647682325,0.418946628014,0.110595121081,0.236172087176,-0.885543394207,0.006958839479,0.215134394140,-0.094270777002,0.775323061125 &
            ,-0.010265056054,-0.178420025805,-0.096494107537,1.130238288136,0.155924544723,0.252104808916,0.004157853615,0.975261671918,-0.655959787112,-0.321065702143,-0.044318688097,3.273266477585 &
            ,1.032471391895,0.494924203800,-0.054604776041,0.045973896782,0.833861218630,0.100526643700,0.348176117058,2.782830715622,-0.233740882228,0.492213828408,0.137928194629,2.089726357285 &
            ,-0.051405225214,0.110969650095,0.117981288817,0.023708193186,0.236534472586,0.049781187632,0.091685311364,0.047701126503,-0.036302945928,0.127671459923,-0.036644301197,0.176824521151 &
            ,0.231571309710,0.028538613441,0.486520602956,0.153317614746,0.197826573686,0.238868351870,0.055811739254,0.161073749179,0.097315050069,-0.027534883321,0.512056514666,0.208169103967 &
            ,0.028375824908,0.137127178212,0.022776462829,0.295739773543,0.600386542776,0.070113070358,0.413208168440,0.110183851756,0.183160737284,-0.004062917599,0.112776112152,0.157682392397 &
            ,-0.124253330515,0.220054793319,-0.000768202025,0.585404619253,-0.003564132529,-0.025392457577,-0.035442381189,0.060323601179,0.036379702956,0.201336914596,0.206865201126,0.255689685146 &
            ,0.064446342634,-0.167113247701,-0.114290361916,-0.325704927057,-0.625482970404,0.072789304555,-0.125227992081,0.144002499831,-0.241526901569,0.026458713726,0.129207701688,-0.300296959484 &
            ,-0.321471074141,0.101882533639,-0.000689999243,0.345368534854,-0.544295216350,0.012858760174,0.086768111607,-0.293146250789,-0.235698937587,-0.144833400103,-0.061481229154,0.340934073434 &
            ,-0.983793011790,0.218811116028,0.380130192708,1.165330749955,0.412419896525,-0.123537179479,0.065517794788,0.348600025339,-0.016087493422,0.172002832954,0.229446687625,1.709655290346 &
            ,0.747976414568,0.123642070669,0.144909355794,-0.420677768164,-0.007504461071,0.034133806978,0.039625071497,0.673303133740,0.189888061410,-0.033228045469,0.007384358078,-0.187261976599 &
            ,-0.136038055196,-0.339793495496,-0.447593419904,-14.582952393159,-7.699390172442,-0.013640068487,-0.157472543573,3.212161410925,0.110198909450,-0.020440818764,-0.020880150677,0.123206569830 &
            ,0.048887394380,-0.076130175699,-0.072265629714,-0.108312387070,0.033249332349,-0.017672868129,-0.011962200398,0.056586491375,0.051316527818,-0.004355378689,-0.025975547670,-0.143504017547 &
            ,0.013528155609,-0.007828441593,0.010919704122,-0.052024310904,0.023154804908,0.003965119427,-0.042495421787,-0.320535639643,-0.267218195874,-0.061512987164,-0.058493098333,0.310390051000 &
            ,-0.156379326667,0.021923020320,0.012594804885,0.869472034110,-0.145940347084,-0.108597315165,-0.090520426495,0.084981995463,0.069151129834,-0.000581071891,-0.092384303118,0.334209191589 &
            /),(/8,126/))
real, dimension(2,8) :: hidden1 = &
  Reshape((/-11.0502,0.29823 &
            ,19.4228,-0.98007,-16.2951,1.0464 &
            ,2.7871,-1.5882 &
            ,-2.7354,1.8189 &
            ,8.7921,-0.4842 &
            ,-7.092,0.15496 &
            ,5.2375,0.81916 &
            /),(/2,8/))
real, dimension(1,2) :: hidden2 = &
  Reshape((/1.0623,3.6121/),(/1,2/))
real, dimension(8) :: bias1 = &
  (/-4.2685,2.2421,4.5644,-2.3673,9.5714,5.8206,-7.036,-5.425/)
real, dimension(2) :: bias2 = &
  (/3.4149,-4.0503/)
real, dimension(1) :: bias3 = &
  (/-0.005787/)
contains
subroutine fnet_CCFB_intake_initall()
end subroutine fnet_CCFB_intake_initall
subroutine fnet_CCFB_intake_engine(inarray, outarray, init)
  intrinsic MatMul, Size
  real, dimension(:), intent(in) :: inarray
  real, dimension(:), intent(inout) :: outarray
  real, dimension(126) :: inarray2
  real (kind=8), dimension(8) :: layer1
  real (kind=8), dimension(2) :: layer2
  real (kind=8), dimension(1) :: layer3
  integer , intent(inout) :: init
  integer :: i, j
  do i = 1, 126
    inarray2(i) = inarray(127-i)
  end do
  layer1 = MatMul(input,inarray2)
  layer1 = layer1 + bias1
  do i = 1, Size(layer1,1)
    layer1(i) = 1.0 / (1.0 + DEXP(-1.0 * layer1(i)))
  end do
  layer2 = MatMul(hidden1,layer1)
  layer2 = layer2 + bias2
  do i = 1, Size(layer2,1)
    layer2(i) = 1.0 / (1.0 + DEXP(-1.0 * layer2(i)))
  end do
  layer3 = MatMul(hidden2,layer2)
  layer3 = layer3 + bias3
  outarray(1) = layer3(1)
end subroutine fnet_CCFB_intake_engine
end module fnet_CCFB_intake
