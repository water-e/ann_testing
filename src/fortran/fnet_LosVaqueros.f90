module fnet_LosVaqueros

! a = 0.00065188
! b = 0.034061

intrinsic Reshape
real, dimension(8,126) :: input = &
  Reshape((/-0.369170660866,-0.035291488123,-0.080732525705,-0.258817555588,0.165065322513,0.312540152775,0.176262026956,0.043545921675,0.299552966496,0.103061176965,0.140578993615,0.187109117071 &
            ,0.123588292944,-0.479285122132,0.069850335034,-0.160352931671,0.384120826050,-0.103714754250,-0.132025695996,0.193746379773,-0.391632009686,-0.258356316462,-0.333232175560,-0.066404888592 &
            ,-0.145730006661,0.040717926985,0.022751420452,-0.014978186676,0.108210496622,-0.305149011397,-0.087330805863,0.123674094189,-0.260006895840,-0.039027988051,0.037435479029,-0.202575212823 &
            ,0.140718218117,0.120387268759,0.173290402504,-0.071291618758,-0.466565423963,-0.103650366916,0.016846009522,-0.292118149798,-0.134216557123,0.368274415991,0.030473812541,-0.035346743777 &
            ,0.008048111777,0.087897229062,-0.111525294186,0.043404350582,0.102651662862,-0.084215153772,0.187041684036,0.123148915451,1.248653349319,0.026127851380,-0.024632124930,0.690800308493 &
            ,-0.074026973241,0.748926767628,-0.255099529541,-0.185058752377,0.394652299272,-0.078248370165,-0.042216869026,0.256859363705,-0.375323889169,1.156003315364,-0.145832026607,0.110071799721 &
            ,-0.062069763230,-0.167406066730,0.021167904414,-0.022172828445,-0.248908091504,0.103608074470,-0.190287814510,0.246381723919,0.058307449338,0.006805802579,0.035638359012,0.021359457964 &
            ,-0.072707084606,-0.098973751848,0.048668048080,-0.103692645843,0.106468277854,0.012547633673,0.027299871272,0.072108735476,-0.017499942738,0.163322892316,-0.038220039796,-0.005476704206 &
            ,-0.069773109537,0.052069867325,0.008842704483,-0.043839503400,0.153446610210,-1.188754578342,0.087126845493,-0.060451642089,-0.032536749902,0.094037931098,-0.016049718801,-0.003460651328 &
            ,0.170986134670,-0.530164297459,0.118077054988,-0.042471422821,-0.089106356624,0.010612684366,0.006191569208,-0.045562418300,0.039804685606,0.165807834396,-0.041754799901,-0.051396742030 &
            ,0.122895304345,-0.144722820403,0.034333328196,0.061165063575,-0.433319063160,0.678930706201,-0.213259540195,0.039857069121,0.017668853988,0.031162249806,0.033345966963,0.040794110130 &
            ,-0.069808833677,0.183610606019,0.015991160742,0.020972786789,0.240369710642,-0.069022635942,-0.082046886149,0.112632605303,-0.271586432658,-0.249237571299,-0.144505140413,0.002220346474 &
            ,-0.243324949470,0.429577539520,-0.925814428598,0.051869352635,0.246544819717,-3.792322286336,0.993870950707,2.002033589973,-1.463193259975,0.221039288016,0.295242084688,-0.908258522223 &
            ,1.179008296074,-0.866333004138,1.345686679697,-0.064215127813,-0.839356964811,0.018888879839,0.469565755819,-0.308505637310,-0.729402092176,-1.457304003851,0.378038827591,0.047453731170 &
            ,0.593266876058,0.079738005081,-0.155168575712,0.327725615505,0.150579115311,1.201787131057,-0.169611628158,0.567669512075,-1.020772848814,-0.647735116640,-0.523470637653,-0.543138909742 &
            ,-1.125004456303,0.696261223573,-0.850799103013,1.679404784333,1.330635995296,0.535098959246,1.602475794994,0.859995047414,0.973636120085,-3.520901356479,1.900700063302,0.658199084940 &
            ,0.430950968952,-0.046141436948,-1.808444400367,-0.261625337814,0.440455775810,-1.082537565222,1.073030858137,1.450116465999,-2.894631020913,-0.709244860286,2.699785241292,-1.408253344737 &
            ,-2.005403879397,1.057343263460,-0.307935568517,0.982689954017,0.257366046474,-0.337334925521,3.328836098311,0.249658298032,-0.268356724095,-0.522539462518,-0.213555486402,0.946210790462 &
            ,-1.534503648485,-0.356411186203,0.236729081232,-1.480006576356,0.813911435417,-4.485704018937,3.029626494031,1.185176060846,-1.388596274774,-0.280885117159,0.408004818820,-0.886673372999 &
            ,-0.145187839469,0.077856284562,0.101963106348,-0.542757396195,-0.326556927372,-0.208512352679,0.113078818528,-0.306027168127,-0.336772697316,-0.689018811835,0.236695505250,0.017953060373 &
            ,-0.610264506069,-0.111965395153,-0.099120983176,-0.427457776103,-0.108299379284,-0.872949183546,0.457692128072,-0.015942534440,0.033789221653,-0.208977054953,0.075929734653,0.000378330971 &
            ,-0.312757574513,-0.983019749104,-0.251855224312,-0.147337336327,-0.079985685729,-0.300041186069,0.044300060028,-0.077732102187,-0.774077544022,-1.490178415195,-0.266849817210,-0.093416971702 &
            ,-0.408788120038,-0.119097325197,-0.067510787787,-0.264636417876,0.283692468395,-0.790597196968,0.319419935971,0.252017119594,-0.947822424688,-0.273233924674,-0.418712421082,-0.642882470955 &
            ,-0.772420809602,-0.718698912779,0.255943500423,-0.005307479245,-0.768501367579,-0.205038170472,-0.411848306471,-0.552292025781,0.222156866374,1.314281207442,0.281812261654,0.431110384867 &
            ,0.633210476973,-0.246490818696,0.163900762449,0.428712334880,-0.780029788728,-4.534754363549,-1.202673095695,-0.991246702974,0.611939548589,-0.240872291461,-0.227887813767,0.065207824961 &
            ,0.334460844194,-3.128819183572,-0.076614776219,-1.585293050430,1.500077569173,-0.277134681519,-0.127269428819,0.669479237659,-1.320773170954,-9.340739785771,-0.253000340992,-1.670173991965 &
            ,1.817477295491,0.131579552850,-0.124464405396,0.981863030021,-0.073879833699,-4.719293357273,-0.071991712333,-0.961767576959,2.081422635087,-0.065353060741,-1.010200220008,1.183940083542 &
            ,-1.819137522847,-5.463767801919,-0.971363716804,-0.288619281985,0.835996614846,0.623400263864,-0.414983076197,0.738727644198,0.158503739627,-0.656850855229,-0.125948072169,1.163864747499 &
            ,0.118859635270,0.293965729894,-0.455301789250,0.450298630421,-0.580031614799,-4.126161603705,-1.639952103077,1.310182193608,-0.501624902125,0.171091696582,-0.892156948671,-0.077115025595 &
            ,0.741109693704,1.214322849394,-1.710250145898,1.551217294165,-0.624582999712,-0.304361324206,0.352437897831,-0.461578217278,0.043042707382,-7.878522082246,-0.347299813476,0.171246708638 &
            ,-0.174985897580,-0.049678188811,1.518550934676,-0.142451048148,1.402844878430,-6.541688551555,0.065257525111,-0.637224449074,-0.163037381824,0.127418930195,0.184806202931,-0.074556926746 &
            ,0.219182500951,-1.168491666295,0.467164416660,0.093434267971,-0.035938331962,-0.042325945782,0.028856284036,-0.025996056593,0.266481952725,0.343364883117,-0.205064300441,-0.060490510630 &
            ,-0.129880266827,-0.072685155607,0.268290366152,-0.114007330908,-0.142344196462,-1.037948006580,0.113774964799,-0.237436802664,-0.181067180172,0.021963705772,0.202315023839,-0.144335633371 &
            ,0.291010735342,-0.665459576731,0.345716745732,-0.147724979013,-0.078755240681,-0.068564055919,-0.023122676224,-0.042154832186,-0.107483494053,-0.461351139460,-0.237592051340,-0.099405363206 &
            ,-0.046192588231,0.035297923030,0.167994668258,-0.039702271661,0.082702505215,-0.596259715814,0.342631975217,-0.020999397985,0.009705436067,0.019521412816,-0.016988504108,0.016513021781 &
            ,0.052611410550,0.098007768137,-0.034900933859,-0.133736426640,-0.035029490656,-0.147860228248,-0.153857958447,-0.065312656734,-0.197501254198,-0.227005086567,-0.203992570751,0.077987432597 &
            ,1.242203498686,1.036770934936,-1.716093753677,0.364878286108,0.746253919457,-4.989546134344,2.048921082138,2.591549516345,-0.111489812758,0.690871202568,-0.871711190128,-1.031690868532 &
            ,4.533491689531,0.398937577371,4.445555146429,-3.722642697111,0.183594646444,-1.221548024781,2.901449106372,0.901733799959,-4.456610806201,3.472294947711,-4.103374420288,-5.614341561023 &
            ,-3.281882784769,-0.580466843075,1.675711830628,-1.440091866697,-3.676020246581,1.774363226909,0.537525548410,0.825177119699,3.063814805259,0.390342947032,-0.465248047986,3.696226982980 &
            ,-1.181086028907,6.517264311439,-3.981469871329,-2.176465414984,0.074838398540,-0.288829906668,2.158071040847,-1.372398706580,3.622008649913,4.729439071083,6.181726058492,-10.827255220531 &
            ,5.617105120343,-0.438343831185,-5.336415495581,3.485048462941,-1.354699827912,-1.495168267013,-4.789445106182,-10.231883490329,-1.471561272808,-2.609051719714,-5.421942684954,-2.173476851220 &
            ,2.309167717810,11.119963522180,-7.406996123268,-13.843106061186,1.145693677885,-0.878693610899,-25.845567769262,0.134218541925,-2.488637070039,0.367708859382,-8.556541860814,-0.535442704317 &
            ,3.544726964504,-0.770214759945,-19.123309408639,2.426641615607,-2.766056393811,-1.177592703190,-16.929141716444,-4.118760653728,-3.615610965000,-0.540476861112,1.099126740629,-1.451342751485 &
            ,0.226001519563,7.470276931588,-2.810620216000,2.196947308737,0.757830204296,-1.009397101449,-1.240223688407,-1.627718570908,-0.228494579769,-4.926822937935,-1.217053139348,-1.215833724108 &
            ,0.888045083353,1.229679476058,0.166957950709,2.336515248471,-0.624180874700,1.921147446452,0.750157501055,-2.864810189256,0.342149657919,-1.451119828443,-1.767997711040,-0.093565946354 &
            ,0.158531718427,3.011371956756,-6.289747000052,-3.205567914664,-0.332200099327,-0.266454024883,-3.297162697013,-0.467964185995,-0.603281670472,2.422389281443,-7.541608349487,-0.626062422263 &
            ,-1.779725801435,0.225030238413,-0.781663505983,1.493475967677,-1.709921955488,4.829594401701,-2.530100287850,-0.048870436869,-1.693232583059,-1.457285257169,-2.459477430647,-2.164289866595 &
            ,-5.567716936846,-0.490425167469,-4.713149840586,-0.883261609733,-1.209902014521,1.009737384217,5.690932282285,1.537180974340,0.634401827277,1.062352287059,1.909278161054,-2.151285221999 &
            ,0.800190258650,0.463600653628,1.629743188299,0.164314010589,1.007431154448,-4.600829984492,2.529852528680,-13.005632603531,-1.178921228370,0.209285088080,0.773448089025,-0.354160093692 &
            ,-0.541493849557,-0.994399303108,0.328696904813,-3.270732719425,-0.394210727625,0.499799738137,-0.280715865606,0.065343059138,-0.290729418505,-3.551973883278,0.981791594430,-7.628446948435 &
            ,0.575319610954,0.359394900272,-1.316503444414,0.028092591224,1.505429277006,-0.492720398873,1.248314409628,-9.308927290041,-2.120888845839,-0.401633745885,-1.512878790356,-1.290748416815 &
            ,-0.511195443512,-0.013630717249,-0.003176248094,-18.369357995288,-0.616810333104,0.730810323214,-8.297535642847,-0.088474824831,-0.472824902283,-4.625326331470,3.241511958642,-24.925562056721 &
            ,-2.390589953216,0.132896204089,-1.472471828821,-1.900548454817,0.990370895143,0.613504339022,3.151350161032,-43.716175044445,-4.268549575131,-0.536625193632,-32.890486864216,-1.741576749174 &
            ,-1.313538801059,2.683616924613,-1.018193237295,-24.341074728055,-0.184984220381,-0.045260123026,-83.664032540912,-0.386866785010,-0.381473286232,-1.851468461339,2.260007361408,-10.628670382167 &
            ,-0.888879060421,-0.595988865091,-33.217409145564,-1.150666743586,-1.223041982873,-1.462361319727,2.026091552349,-5.211009524057,-1.608699376816,-0.071637160698,0.348745185186,-0.905011327284 &
            ,0.613279910784,1.107373850620,-0.436922204426,-0.248274589748,0.769476236624,0.012439318716,-1.417604774460,0.579077113003,-0.558426627204,-0.770447329576,0.093063041244,-1.135342162033 &
            ,-0.838461383037,0.044935703467,-2.626323641784,-0.540993332535,0.450006730242,-0.441424727419,-0.075897112309,1.167576356188,2.098866422304,0.174871259065,0.532071343607,1.409200056727 &
            ,-0.010594323496,-0.880494018610,-0.423908449840,-0.115793470320,2.199949835240,-0.066220712314,0.015853757602,1.554971148126,-1.844437810083,-2.508240462145,-0.189633483897,0.022313499452 &
            ,1.280454156227,0.333571253819,-0.290802397194,0.779205366059,0.320117403533,-3.709486801069,-0.237148973593,0.152104132748,-0.347716579531,-0.154463487602,-0.568933558838,-0.294999656760 &
            ,-0.553546184282,-1.973362398717,-0.648219839610,-0.149721165643,1.166639520322,0.033962555899,0.450958593342,0.665831688918,0.781109311472,0.293013993670,-0.352306058630,0.654477463299 &
            ,1.831117861042,0.356605517607,-0.277421862834,1.303851924199,-0.517413524942,-3.622878292886,-0.530694693612,0.740670263177,-2.602744479234,-0.567150369156,0.094361622766,-1.727718484550 &
            ,0.210580105379,2.361197388257,0.382436655676,0.233695151990,-0.602531904177,0.178410684504,0.250464531607,-0.255313417908,0.186211132776,0.265840096003,0.362086357376,0.400111481018 &
            ,0.191458783786,-0.101614862032,-0.435692081406,-0.070930104542,0.259298035297,0.763886423277,0.107029823981,0.371391662693,-0.403280198114,-0.285509245154,0.057699821636,-0.308042727721 &
            ,0.201837828491,1.984303591899,-0.353417434087,1.470719219925,0.036106407012,-0.234176596033,-0.033178626366,-0.041319356393,-0.215561847108,-1.726711206241,-0.566387316456,2.105623136291 &
            ,0.737225503222,-0.094464747063,0.477568534208,0.516389501143,-1.028841886717,-0.313890875668,-0.657542367842,2.045260564848,0.677958142970,0.402183743724,1.775838446294,0.330510010507 &
            ,0.330619061567,0.071065248573,1.273399749467,2.366667731494,0.282808283926,-0.186511002307,6.381905514459,0.519179462108,-0.942888768535,0.854616001452,-0.389784610799,1.718925599144 &
            ,0.608726062918,-0.106108013613,4.482368610011,0.442841627424,-0.610370188278,-1.201519700211,1.890557126987,0.799112644293,0.124074381038,-0.028772638096,0.075750188291,0.008643012650 &
            ,0.045054770429,-0.192009955619,0.379006621241,0.030681278383,0.135833626768,-0.010085173955,0.231744436216,0.070973566141,-0.064069506848,0.132758226434,0.244730018897,0.016885743349 &
            ,0.239691686691,0.006609411632,0.251845405728,0.097035074933,-0.064053559209,-0.244436866694,0.516906081105,0.043788852861,0.086950749506,-0.080063860069,0.208368886196,0.020334715173 &
            ,0.003028932758,0.260032643941,0.089668813453,-0.026394653471,-0.072464259656,-0.115524613544,0.213331400482,-0.064985658824,0.087747783233,0.570409030954,0.214540092140,0.112330099900 &
            ,-0.000836344110,-0.305959555379,0.418798105491,-0.137846735920,0.544109221193,2.511830158329,0.457504302380,0.366132302550,0.141188473184,0.028891492662,0.321898177956,-0.022165301005 &
            ,1.099899182668,2.068764084980,1.223737742023,0.371648629213,-0.224869416010,-0.225343956561,-0.005698637758,-0.368238101331,0.422362986857,2.165813023716,0.649099783782,0.153846454239 &
            ,-0.540185153268,-0.003522182328,0.211208301629,-0.337095661848,0.108598433899,0.184777393027,0.185816664564,-0.140920445963,0.264206210661,0.067790631278,-0.166289849118,0.117538173187 &
            ,-0.002478411533,-0.922777510301,0.051950482389,-0.097714781534,-0.092143103080,0.026325831270,-0.102563270998,-0.093937643050,0.028245182263,-0.104064502809,0.176227666377,0.009227215969 &
            ,0.242715530993,-0.005076118349,0.172584041898,0.234909248546,-0.601663904026,-0.413543481502,-0.297683658664,-0.157463570129,0.371665577363,0.073145487862,0.018607771481,0.242849877589 &
            ,-0.398272302853,-1.136849133633,0.110453479348,-0.047313607202,0.186946556652,0.065865769446,-0.085767570973,0.112589793598,-0.304900557570,-10.665808803175,0.314956535650,-0.000961074304 &
            ,-0.295632992376,-0.066009272290,0.213015240592,-0.171079386009,0.011014552180,-3.464548035654,0.018120786179,-0.168699161944,-0.267090106312,-0.065809389140,0.028337539356,-0.164839412200 &
            ,-0.011172523071,4.912783375146,0.061972449379,-0.223836539522,-0.731986104252,-0.303953439902,-0.172925302265,-0.549074077478,-0.009598587040,-1.582705861485,-0.278305053231,-0.391018053569 &
            ,-0.295674185638,-0.551378542201,-0.261800043322,-0.355540197233,0.291777334545,-5.514808870510,-1.062100920308,-0.766547197875,-0.182437339569,-0.129098431682,-0.091371584870,-0.115416608637 &
            ,0.086359221367,-1.594707327699,-0.399434618257,-0.039575502365,-0.091336167179,-0.078184559278,-0.076845022649,-0.075756938832,0.069442228486,-1.176054487966,-0.182236594004,-0.044618397659 &
            ,0.030800051927,-0.019970225625,-0.026751384443,0.027249394200,-0.006056849499,-0.534148631031,-0.199801805858,-0.088223002101,-0.012724406726,-0.072461654169,-0.061761125700,-0.006000910257 &
            ,-0.042821876806,-0.560007057202,-0.295498834517,-0.035544068034,-0.073521326552,-0.062191990811,-0.042307437735,-0.045977490071,-0.003397642211,0.882881016112,-0.226106426836,-0.021408761505 &
            ,-0.146120425300,-0.078018974171,0.000485318368,-0.110813934783,0.046265701684,1.155696574416,-0.123070037749,-0.050979246514,-0.076078370440,-0.041215090147,0.049463373285,-0.046695094054 &
            ,-0.016457177555,0.588172667498,-0.077615884217,-0.015517709333,-0.191120532311,-0.048345062112,0.163616036358,-0.112093522681,0.030171712833,-6.711250552707,-0.088838417685,-0.152625487659 &
            /),(/8,126/))
real, dimension(2,8) :: hidden1 = &
  Reshape((/8.3642,-6.0498 &
            ,6.2763,-4.6456,1.2283,-1.1284 &
            ,-14.9588,10.8308 &
            ,-2.9184,2.0471 &
            ,1.4259,-1.0529 &
            ,-1.895,1.4178 &
            ,-9.7883,2.7534 &
            /),(/2,8/))
real, dimension(1,2) :: hidden2 = &
  Reshape((/-7.314,-19.0445/),(/1,2/))
real, dimension(8) :: bias1 = &
  (/5.922,4.0914,15.515,4.3026,7.0607,26.0466,0.90992,11.7738/)
real, dimension(2) :: bias2 = &
  (/3.4086,-1.1029/)
real, dimension(1) :: bias3 = &
  (/18.9654/)
contains
subroutine fnet_LosVaqueros_initall()
end subroutine fnet_LosVaqueros_initall
subroutine fnet_LosVaqueros_engine(inarray, outarray, init)
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
end subroutine fnet_LosVaqueros_engine
end module fnet_LosVaqueros
