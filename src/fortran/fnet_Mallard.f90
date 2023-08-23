module fnet_Mallard

! a = 4.4336e-005
! b = 0.092741

intrinsic Reshape
real, dimension(8,126) :: input = &
  Reshape((/-0.053613806018,-0.087848168374,-0.734714232694,-0.745918467533,-0.295847001603,0.162002384372,-0.001819805996,-0.053990044322,-0.097917505561,0.046194241209,0.396554478069,-0.909480804229 &
            ,-0.219703343884,-0.096738177592,-0.041966289432,-0.333647499880,0.128507266260,-0.019490420243,0.377854444030,-0.934120895378,0.406136792816,-0.079725872779,-0.003489424629,-0.252690754728 &
            ,0.014544867083,0.001144216527,-0.750832895101,-0.891246308022,0.043178890034,-0.094718404753,0.005366995109,-0.692640620503,0.071244005197,-0.116444444350,0.178638624539,-0.730838310492 &
            ,0.003208858248,-0.281081577695,0.005791796081,-0.283035504627,-0.001471950309,0.148681417603,0.099725311777,-0.570076236565,0.110673324591,-0.781705969666,0.035006571906,-0.258742164972 &
            ,0.201989400323,-0.082132074812,-0.570633748088,-0.656770907566,0.545253482884,0.299606386958,0.051097761272,-0.199884994515,-0.146461127351,0.185322627740,0.990480221922,-0.676989113385 &
            ,-0.067394204370,-0.599502281575,-0.057758412230,1.079257145241,0.037591824629,-0.022720457595,0.326469298911,-0.524365191475,0.039521662002,-0.787220723057,0.019010872852,-0.102203040904 &
            ,0.043746964556,-0.031193494700,-1.103720625032,-0.120151826585,0.070644144848,-0.362611947081,0.049670279854,-1.905167138082,0.015934563648,-0.000659326133,-0.210586442800,0.134195527328 &
            ,-0.012696609891,-0.619481206270,0.041058954938,-0.397291352455,0.051347508403,0.003710327511,-0.670895559498,0.135421900451,0.036024900612,-0.447297828760,0.062882887979,-0.638084103145 &
            ,0.013311535088,-0.050954952705,-0.311268413779,0.125403518241,-0.000287822483,-0.427440879624,-0.009569316716,-0.702943160413,-0.009781761330,-0.068596061962,-0.385653566405,0.095557553808 &
            ,-0.037439018462,-0.480555663063,-0.040995754336,-0.149958128346,0.008769944783,-0.058363854343,-0.235856819934,0.054750438053,-0.014417043630,-0.413942237307,-0.019256444246,-0.231121409589 &
            ,-0.009697949230,0.108106591232,0.038365763716,0.019316807244,0.139777701872,-0.097905283877,0.022143995236,-0.417771204343,-0.028596924280,-0.013245907999,0.113159290511,0.021831439239 &
            ,-0.038948176825,0.173032140106,-0.004204698843,-0.108583375054,0.022708856156,-0.063713376835,0.015531287926,0.077818694997,0.055759363385,0.052179007788,-0.028525491129,0.222696262793 &
            ,0.042451912205,0.176572593257,-1.375308012470,-0.576475319544,0.315713456377,-0.715710424048,0.212314214451,0.801200117644,0.299366514603,-0.755217011015,-0.886417003842,-0.659769669199 &
            ,-0.262628149818,-0.186676786505,-0.042049926104,0.458343757470,-0.675912128239,1.092845887259,-1.048325134981,-0.684608053271,0.007551311913,-0.047943670306,-0.163116823073,-0.177893726905 &
            ,0.213155738801,-0.365256888009,-0.134400733037,-0.650442981543,0.392069027745,0.659214456556,-0.143362450406,-0.143068039655,0.025159862399,-0.564003481845,-0.155551811129,-0.656855404723 &
            ,-0.239233799063,1.320122365893,-0.202065199299,0.381449071330,-0.778598468532,0.293205143472,0.105143996585,-0.618984020523,-1.817656352829,1.052395756777,-0.282145804155,-0.804070856971 &
            ,-0.467850976770,-0.511599402581,0.308327439922,-0.592838436475,-2.230359543192,0.512512781192,-0.437885005940,-0.695610289515,-0.592186571148,0.618628974384,-1.617875053846,-0.597665302380 &
            ,-0.766620570898,0.276560640483,-0.165409590322,-0.713493549776,0.402246305270,-0.638357244900,-0.119457750257,-0.624669352406,0.370474048582,1.369230455861,-0.335989059963,-0.796933186153 &
            ,0.319266525326,0.196392426345,-1.759104156174,-0.650560790321,1.182178103171,1.265915922659,-0.416497735548,-0.165189005826,0.065259918820,0.181977039785,-0.424273180008,-0.634367409728 &
            ,0.096398164711,0.249998426146,0.088398131186,0.173188604262,0.001332630070,0.134249076191,0.189173875189,-0.630910896788,0.126253425484,0.206083228902,0.032713304061,-0.092939732650 &
            ,-0.254370367044,0.432744215045,0.203500235766,-0.634441489110,-0.013130121843,0.103503325080,-0.113491830115,0.062284260127,-0.424235298699,0.214273059505,0.205071329094,-0.640599945936 &
            ,-0.606107530918,0.012710989546,-0.324277911317,0.155001967153,0.209636616588,-0.268299643591,-0.010687810781,-0.648268128269,-0.335719022522,-0.172942185818,0.153357122194,0.260210666422 &
            ,0.066462893648,0.078876546209,-0.496243776273,-0.655663856049,-0.196722111807,-0.234568212446,0.168407676084,0.120960484698,0.351570603620,-0.091817738673,-0.422683406101,-0.659259065834 &
            ,0.229659115053,-0.014648928979,0.325742086144,-0.029182745090,0.155051631828,-0.158780901873,0.154227226542,-0.647174766211,-0.169354849735,-0.124610198093,0.100865155608,0.074746233281 &
            ,0.067508741801,-0.556246001367,0.835997454501,-0.165610333406,-0.031124023295,0.786268203186,-0.181546758382,-0.553088989022,-0.255752057507,-0.140301286218,0.215161552796,-0.146833506736 &
            ,-0.311217595873,0.720861209570,-0.092194107161,-0.308116785523,0.483811260085,-0.616635634424,1.309004467215,-0.340134249497,1.485871690322,0.132437690873,-0.183895499921,-1.957925840630 &
            ,0.100798330348,1.144007961944,1.774049362016,-0.225406738356,0.810401973626,-0.042130082364,0.143433788671,-1.113118960246,0.758740147194,-0.942021432014,2.551818653854,-0.201816279952 &
            ,1.287495154431,0.980635108089,0.013436253580,0.642718447892,0.554737125405,0.739879572884,-1.340162749846,-0.176944146005,2.761652629039,1.060368726020,0.230985509334,0.090005194607 &
            ,0.663060187590,-0.738473991372,1.072459260384,-0.246215799724,0.817814538854,0.654160118732,0.165397592088,-0.327120959452,-0.197268584393,0.643356103814,1.176382539563,-0.284976319635 &
            ,-1.494929622536,0.642288103818,0.438823696906,0.578336449704,-0.166781740831,-0.824149456670,0.370624585867,-0.069079333232,-0.797280880498,-0.224635810848,0.146131856481,1.904930027325 &
            ,-0.748551636125,0.949463923070,0.227108239271,-0.244421749725,-1.238819010131,-0.121533133284,0.141761951270,2.179958924034,-0.131531645256,-0.073911943745,0.024217922649,-0.067543040651 &
            ,-0.242593125405,0.578717729715,-0.035334837142,1.142158497203,-0.002071172792,-0.138827219152,0.295637789547,-0.187810223745,-0.096430613550,-0.112960429392,-0.015452924234,0.333681052061 &
            ,-0.098106185968,0.063767293783,0.255062978709,-0.325300782856,-0.251747121835,-0.366080066868,0.059273275194,-0.022164769568,-0.059568498333,0.047770794684,-0.079985527059,-0.449383615694 &
            ,-0.074602621715,-0.202446179504,-0.037388548569,-0.362494795433,0.075905288691,-0.169374875038,-0.068327463664,-0.531592914164,-0.161250761878,0.186009185721,0.083237809256,-0.206482500939 &
            ,-0.101366541099,0.293845011060,0.431614386392,-0.560063501083,0.137754512646,0.190658372309,-0.002036871131,0.722352694904,-0.159823356376,0.169400348969,0.886586398725,-0.528380502399 &
            ,-0.146984687028,0.554581834478,-0.084531072724,1.089707292675,0.031007359456,0.099520816543,0.373868939501,-0.430762059295,-0.810672058661,0.757929489823,0.272241045836,1.461704080405 &
            ,-0.021022946664,-0.461041314440,-0.077592812638,0.008993066682,-1.304649926851,0.532876692817,-0.014616974296,0.325196165997,-0.304908283086,0.227534567565,-0.206276815440,0.021543215380 &
            ,-0.238173543626,0.589653458522,-0.039720282053,0.367421263578,-0.931425414565,0.742024334828,-0.155313232617,0.029177665289,-0.237807446521,0.500951614408,-0.692741358050,0.251590590526 &
            ,-0.147605648623,0.626285122299,-0.098345208396,0.028715936302,-0.187530606642,0.366794165118,0.521128179820,0.270708140311,0.023090388937,-0.483196022779,0.001074574190,0.034590604231 &
            ,-0.158387251232,0.297973479831,-0.606259552107,0.335148273801,0.451578155472,-0.757617112157,0.024782798234,0.034923726099,-0.477747459273,0.272437870179,0.871998196251,0.424475853221 &
            ,0.889617850611,-0.111664378520,-0.119150956510,0.045146586150,-0.133290387984,0.157678185683,2.208669104234,0.564534069550,-0.280342777766,0.165909690037,-0.158378808735,0.064877074337 &
            ,-0.079620169038,0.047561931785,1.500393120736,0.670431715649,-0.361628618718,0.613484793837,-0.087567202409,0.071820765330,0.117146701241,0.081141383092,2.599262715813,0.573438288068 &
            ,-1.769967944597,-1.221690001938,-0.050716372808,0.082972133971,-0.223139147220,0.009973428135,0.272024219919,0.328959476766,-1.268742522727,-0.885702095789,-0.041622400633,0.087170248829 &
            ,-0.771113319728,0.061662437028,-0.450860475910,0.182589971464,-0.522287316198,-0.448471483686,-0.043349657950,0.087983961463,-0.880145529112,0.077206751687,-0.483073751773,0.162706581597 &
            ,-0.275273325970,-0.376958591579,-0.045584234956,0.089169303570,-0.906960119397,0.084178039703,-0.424200950106,0.134541876937,-0.458602129829,-0.456784564985,-0.066725013521,0.090522101742 &
            ,-0.879947161902,0.078192422376,-0.378700450465,0.121158818639,-0.499688416400,-0.407033800740,-0.089510066002,0.091740165579,-0.900579965530,0.066957619469,-0.523620130250,0.118468454025 &
            ,-0.570350256646,-0.411105418064,-0.115684196914,0.092406137246,-0.897324731153,0.052207685730,-0.114421192422,0.113160246187,-1.149813566869,-0.729646012401,-0.195985766962,0.091928720531 &
            ,-0.702688546946,0.009983712216,1.043638934687,0.113618191096,0.438465684637,0.310202371269,-0.397974632564,0.090614667057,-0.819323400203,-0.063493324718,-0.691791376271,0.191423908049 &
            ,-0.154549585944,0.395775941303,-0.803829710805,0.053762158845,0.210379823104,0.861686974047,-0.063131720410,-0.476216174918,0.499229621868,-0.619459722402,-0.817806933744,0.048926051253 &
            ,0.358154182160,0.535502073209,0.505401390985,0.306793832944,-0.066040817750,0.017384539346,-0.573345209943,0.080739538825,-1.664206311972,0.540170089582,0.699308207372,0.519725598473 &
            ,-0.565026921793,-0.266492501007,-0.352047246220,0.106995147264,-1.071950635353,0.366200931175,-0.463238454343,0.163492252387,-0.044560683236,-0.265926337696,-0.394166654709,0.118141455018 &
            ,-1.543206714042,0.014777862265,0.924586786113,0.540390274791,-0.649306083190,0.679139896785,-0.501010060020,0.126141966946,-2.283677826429,0.005738135589,3.019408923711,0.304318325760 &
            ,-0.333879100871,-0.022104334135,-0.339607815572,0.148423678162,-2.597666315257,-0.364519222727,3.073446518173,-0.158224076092,0.714528030481,-0.255279825729,0.150767364714,0.177179582418 &
            ,-1.722282844901,-0.321455381199,9.899767665384,-0.518378457716,-1.216291783758,-0.772740665840,0.208021643103,0.219417447307,-0.327794844659,-0.282702500057,9.640303572426,-0.256660504315 &
            ,-4.900591334713,-3.171474454904,0.639261176521,0.276482659145,0.867405466910,-0.121211142949,10.487556974877,-0.132332794416,-2.903839885010,-1.947200717662,0.705043573344,0.296809049724 &
            ,0.412892063331,-0.098480234488,-1.983225628171,-0.341080699884,-1.817215067976,-2.042622020676,0.705682010419,0.300214067581,0.347907709097,-0.083804962859,-2.064311715349,-0.337979789324 &
            ,-1.290524793976,-2.372807818395,0.704460408950,0.305707778554,0.398535519017,-0.070155137253,-1.598883174830,-0.335215469070,-0.882234779515,-2.104447948531,0.664970230146,0.312311554561 &
            ,0.417532096720,-0.095464770652,-1.410512937961,-0.308279094847,-1.504627319600,-2.432443382983,0.639560667874,0.320467002345,0.476658783208,-0.191808397866,0.066751930263,-0.254269228782 &
            ,-1.105566272783,-1.887038733871,0.540956942093,0.325901540752,0.276703386963,-0.357617741271,0.047967387942,-0.149153629143,0.091727765494,-2.431324717283,0.226866585544,0.329732473512 &
            ,0.032195626026,-0.505459734941,0.620269640661,-0.068965752846,2.174795571803,-2.923129819249,-0.471116640741,0.326577396411,-0.179229911130,-0.527882158450,0.637105131681,0.094890382810 &
            ,-0.039112348247,0.162998005333,0.304170210568,-0.112929500859,0.130387930297,-1.093425429054,0.006265674159,-0.293476811762,0.211414153565,-0.152985225319,-0.379949103992,-0.234662568163 &
            ,0.330341015217,0.490987053154,0.071541001870,0.701218152040,0.018131737389,0.215821387607,0.162490902281,-0.186201078549,0.803247026818,0.559084234713,-0.100683380424,1.170179971753 &
            ,0.256048856002,-0.305465516456,-0.236865147161,-0.174341294318,-0.172667647048,-0.364341761396,0.217293441311,-1.046104351515,-0.346450075316,0.278925624131,0.473718740186,-0.200550383591 &
            ,-0.546254208123,-0.230996844336,-0.132770831650,0.390782395554,-0.447589375993,-0.260555657754,0.463500479922,-0.156654780854,-2.358423818152,-0.282888911485,-0.068695373458,-0.331055492007 &
            ,0.083447959599,0.211408153708,0.058684078434,-0.085335720873,0.896660973647,-0.524020260638,-0.304168230746,-0.665533605322,0.076960513146,-0.065455389877,1.049935550913,-0.035096528620 &
            ,-0.245198612844,-1.002744076355,-0.325020720263,-0.916240667890,0.219072506336,-0.070334195307,-1.001193075869,0.032563187837,0.124538832835,-0.658317655348,-0.408725960172,-0.033529375329 &
            ,0.406346104806,0.831098033649,0.275798493291,0.067278443468,1.422637578701,-0.655765943230,-0.267557553687,2.233629187223,-0.004868256897,-0.131069986268,0.136178991622,0.053840597088 &
            ,-0.754865433444,-0.046166696001,-0.010458086757,0.259903015345,0.200075150467,-0.044003733394,-0.116411306446,0.054504463799,-0.070579056222,0.080001691291,0.181437821806,-0.286668871408 &
            ,0.012090542228,0.300384657503,-0.094773831019,0.059945495779,0.036117538412,0.133176910554,0.070982948875,-0.336660453922,0.150662373899,-0.087090458703,-0.093629596351,0.067518310440 &
            ,-0.125990980806,0.152951497132,0.076798219268,-0.614894385685,0.092480048379,0.045668942989,0.136318377999,0.076531156212,-0.066140250839,0.079682025544,0.038484912267,-0.792494610307 &
            ,-0.022234570869,0.271392354143,-0.056101739936,0.088519081252,0.172100134024,0.075980567189,-0.074278896370,-0.219449823481,-0.093028036971,0.200759834863,-0.122999810635,0.109945463492 &
            ,-0.196940220857,0.556244610662,-0.068792549107,-0.142224669752,-0.161056866989,0.029036013386,-0.757905195277,0.148381563667,-0.483077442224,0.013645562529,-0.043595426920,0.166569570476 &
            ,-0.091144479185,-0.158583273349,0.557959198579,-0.728080894182,-0.502330372451,0.399541137815,-0.073746492124,0.460498094922,-0.296107204522,0.116947911179,-0.366128568005,-0.840853297812 &
            ,-0.326297896458,0.715728349551,-0.220808222407,-1.031686635085,0.086656458557,-0.108312531546,0.289771782494,-0.847019115834,0.005926208383,0.184988665682,0.036666877060,0.544894937201 &
            ,-0.107042186922,0.025680802377,-0.954739324741,-0.744186180151,-0.357583515290,-0.013933849198,0.002651628896,-1.850833900691,0.024922101340,0.014833861777,-0.421111950167,-0.730876615486 &
            ,0.186075557294,0.550141073288,0.041666125066,1.264496924048,0.006855423189,-0.115931356489,0.248060716681,-0.520780367818,-0.003279541090,0.730575193503,-0.053844418885,0.572877132110 &
            ,-0.005974907942,0.098977320337,0.072324103237,-0.244215992217,0.013889562278,-1.155815762019,-0.037507816010,-1.689122037636,0.100839643486,-0.037734992938,-0.263736162268,-0.105771550270 &
            ,0.259177327243,-0.021963287044,-0.006880400215,-1.127049867170,0.188699208203,0.040489361033,0.101236348230,0.105801448157,0.666603524455,-0.637606763779,-0.021889453174,-0.988018444707 &
            ,0.064810646048,-0.029666515924,0.192943411243,-0.109465162021,0.052502186654,0.011337410703,0.026637525970,0.019534469988,0.045821707573,0.005347905193,-0.163539654966,-0.258857602922 &
            ,0.209777748840,0.199273615961,0.002297098891,-0.250084895397,0.008859400860,-0.005301768343,-0.066702214371,-0.290829546084,0.074620732816,-0.059352886169,-0.025907746074,-0.034593301401 &
            ,0.015077933652,-0.025327258097,-0.604459468550,-0.349148938246,0.026357450293,0.121993224473,0.010644553281,0.468667874321,-0.024984157072,0.022506941239,-0.432193618055,-0.409748727968 &
            ,-0.019602785389,0.206462307728,-0.004319023792,0.318249048766,0.036080838885,-0.066858971485,-0.440146308867,-0.532301860412,-0.007961304342,0.290391914796,0.015033709116,-0.071029558896 &
            ,0.025809718191,-0.006089982752,-0.536601442418,-0.687266679179,0.066364828558,0.235711928621,0.011454478955,-0.109229888920,0.017247224362,0.036146623261,-0.694637912132,-0.776173151847 &
            ,0.095073427461,0.008364982391,0.011915042338,-0.167505470054,-0.005913995532,0.044282766613,-0.438884634286,-0.816503093508,0.055591406343,-0.093560926259,0.003714190397,0.036780484497 &
            /),(/8,126/))
real, dimension(2,8) :: hidden1 = &
  Reshape((/0.44786,7.2391 &
            ,-2.4728,6.4118,1.4064,-0.73839 &
            ,0.61438,-1.089 &
            ,-1.5295,-2.025 &
            ,0.71907,1.2732 &
            ,1.3185,-11.5407 &
            ,-3.8279,0.27143 &
            /),(/2,8/))
real, dimension(1,2) :: hidden2 = &
  Reshape((/-0.010562,1.6047/),(/1,2/))
real, dimension(8) :: bias1 = &
  (/2.138,4.4478,0.076158,-0.47862,4.2869,0.62545,-1.1175,0.68211/)
real, dimension(2) :: bias2 = &
  (/0.309,0.64604/)
real, dimension(1) :: bias3 = &
  (/0.11009/)
contains
subroutine fnet_Mallard_initall()
end subroutine fnet_Mallard_initall
subroutine fnet_Mallard_engine(inarray, outarray, init)
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
end subroutine fnet_Mallard_engine
end module fnet_Mallard
