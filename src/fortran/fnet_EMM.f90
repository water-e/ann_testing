module fnet_EMM

! a = 8.9808e-005
! b = 0.18092

intrinsic Reshape
real, dimension(8,126) :: input = &
  Reshape((/-0.010433913003,0.035760844885,0.079126752621,0.227970742376,0.063802618283,-0.018635325424,-1.009426508259,0.019839811917,-0.031062381371,-0.057596257530,-0.143335228288,-0.623049737494 &
            ,-0.143356796512,0.109319633492,-0.026037769381,-0.130366997962,0.089480219714,0.049709842605,0.056082742369,0.188990139345,-0.004564921792,-0.008788872660,3.864539212533,0.211518099969 &
            ,0.031291281979,-0.094915046503,0.026053041824,0.152362785710,0.113011171602,-0.006409800369,-1.137345221681,0.022917219165,-0.025391892171,-0.013238203538,-0.007421149441,0.428846808517 &
            ,0.078633269821,-0.249118933699,-1.299201157774,-0.049477956348,-0.028401546101,0.031324274521,-0.021490257107,0.128370697475,0.075514927961,0.601926353401,-0.690308450211,-0.020645766042 &
            ,0.077956714305,-0.001883204026,0.273939490099,-0.046135738337,-0.049166699749,-0.591239160956,-2.641788396606,0.115030742984,-0.173138377587,-0.061563777498,-0.505495892418,-1.372541234579 &
            ,-0.323472023238,0.490486944309,3.708909556936,-0.423444673401,-0.058222084320,0.279871702078,0.031524553269,1.848491902848,0.460251479564,-1.253290961357,-7.934985744170,0.284684121026 &
            ,-0.181730749632,0.198004896907,0.213967591013,0.432354593556,0.114063576923,1.101418284681,-13.340838689577,-0.298445579121,0.134678017600,-0.040681583434,0.066102358451,-0.108847162662 &
            ,-0.031958973892,-0.086235605014,-1.133642605222,0.234848043687,0.036827667873,0.011815910505,-0.052378312357,0.093829594671,0.005928117032,-0.096654362194,-0.653557779345,0.102664471907 &
            ,-0.025609838413,-0.009351808252,-0.002247740530,-0.048540743227,-0.016492420388,-0.119180494915,-0.750131105246,-0.067547138521,-0.048717764923,0.051194000137,0.027009416805,-0.150111558086 &
            ,-0.032838397289,0.256380517660,-1.105513682495,-0.078798774795,-0.058851071505,0.049988214416,-0.191652262784,0.157918220884,0.044643479603,-0.221262028698,-0.446948425411,-0.021591024329 &
            ,0.087280443853,-0.045999398161,0.024526807421,-0.163004727884,-0.008864888975,0.132138271575,-1.833102197725,0.162170540951,0.196848115846,-0.263807744484,0.112355793087,0.018705026412 &
            ,0.034174723520,-0.059520541570,-1.848043582703,0.160559949105,-0.066617382079,-0.034381495355,-0.160534085468,-0.440965604510,-0.133608467076,0.114811153296,-0.417996101770,-0.187653736085 &
            ,0.525898304339,0.009525773485,0.764775595767,1.106538672669,0.205349992481,0.569073224924,-7.189432279316,0.993018990717,-0.744234497601,0.433100908853,-0.458214233578,1.449331845713 &
            ,0.458919352193,-1.729885547409,5.354249788114,-0.934798042940,1.740855106218,-2.088341700224,1.647161708122,-5.871805053283,-1.035224887990,-0.706388080297,-8.825407051329,1.443609824917 &
            ,-0.617283844260,0.778785746769,-3.378176155631,2.882747013628,0.410608746671,-1.021434081115,2.317354163307,0.246580408122,-1.042638460299,0.451217441069,-1.300501063524,-3.402630558622 &
            ,-0.998984168849,-0.410967873790,-1.144313812329,-1.875053315095,-1.517093251712,1.828633854849,-1.392224521133,-0.643463153348,-0.774191384509,-2.201441957188,6.119048860510,-1.507058467163 &
            ,0.997561976953,-0.565996482327,1.709170288683,-0.798769621394,0.172779832504,2.527721539837,-7.111765635989,1.470811147550,-0.597763611100,-1.301129771127,-2.017415400478,-0.598431687964 &
            ,-0.147458519806,0.032169891853,1.641594048798,-2.405297038221,-0.999083280517,2.768321839775,1.037857069964,5.752682554747,0.767944825744,0.613858568789,3.673171181717,0.326654520499 &
            ,2.586956839768,-4.700783780440,1.633768531619,-3.256131870988,-0.043031646340,-0.836936950142,23.984723481567,1.167551865431,-0.679461524885,1.102302655340,-0.837813249417,1.914351724155 &
            ,0.117022960530,0.478021765516,3.177925256724,-0.380389938027,-0.051653118372,-0.361831516050,-0.363022440728,-3.003564970710,-0.691756829741,-1.173052342255,-4.898194679036,-0.436556895768 &
            ,-0.040956394917,0.389713563577,-0.353682660051,-1.896990426479,-0.730173996992,-0.008113757097,-3.431519471701,0.173963059164,-0.001153935987,0.029371711556,-0.539599751763,1.427970550724 &
            ,0.156484299082,-1.713692368650,-1.373382289985,0.161316730953,-0.016956409915,-0.142657314975,-0.045393024831,-0.143186500220,0.034367800348,0.500539878690,-1.438147536579,-0.093425853002 &
            ,0.089272388279,0.046903249052,-0.150891834663,-0.396183609003,-0.082939029118,-0.018037828550,-2.034702460341,0.334241743806,0.405013097301,0.178945972986,0.228418372284,1.237656628568 &
            ,0.430661505212,1.879204146835,-1.677619973191,1.183883808214,-0.302395508078,-0.030527257079,-0.405690609587,-0.684986738493,-0.224809320786,-0.580375792987,-11.067910253615,-0.623890688470 &
            ,0.246475628396,-0.601807200913,-0.252971134453,1.811641475528,1.051334141235,-0.280524664332,2.706193178976,0.400920226436,-0.230830620786,-0.580760169863,0.274193817073,-2.530501411010 &
            ,-0.493358629688,1.989624646078,1.152238746314,-1.373384902338,0.068990859481,0.125049385084,0.287848630028,3.094600120286,0.982215054269,-0.815801614334,-3.969540481740,0.432032666119 &
            ,-0.212519123065,0.251929357309,-0.126551009738,-0.115804711450,-0.614104262077,-2.088338737616,3.811954476310,-0.309880500486,-0.007213638615,0.143637853585,0.085930021380,-0.401362413958 &
            ,0.059740412587,1.874075636062,-1.076178233630,0.042237879582,-0.023641829285,0.651337008271,0.017387786049,-0.295674000570,-0.595871576341,-0.981350992340,-0.345955654360,0.406696572047 &
            ,1.154376235095,-0.510429990965,0.337326665056,1.780748807703,1.018141063418,-1.789493213231,3.953511685736,2.553587026184,0.930705880490,-1.912090252278,0.473865180906,-4.471683947087 &
            ,-0.569259343863,0.355028319177,1.810227519443,0.354592803071,-0.092435865561,1.300686731936,-0.129430525490,2.373062570958,0.672428978716,0.205268698681,-0.986994398465,1.044672784324 &
            ,-0.328035010170,1.571595308700,-0.612001959514,1.994282783389,-0.279110492292,-3.659240101899,10.550394556723,0.730846434391,0.108668083659,0.179257913969,0.072745471107,0.480334594349 &
            ,-0.003938736665,-1.235895277378,2.958162945848,0.367556950951,-0.207933398557,0.314473124213,-0.146965825146,-0.290313309430,-0.166329164687,1.092121597459,0.628361567587,-0.237839070450 &
            ,-0.152326852053,0.307451664346,0.073315895440,0.980884636406,0.185551555869,0.127420576241,-0.456633844595,-0.070921924632,0.180245603979,-0.205084994805,0.242091201968,-1.271894248454 &
            ,-0.265042117074,0.482469222728,-0.856360645527,0.100561210229,-0.231672124955,0.421603948547,-0.410082252855,0.411467387099,-0.109125260185,-0.754692088535,2.712752124722,-0.109006744942 &
            ,-0.214127963457,0.441004266811,-0.136880826489,2.423347587847,0.442583966715,-1.178004789505,0.796365280803,0.046628352382,0.332949378838,-0.347707243762,0.564454243292,-4.469994376565 &
            ,-1.058727916661,1.254882832158,-0.269926541863,0.081652303580,-0.201043638041,-0.004365135335,-0.225742031182,1.008712636035,0.157814776467,0.305364898716,-0.348400825201,-0.387615436728 &
            ,2.767843466202,-3.513715878654,0.280611248265,4.028637333184,1.919292888193,-7.809285620648,4.152363034786,3.565568538200,-1.426830074683,3.790428435362,2.908190959153,-0.866932944745 &
            ,-0.585105976779,5.391212274196,-2.866869468178,-0.342154706694,1.022940175384,-0.772513265148,-1.823031831196,0.143437541662,-1.172994981386,0.577438625776,-3.225368115041,0.899078235863 &
            ,1.242132874734,0.743761720297,2.564771312931,-0.962794751789,-0.301479667958,0.643049159297,-4.878472322857,2.670347578469,-6.694472424037,5.887437336414,-4.525836257001,5.593253254313 &
            ,1.246240100340,2.313081695686,-5.680385556799,-7.526415208250,3.352670804706,-8.196247176044,-0.384564053185,0.139660856122,3.083459629965,-7.569433268130,-4.014079696750,1.651774027734 &
            ,-0.110271112939,2.383192687316,1.420419819559,-3.796426439700,-3.320790613590,-11.785848156818,-1.107670082973,-0.593915664603,-0.907165171916,0.864056353622,2.292521032829,-8.155328168073 &
            ,-1.488556573614,-7.749675397112,-1.635805556196,-2.046960813069,1.346801796154,-5.586655880038,1.025613156358,-3.780305370363,0.209641731275,-7.829046853867,-1.208410487104,-3.252446802996 &
            ,-1.677519057407,4.635828154251,-0.785455209990,4.982729161136,3.409054293971,-5.537660143421,-1.883822780286,1.831163188604,-0.927395482127,0.900597900028,4.289719011225,1.113449403249 &
            ,1.008381883086,-2.761037264597,-0.441708661900,-3.087551026117,-0.406410179231,1.430047130825,-1.844673773493,1.667073542237,-0.078500626352,-1.783426787663,-0.217673283487,1.965111546086 &
            ,1.209294004988,1.276750418770,-3.157015693600,2.029715470355,-0.261555543583,-0.443686747962,-0.203610369949,2.562425454998,-0.123255637115,0.066693638828,-1.356760113321,1.075067199190 &
            ,0.332974344906,1.514356514045,-0.362832071343,1.034272990256,-0.757215041553,0.815732297010,1.204447665317,0.151344439873,2.233071444039,2.836183617014,-0.470941157342,-1.003001009566 &
            ,0.126803264280,-0.916122977267,3.033229878306,1.249288969114,-2.112088935762,4.767037527130,-0.548825497006,-1.594991542069,0.704254044639,-2.741310675624,0.335734954548,0.630090993747 &
            ,0.158961077255,3.658859999891,0.041295383508,-2.859077443020,-1.722592189078,3.172502253224,-5.715768763233,-0.216451176958,0.827421802540,-4.769625408253,1.752133070479,1.830885457089 &
            ,1.401380233040,0.219985252362,2.585325069417,-0.832987433121,-0.675007992520,3.322835857567,-0.375999870712,2.218805506545,-0.820923016540,-2.661513529122,-4.372081681575,-1.998867713053 &
            ,-0.019525208009,-4.844316226488,-1.802032816072,-3.197583731680,0.140720835871,0.210027549276,2.479455139073,0.317103368036,0.522184188131,-7.340351225476,-3.743966665273,0.346375898296 &
            ,0.477980415500,0.280243654125,-1.244912747768,2.637539987116,0.844850742224,-13.972662646079,-2.215803496713,1.977805542234,3.481431966986,-1.655720628143,5.134461230813,1.963020954309 &
            ,0.130990154376,-16.318827680173,-0.532141121230,4.652592395178,4.118763890792,-0.287117365688,3.620372784840,-5.268429238685,-2.069726642806,-13.882720986275,1.025249825376,7.483149235422 &
            ,1.760889391241,2.749237963124,3.475335909795,6.334152282633,-0.816978072995,-11.570771226338,1.701380351280,2.928908049519,-6.547856112646,5.088988003625,6.468099345668,-0.761831172325 &
            ,3.026013065730,-16.325657968031,2.214769038830,-10.483908155213,1.243302948139,9.351938211868,-5.560972933689,-8.623001345915,-2.832720473061,-10.628431761921,-1.475962217052,10.379325366339 &
            ,0.038113499147,-0.533301077809,-7.858422517420,-1.203420109242,8.127551750592,4.502306502836,0.316363149383,0.560900044598,-0.459518211325,-0.077953849915,4.253776267538,2.539674957349 &
            ,0.840925377755,2.369616327565,-1.239440845257,-3.900929815532,-0.723820562765,-1.076161027736,4.049064404553,-0.165998647771,3.457584717301,0.454067086463,-1.697411717985,-2.209788568572 &
            ,0.019611586910,-2.111191290345,4.171370721196,1.090513643735,1.027751905129,-0.955115795209,-1.729051039120,-3.654948349828,-0.237728883774,-1.208961663053,2.422273666682,1.721142084802 &
            ,2.464635436181,-0.318735882869,-1.760277574522,-1.937590818899,0.256817011248,0.973727451161,2.924261959701,2.413984003027,2.079358567489,-0.037187171017,-1.498037567576,-0.216689020502 &
            ,2.180599421277,-3.082803804852,2.471674823462,2.776484466876,4.448884542106,3.490373871760,-0.618264404650,2.638525544122,0.653421896194,-3.549736550275,7.871259364098,1.397594813559 &
            ,5.818834670246,1.702773003014,1.660941532226,-3.276897164918,4.684305546857,-7.576165183193,1.576025928404,-8.062378348326,-0.995407154581,-9.891358964618,4.862048616916,3.983516076001 &
            ,0.086893622518,-0.314173633815,0.157776224923,-0.509105625748,-0.037510731188,-0.002492743097,-0.711923989263,-0.136689407522,0.042913669013,0.225763669520,-0.157917997821,-0.179131479947 &
            ,-0.113699260901,-0.484037537011,6.289399119785,0.323070016891,-0.240761429922,0.632914884598,-0.071457952660,-1.073251202864,-0.476190386825,0.559955543782,-2.014832742099,-0.062796458197 &
            ,0.588722429249,-0.796406847279,0.419085605355,0.321908893719,0.209041249406,0.885448047466,-0.735986472522,0.503505871011,-0.597387575032,0.868831422696,-0.406214916921,0.924115651273 &
            ,0.118247076372,0.270025802283,4.803880442310,-0.379772844698,0.265203984945,-0.641071559244,-0.093240616882,-2.041396545079,-0.399480274200,0.381181809837,5.443298050608,-0.038370885633 &
            ,-1.190775414810,1.086021428523,-0.531977123205,3.180032951241,0.759345890808,1.330956061884,-12.836512217848,-1.276794279480,1.120402740160,-0.717164650870,0.165113516128,-1.465290656304 &
            ,-0.457925779672,0.950521102984,8.622155456863,1.701329091687,-0.383119653934,-0.194571061405,0.299933454464,1.108078013747,0.309732267614,0.134540998490,-1.720251085813,-0.912975646535 &
            ,0.098636870246,-0.341485056702,0.210677791102,-0.323892519366,-0.457174537910,-0.731507359554,0.781834467722,-0.132640240873,0.048124506444,0.721365964733,-0.144481191856,-0.838262261673 &
            ,-0.619485440756,0.687158972049,5.830444073944,0.602431231939,0.076334955016,0.021104229031,-0.142747164162,-0.358578998854,-0.146456267433,0.607632488859,-1.706262965823,0.221327619026 &
            ,0.004379802109,-0.084980373366,-0.190153944684,-0.052135148292,-0.015872405867,0.496870224435,-2.071256640710,-0.000943508760,0.449053555531,-0.608646996969,0.570559907913,1.136185301098 &
            ,0.498377095868,-0.525708732008,1.745674137532,0.505819480176,-0.075351945516,-0.060031409915,-0.361967960663,0.208900781633,0.013148176130,-0.589030030870,1.403869704418,-0.085809266533 &
            ,0.030305194885,-0.015595558610,-0.064569193367,-0.764456320769,-0.252608500727,-1.170782716856,-2.724753552900,0.079743987454,-0.136761060975,0.161467026024,0.000858439067,1.405731116006 &
            ,0.197427857395,-0.558877717028,-3.112664004283,-0.116748714496,-0.103179597461,0.135685818034,0.009744406765,-0.040041038120,-0.005821835752,0.390340314413,-3.048595215437,-0.096190887882 &
            ,0.095892741100,-0.108414253000,0.191195008331,0.202230867922,0.105136646149,-0.183500252467,2.220851354135,0.108714201186,0.177668601981,-0.211370834537,0.209482022225,-0.656930232110 &
            ,-0.168026523009,0.414189208595,5.038853984008,0.058305936269,-0.173877125940,0.165418971537,-0.441335075548,0.605139630965,0.064503486257,-0.599933607181,-0.613319115356,-0.119897423367 &
            ,0.214911184807,-0.292515098726,0.212238353364,-0.491838873523,-0.072278871908,-0.051925311174,1.472282121234,0.131059759938,-0.212400291931,0.147866361963,-0.053373469522,0.259095536210 &
            ,0.032342161002,0.432560911745,-1.999749042105,-0.336491530109,0.090627642920,-0.072337873427,-0.071141212036,0.283508237080,0.083368655466,-0.068166948065,1.285754399512,0.158196657202 &
            ,0.360337467820,-0.378388190799,0.420899405040,-1.031754671919,-0.244241455662,0.449174697563,6.521443827641,0.250545427313,-0.102969354477,0.080976161484,-0.093425965389,-0.492768782242 &
            ,-0.142801032883,-0.020955214196,-1.637971189822,-0.155692688956,0.525813254936,-0.673141327051,0.617626939006,0.340061230393,0.277081548108,0.470359929074,1.197904058363,0.457472008112 &
            ,-0.627309843995,0.694617888530,-0.394324238155,0.867758692010,0.073060525043,0.098418295426,-2.199397391832,-0.675288392950,-0.005872991416,-0.008998558394,-0.099010002055,-0.161858572666 &
            ,-0.051769185435,-0.075468724634,0.278636385924,-0.007734250599,-0.007514777315,-0.017406411916,-0.070446681927,0.020718895029,-0.000371989805,0.109569106610,0.344384688596,-0.022482200344 &
            ,-0.006412883531,0.014425589900,0.033087865880,0.249073762337,0.058870455418,0.055802018002,-0.059528233178,0.000445798152,-0.005919928301,0.074648757629,-0.001785694805,-0.011047209750 &
            ,-0.027345277481,0.054177504754,0.403907809254,0.048359562490,0.033221908093,-0.002245437486,0.069006972793,0.018320888740,0.006513076192,0.078721352639,-0.371022219933,0.062768040868 &
            ,0.028473243944,-0.050466289581,0.080302441629,-0.190465343659,-0.045600662694,-0.119199671569,-0.947438970594,0.000342995593,-0.003177043204,-0.067775326175,0.066418094141,-0.302230733481 &
            ,-0.076846072894,-0.086200268025,-1.484380775569,-0.089899174837,-0.071810517208,0.015223859992,0.022305093927,0.037414374991,-0.006354444917,0.175479052464,-2.637723136365,-0.157572074519 &
            /),(/8,126/))
real, dimension(2,8) :: hidden1 = &
  Reshape((/-5.6371,27.4389 &
            ,5.2918,14.3546,6.2824,-4.965 &
            ,-4.3291,-3.6579 &
            ,15.7331,16.0405 &
            ,-11.734,-9.4368 &
            ,0.12317,-0.056965 &
            ,17.1169,-11.4754 &
            /),(/2,8/))
real, dimension(1,2) :: hidden2 = &
  Reshape((/33.1498,-23.9041/),(/1,2/))
real, dimension(8) :: bias1 = &
  (/-2.3414,0.94744,-3.3276,1.4494,-2.7985,14.6875,-6.4856,-3.318/)
real, dimension(2) :: bias2 = &
  (/-2.7516,-13.2494/)
real, dimension(1) :: bias3 = &
  (/-9.0476/)
contains
subroutine fnet_EMM_initall()
end subroutine fnet_EMM_initall
subroutine fnet_EMM_engine(inarray, outarray, init)
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
end subroutine fnet_EMM_engine
end module fnet_EMM
