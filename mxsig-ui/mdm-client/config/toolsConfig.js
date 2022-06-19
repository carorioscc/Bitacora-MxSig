define(function(){
	return{
        timeLine:{
            base:{
                //url:'http://10.1.30.102:8181/fcgi-bin/mapserv.exe?map=/opt/map/mercator.map',
                //layer:'c104'
            },			
			layers:'http://gaia.inegi.org.mx/NLB/tunnel/wms/mdm6wms?map=/opt/map/mdm60/mdm61vector-beta-102.map'//'http://10.152.11.6/fcgi-bin/ms62/mapserv.exe?map=/opt/map/mdm60/mdm6vectorsolar.map'
		},
		sakbe:{
			mainUrl:'http://gaia.inegi.org.mx/sakbe_v3/',
			key:'xxxx-xxxx-xxxx-xxxx-xxxxxxxx', 
			projection:'MERC'
		},
		inegiHeader:false,
		inegiHeaderPath:'https://www.inegi.org.mx/componentes/encabezado/js/enc_genera_v2.min.js',
		routing:{
			dataSource:{ 
				saveStats:{
					url:'stats/layers',
					type: 'POST',
					contentType : "application/json; charset=utf-8",
					dataType: "json",
				}
			}
		},
		denue:{
			active:false,
			visibleScale:2.388657133483887,
			denueSearchLayerId:'cturistadenue',
			excludeLayer:'cdenue14,c111servicios',
			urlDownloadService:'http://10.153.3.13:8080/DescargaDENUE/procesaDescarga.do',
			//urlDownloadService:'http://geoweb2.inegi.org.mx/denue_mdm/',
			tools:{
				'd_what_here':{id:'d_what_here',type:"btn", filter:'',keyWord:'searchLocal',size:30,float:'left',text:'&iquest;Qu&eacute; hay aqu&iacute;?',desc:'Obtener informaci&oacute;n sobre la vista actual',icon:'module-denue-sprite module-denue-sprite-houseSearch'},
				'd_food':{id:'d_food',type:"tool",filter:'Restaurantes',color:'#ff6600',keyWord:'18',size:20,float:'left',desc:'Restaurantes',icon:'module-denue-sprite module-denue-sprite-food'},
				'd_hospital':{id:'d_hospital',type:"tool",filter:'Hospitales',color:'#e02327',keyWord:'12',size:20,float:'left',desc:'Hospitales',icon:'module-denue-sprite module-denue-sprite-hospital'},
				'd_bank':{id:'d_bank',type:"tool",filter:'Bancos',color:'#00ae00',keyWord:'4',size:20,float:'left',desc:'Bancos y cajeros',icon:'module-denue-sprite module-denue-sprite-bank'},
				'd_hotel':{id:'d_hotel',type:"tool",filter:'Hoteles',color:'#0066ff',keyWord:'13',size:20,float:'left',desc:'Sitios para alojamiento',icon:'module-denue-sprite module-denue-sprite-hotel'},
				'd_museum':{id:'d_museum',type:"tool",filter:'Museos',color:'#993300',keyWord:'16',size:20,float:'left',desc:'Museos',icon:'module-denue-sprite module-denue-sprite-museum'},
				
				'd_bar':{id:'d_bar',type:"tool",filter:'Bares',color:'#cc0066',keyWord:'5',size:20,float:'left',desc:'Centros nocturnos',icon:'module-denue-sprite module-denue-sprite-bar'},
				'd_bus':{id:'d_bus',type:"tool",filter:'Centrales de autobuses',color:'#0033ff',keyWord:'7',size:20,float:'left',desc:'Central de autobuses',icon:'module-denue-sprite module-denue-sprite-bus'},
				'd_zoo':{id:'d_zoo',type:"tool",filter:'Zoológicos (Jardines botánicos y zoológicos)',color:'#0b7263',keyWord:'24',size:20,float:'left',desc:'Zool&oacute;gicos',icon:'module-denue-sprite module-denue-sprite-zoo'},
				'd_piramid':{id:'d_piramid',type:"tool",filter:'Sitios históricos',color:'#996600',keyWord:'20',size:20,float:'left',desc:'Sitios hist&oacute;ricos',icon:'module-denue-sprite module-denue-sprite-piramid'},
				'd_mail':{id:'d_mail',type:"tool",filter:'Servicios postales',color:'#ffcc00',keyWord:'19',size:20,float:'left',desc:'Servicios postales',icon:'module-denue-sprite module-denue-sprite-mail'},
				'd_shopping':{id:'d_shopping',type:"tool",filter:'Tiendas departamentales',color:' #ff6699',keyWord:'23',size:20,float:'left',desc:'Tiendas departamentales',icon:'module-denue-sprite module-denue-sprite-shopping'},
				'd_market':{id:'d_market',type:"tool",filter:'Supermercados y minisupers',color:'#00ccff',keyWord:'21',size:20,float:'left',desc:'Super mercados',icon:'module-denue-sprite module-denue-sprite-store'},
				'd_gas':{id:'d_gas',type:"tool",filter:'Gasolineras',color:' #006600',keyWord:'11',size:20,float:'left',desc:'Gasolineras',icon:'module-denue-sprite module-denue-sprite-gas'},
				'd_drugs':{id:'d_drugs',type:"tool",filter:'Farmacias',color:'#e02327',keyWord:'10',size:20,float:'left',desc:'Farmacias',icon:'module-denue-sprite module-denue-sprite-drugs'},
				'd_rentcar':{id:'d_rentcar',type:"tool",filter:'Alquiler de autos',color:'#ff6600',keyWord:'3',size:20,float:'left',desc:'Renta de autos',icon:'module-denue-sprite module-denue-sprite-rentcar'},
				'd_gov':{id:'d_gov',type:"tool",filter:'Oficinas de gobierno',color:'#e02327',keyWord:'17',size:20,float:'left',desc:'Oficinas de gobierno',icon:'module-denue-sprite module-denue-sprite-gov'},
				'd_church':{id:'d_church',type:"tool",filter:'Iglesias',color:'#996600',keyWord:'14',size:20,float:'left',desc:'Iglesias',icon:'module-denue-sprite module-denue-sprite-church'},
				'd_school':{id:'d_school',type:"tool",filter:'Escuelas',color:'#66cc00',keyWord:'8',size:20,float:'left',desc:'Escuelas',icon:'module-denue-sprite module-denue-sprite-school'},
				'd_airport':{id:'d_airport',type:"tool",filter:'Aeropuertos',color:'#067161',keyWord:'1',size:20,float:'left',desc:'Aeropuertos',icon:'module-denue-sprite module-denue-sprite-airport'},
				'd_parking':{id:'d_parking',type:"tool",filter:'Estacionamientos',color:'#0066ff',keyWord:'9',size:20,float:'left',desc:'Estacionamientos',icon:'module-denue-sprite module-denue-sprite-parking'},
				'd_mechanic':{id:'d_mechanic',type:"tool",filter:'Talleres automotrices',color:'#00ddb0',keyWord:'22',size:20,float:'left',desc:'Talleres automotrices',icon:'module-denue-sprite module-denue-sprite-mechanic'},
				'd_package':{id:'d_package',type:"tool",filter:'Mensajería y paquetería',color:'#c69a00',keyWord:'15',size:20,float:'left',desc:'Mensajer&iacute;a y paqueter&iacute;a',icon:'module-denue-sprite module-denue-sprite-package'},
				'd_currency':{id:'d_currency',type:"tool",filter:'Casas de cambio',color:'#00ae00',keyWord:'6',size:20,float:'left',desc:'Casas de cambio',icon:'module-denue-sprite module-denue-sprite-currency'},
				'd_travel':{id:'d_travel',type:"tool",filter:'Agencias de viaje',color:'#0066cc',keyWord:'2',size:20,float:'left',desc:'Agencias de viajes',icon:'module-denue-sprite module-denue-sprite-travel'}
			},
			dataSources:{
					view:{
						url:'http://gaiamapas.inegi.org.mx/mdmSearchEngine/denue_2017/select', //http://gaiamapas.inegi.org.mx/mdmSearchEngine/denue_2014/select',
						type: 'POST',
						dataType: "jsonp",
						jsonp:'json.wrf',
						stringify:false,
						params:{ //Params----------------------
							'sort':'geodist() asc',
							'q':'*',
							'df':'busqueda',
							'wt':'json',
							'indent':true,
							'facet':true,
							'facet.field':'tipo',
							'spatial':true,
							'sfield':'locacion',
							'fq':'{!bbox}',
							'rows':0,
						},
						mainPath:false
					},
					searchInLayer:{
						url:'http://gaiamapas.inegi.org.mx/mdmSearchEngine/denue_2017/select',
						type: 'POST',
						dataType: "jsonp",
						jsonp:'json.wrf',
						stringify:false,
						params:{ //Params----------------------
							'sort':'geodist() asc',
							'q':'*',
							'df':'busqueda',
							'wt':'json',
							'indent':true,
							'facet':true,
							'facet.field':'tipo',
							'spatial':true,
							'sfield':'locacion',
							'fq':'tipo:"text"',
							'rows':10,
						},
						mainPath:false
					},
					//Revisar ip
					searchLayer:{
						url:'http://10.1.30.101:9090/solr/busq-denue_sector/select',
						type: 'POST',
						dataType: "jsonp",
						jsonp:'json.wrf',
						stringify:false,
						params:{ //Params----------------------
							'hl':true,
							'hl.fl':'busqueda',
							'hl.simple.post':'</strong>',
							'hl.simple.pre':'<strong>',
							'rows':10,
							'wt':'json',
							'indent':true,
							'facet':true,
							'facet.field':'tipo'
						},
						mainPath:false
					},
					download:{
						url:'mdmservices/denue/list',
						contentType : "application/json; charset=utf-8",
						type: 'POST',
						dataType: "json"
					}
			}
			
		},	
		attributeTable:{
				dataSources:{
					filter:{
						url:'http://10.152.11.41:8200/mdmSearchEngine/busq-apiyan/select',
						type: 'POST',
						dataType: "jsonp",
						jsonp:'json.wrf',
						stringify:false,
						params:{
							'df':'busqueda',
							'wt':'json',
							'indent':true,
							'rows':20,
							'start':0
						},
						mainPath:false
					},
					exportFile:{
						url:'export/layers/zip',
						contentType : "application/json; charset=utf-8",
						type: 'GET',
						dataType: "json"
					},
					tabulate:{
						url:'identify/layer',
						contentType : "application/json; charset=utf-8",
						type: 'POST',
						dataType: "json",
						stringify:true
					},
					geometry:{
						url:'identify/layer/geometry',
						contentType : "application/json; charset=utf-8",
						type: 'POST',
						dataType: "json",
						stringify:true
					},
					exportData:{
						url:'export',
						urlGet:'export',
						type: 'POST',
						contentType : "application/json; charset=utf-8",
						dataType: "json",
						stringify:true
					}
				}
		},

	}
	
	
});