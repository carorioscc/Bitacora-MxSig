// JavaScript Document
window.layerWidgets.ccpv2010.config = {
		    settings:{
				 project:'mxsig',
				 sourceDataInfo:'Fuente de los datos',
				 mainDoc:'config/layerWidgets/ccpv2010/docs/mainDoc.pdf', //documento de descarga ubicado en los botones principales
				 bootDialog:'config/layerWidgets/ccpv2010/docs/init.html',  //documento html que funge como mensaje de información
				 bootDialogTitle:'Censos de Gobierno, Sección Catastro',  //titulo de la ventana emergente con el mensaje de información
				 docPath:'docs/cenagocatastro', //path archivos para metadatos
				 layerMapName:{
					nal:'MAPAESTATAL',
					lv1:'MAPAMUNICIPAL'
				 },
				 mapDefaultLayers:'d100,d101', //capas por defecto que son enviadas al momento de tematizar
				 cardIndicatorOrder:[  //orden de impresion en tabulado de resultados
									{id:'label',label:'Indicador'},
									{id:'value',label:'Valor'}
								],
				 transparency:100, //transparencia por defecto del tema pintado en mapa
				 colorRamps:[
					 	{id:0,name:'Amarillos',colors:['#FFF28C','#DCBB6A','#BE8447','#9F4C25','#801502']}, //la primer rampa de color debe estar sincronizada con el mapserver
						{id:1,name:'Verdes',colors:['#D8F2ED','#9FC4BE','#6B9993','#3F736D','#144F4A']},
						{id:2,name:'Naranja',colors:['#F5F500','#F5B800','#F57A00','#F53D00','#F50000']},
						{id:3,name:'Azules',colors:['#B6EDF0','#74B4E8','#1F83E0','#1D44B8','#090991']},
						{id:4,name:'Escarlata',colors:['#FFE0E0','#EBA59B','#CF705F','#B04130','#910A0A']},	
						{id:5,name:'Grises',colors:['#BDBFBF','#8B8C8C','#656666','#4C4C4C','#292929']}
				 ],
				 numStrats:5,
				 methods:[ 
						/*{name:'cuantiles',title:'Cuantiles'},
						{name:'nei',title:'N.E.I'},
						{name:'d2r',title:'Dalenius'},
						*/
						{name:'jenks',title:'R. Naturales'}
						],
				 minStrats:1,
				 maxStrats:5,
				 //Valores fijos -------------------------------------------------------------------
				 geoLevels:[],// Carga dinamica de valores desde servicio de action 
				 exportTypes:['xls','csv'], //formatos de exportación que serán solicitados al momento de presentar los datos.
				 varByLevel:{  //indicadores precargados con los que se tematizará al seleccionar un corte geográfico al inicio
					 lv1:{
						descripcion:"Población",
						id:13,
						metadata:false,
						subcat:false,
						theme:true,
						variable:"pobtot",
						geoLevel:1,
						parent:10,
						idTab:10,
						censo:"Censo Nacional de Población y Vivienda 2010",
						subtitle:"Población Total"
					 },
					 nal:{
						descripcion:"Población",
						id:4,
						metadata:false,
						subcat:false,
						theme:true,
						variable:"pobtot",
						geoLevel:'nal',
						parent:1,
						idTab:2,
						censo:"Censo Nacional de Población y Vivienda 2010",
						subtitle:"Población Total"
					 }
				 }
				 //----------------------------------------------------------------------------------
			},
			startingData:{ 
					varActive:null,
					colors:{id:0,name:'Escarlata',colors:['#FFF28C','#DCBB6A','#BE8447','#9F4C25','#801502']},
					method:'jenks',
					strats:5,
					geoLevelActive:1, //debe ser acorde a la posicion de la primer variable activa
					//Valores Fijos----------------------------------------------
					geoIndex:0,  //valor de carga inicial, 0 es inicio del arbol geográfico
					geoSelected:['0'],
					geoType:'1',
					index:0,
					showTotal:'',
					typeVarSelection:'',
					selected:null,
					geoLevel:0,
					currentMapTheme:null,
					tree:[]
					//------------------------------------------------------------
			},
			dataSources:{
				exportData:{
					url:'/mdmservices/export',
					urlGet:'export',
					type: 'POST',
					contentType : "application/json; charset=utf-8",
					dataType: "json",
					stringify:true
			    },
				varlist:{
					url:'/mdmservices/widget/mxsig/indicator', //*
					contentType : "application/json; charset=utf-8",
					type: 'GET',
					dataType: "json",
					params:{alias:'indicadorescpv2010'}
				},
				getGeoConfig:{
					url:'/mdmservices/widget/mxsig/actions',
					contentType : "application/json; charset=utf-8",
					type: 'GET',
					dataType: "json",
					params:{
						project:'cpv2010'
					}
				},
				geolist:{
					url:'/mdmservices/widget/mxsig/catcvegeo',
					contentType : "application/json; charset=utf-8",
					type: 'GET',
					dataType: "json"
				},
				theme:{
					url:'/mdmservices/widget/mxsig/theme', //*
					contentType : "application/json; charset=utf-8",
					type: 'POST',
					dataType: "json",
					stringify:true,
					params:{proy:'mxsig'},
				},
				getExtent:{
					url:'/mdmservices/widget/mxsig/wkt',  //*
					contentType : "application/json; charset=utf-8",
					type: 'GET',
					dataType: "json",
				},
				themeColor:{
					url:'/mdmservices/widget/mxsig/colors', //*
					contentType : "application/json; charset=utf-8",
					type: 'POST',
					dataType: "json",
					stringify:true
				},
				infoPoint:{
					url:'/mdmservices/widget/mxsig/find', //*
					contentType : "application/json; charset=utf-8",
					type: 'POST',
					dataType: "json",
					stringify:true
				},
				getCardValues:{
					url:'/mdmservices/widget/mxsig/label',
					contentType : "application/json; charset=utf-8",
					type: 'POST',
					dataType: "json",
					stringify:true,
					params:{aliasIndicators:'indicadorescpv2010'},

				},
				getTabulated:{
					url:'/mdmservices/widget/mxsig/tabulated', //*
					contentType : "application/json; charset=utf-8",
					type: 'GET',
					dataType: "json",
					params:{alias:'indicadorescpv2010'}
				}
			}
} 
window.layerWidgets.ccpv2010.widgetLoaded = true;
//@ sourceURL=config_catastro.js