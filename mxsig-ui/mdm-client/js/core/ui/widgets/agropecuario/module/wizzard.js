// JavaScript Document
define(function(){
	var wizzard = {
		connect:function(main){
			var obj = this;
			obj.main = main;
		},
		mainEvents:function(evt){
			var obj = this;
			var type = evt.type;
			var value = evt.value;
			switch(type){
				case 'onUICreated':
					obj.injectButton();	
				break;
			}
		},
		injectButton:function(){
			var obj = this;
			var container = $('#agropecuario_toolbar_container');
			container.append('<div id="btnAgroWizzard" idref="wizzard" title="Abrir tutorial" class="agropecuario-resize agropecuario-tab sprite-wizard wizard-open-btn02"></div>');
			$('#btnAgroWizzard').click(function(){
				obj.main.cancelModify();
				obj.printwindows();
				
				
			});
		},
		//variables globales
		geoSelected:null,
		productTable:null,
		edoList:null,
		activitySelected:null,
		localList:null,
		produSelect:{},
		tenSelect:null,
		listTenencia:{},
		menuSelected:'geografico',
		menuStep:1,
		geoName:'Nacional',
		sendSelection:{geografico:"00",actividad:["00"],agua:null,tenencia:["00"]},
		
		resetCurrentData:function(){
			var obj = this;
			obj.currentData.geoSelected=["00"]; //reinicio de corte geografico
			
			//reinicio de actividades
			obj.clearAllActivityProducts();
			var acts = obj.currentData.activitiesList;
			for(var x in acts){
				var item = acts[x];
				item.active = false;	
			}
			acts[0].active = true;
			obj.clearAllTenencia();
			//----------------------------------
		},
		/*Aqui probando el getData*/
		getData:function(source,params,callback,error,before,complete){
			var obj = this;
			if(source){
				var spinner = this.spinner;
				var url = source.url;
				//Anexo de parametros que vengan definidos desde fuente de datos
				var s_params = source.params;
				var stringify = source.stringify;
				
				//control de servidor personalizado
				var proyect = source.proyect;
				var server = obj.main.options.config.dataSources.server; 
				if(proyect && proyect == 'agropecuario')
					source.url = server+source.url;
				//----------------------------------
				
				
	
				if (!(s_params === undefined)){
					for (var x in s_params){ //anexo de la conifuracion del origen de datos
						params[x] = s_params[x];
					};
				}
				if (!(stringify === undefined) && stringify){
					params = JSON.stringify(params);
				}
				//Estructura basica de peticion
				var dataObject = {
					   data: params,
					   success:function(json,estatus){
						   if(!(json && json.response && json.response.success)){
							   var mensaje = 'No hay terrenos que cumplan con los criterios seleccionados';
							   if(json.response.message)
								   mensaje = json.response.message;
								
								if(mensaje != 'false#409')
									obj.main.options.systemMessage(mensaje,{height:130});
						   }
						   
						   callback(json,estatus);
					   },
					   beforeSend: function(solicitudAJAX) {
							obj.spinner('show');
							if ($.isFunction(before)){
								before(params);
							};
					   },
					   error: function(solicitudAJAX,errorDescripcion,errorExcepcion) {
							var url = source.url;
							obj.main.options.systemMessage('Sistema en mantenimiento, favor de verificar más tarde',{height:130});
							if ($.isFunction(error)){
								error(errorDescripcion,errorExcepcion);
							};
					   },
					   complete: function(solicitudAJAX,estatus) {
							obj.spinner('hide');
							if ($.isFunction(complete)){
								complete(solicitudAJAX,estatus);
							};
					   }
				};
				//anexo de la conifuracion del origen de datos
				for (var x in source){ 
					if ( !(/field|name|id|params|stringify/.test(x)))dataObject[x] = source[x];
				};
				jQuery.support.cors = true;
				$.ajax(dataObject);
			}
		},
		
		spinner:function(option){
			var obj = this;
			if(option == 'show'){
				if(!$('#wizard_spinner_panel').attr('id')){
					var w = 497;//obj.element.width();
					var h = "auto";//obj.element.height();
					var cadena = '<div id="wizard_spinner_panel" class="wizard-spinner-panel" count="1" style="width:'+w+'px;height:'+h+'px">';
						cadena+= '	<div class="ui-widget-overlay wizard-block-overlay"></div>';
						cadena+= '	<div class="wizard-spinner-image-container"><span class="wizard-spinner-image"></div>';
						cadena+= '<div>';
		
					//obj.element.append(cadena);	
					$('#agro-wizard-dialog-container').append(cadena);
				}else{
					var count = parseInt($('#wizard_spinner_panel').attr('count'),10);
					$('#wizard_spinner_panel').attr('count',count+1);
		
				}
			}else{
				if($('#wizard_spinner_panel').attr('id')){
					var count = parseInt($('#wizard_spinner_panel').attr('count'),10);
					if(count > 1){
						$('#wizard_spinner_panel').attr('count',count-1);	
					}else{
						$('#wizard_spinner_panel').remove();
					}
				}
			}
		},
		/**/
		/*Aqui es mi dialog ----------------------------------------------------------------------------------------------------*/
		printwindows:function(){
			var obj = this;
			obj.currentData = $.extend(true,{},obj.main.currentData);
			obj.resetCurrentData();
			
			var cadena=	'<div id="wizard-dialog">'+
							'<div id="wizard-header">'+
								'<div id="icnContainer">'+
									'<div class="wizard-imgMenu" id="geografico" step=1 color="blue">'+
										'<div class="sprite-wizard wizard-geografico-01 one" status="1" style="display:none"></div>'+
										'<div class="sprite-wizard wizard-geografico-02 two" status="2" style="display:none"></div>'+
										'<div class="sprite-wizard wizard-geografico-03 three" status="3"></div>'+
									'</div>'+
									'<div class="wizard-bar"></div>'+
									'<div class="wizard-imgMenu" id="actividad" step=2 color="grey">'+
										'<div class="sprite-wizard wizard-actividad-01 one" status="1"></div>'+
										'<div class="sprite-wizard wizard-actividad-02 two" status="2" style="display:none"></div>'+
										'<div class="sprite-wizard wizard-actividad-03 three" status="3" style="display:none"></div>'+
									'</div>'+
									'<div class="wizard-bar"></div>'+
									'<div class="wizard-imgMenu" id="tenencia" step=3 color="grey">'+
										'<div class="sprite-wizard wizard-tenencia-01 one" status="1"></div>'+
										'<div class="sprite-wizard wizard-tenencia-02 two" status="2" style="display:none"></div>'+
										'<div class="sprite-wizard wizard-tenencia-03 three" status="3" style="display:none"></div>'+
									'</div>'+
									'<div class="wizard-bar"></div>'+
									'<div class="wizard-imgMenu" id="seleccion" step=4 color="grey">'+
										'<div class="sprite-wizard wizard-seleccion-01 one" status="1"></div>'+
										'<div class="sprite-wizard wizard-seleccion-02 two" status="2" style="display:none"></div>'+
										'<div class="sprite-wizard wizard-seleccion-03 three" status="3" style="display:none"></div>'+
									'</div>'+
								'</div>'+//fin icnContainer
								'<div id="wizard-btnArea">'+
									'<div class="wizard-btn-wrap">'+
										'<div class="wizard-btn" id="wizard-btn-anterior" style="float:left;cursor:pointer">'+
											'<img src="js/core/ui/widgets/agropecuario/module/wizardSprite/wizard-arrow-left.png" style="float:left">'+
											'<p class="wizard-btn-txt">anterior</p>'+
										'</div>'+
									'</div>'+
									'<div class="wizard-btn-wrap">'+	
										'<div class="wizard-btn" id="wizard-btn-siguiente" style="float:right;cursor:pointer">'+
											'<p class="wizard-btn-txt" style="margin-right:5px">siguiente</p>'+
											'<img src="js/core/ui/widgets/agropecuario/module/wizardSprite/wizard-arrow-right.png" style="float:left">'+
										'</div>'+
									'</div>'+
								'</div>'+//fin wizard-btnArea
							'</div>'+//fin header
							//contenidos
							'<div id="wizard_contents">'+
								//contenido geográfico
								'<div class="wizard-section-container" id="wizard_geografico_container">'+
									'<div class="wizard_seleccion_title">1. Seleccione la Entidad Federativa</div>'+	
									'<div  class="wizard-section-container-geo"  style="overflow-y: auto;"></div>'+
								'</div>'+
								//contenido actividad
								'<div class="wizard-section-container" id="wizard_actividad_container" style="display:none">'+
									'<div id="wizard_activity_container"></div>'+
									//'<div id="wizard_products_container"></div>'+
								'</div>'+
								//contenido tenencia
								'<div class="wizard-section-container" id="wizard_tenencia_container" style="display:none">'+
								'</div>'+
								//contenido VERIFICAR selección
								'<div class="wizard-section-container" id="wizard_seleccion_container" style="display:none">'+
									'<div class="wizard_seleccion_title">4. Verifique su selección</div>'+
									'<div id="seccionGeografico" style="border-bottom:1px solid #f0f0f0; margin-top: 40px;">'+
										'<div class="titleGeografico"><p id="titleSeleccionResume">Corte geográfico: <span id="wizard_geografico_selection" class="wizard-seleccion-txt">Nacional</span></p></div>'+
										//'<div class="seleccionGeografico"><p>prueba</p></div>'+
									'</div>'+
									'<div id="seccionActividad" style="border-bottom: 1px solid #f0f0f0">'+
											'<div id="titleActividad"><p id="titleSeleccionResume">Actividad principal: <span id="wizard_activity_selection" class="wizard-seleccion-txt">Agricultura </span></p></div>'+
											'<div id="wizard_resume_product" style="padding-left:35px"><p id="titleSeleccionResume">Productos de la actividad: <span id="wizard_product_selection" class="wizard-seleccion-txt">Todos los productos </span></p></div>'+
											'<div id="wizard_resume_tipoAgua" style="padding-left:35px; color:#3696a8; display:none"><p id="titleSeleccionResume">Disposición de agua: <span id="wizard_tipoAgr_selection" style="color:grey; font-weight:500">temporal, riego</span></p></div>'+
										//'<div id="seleccionActividad"><p></p></div>'+
									'</div>'+
									'<div id="seccionTenencia" style="border-bottom: 1px solid #f0f0f0;">'+
										'<div id="titleTenencia"><p id="titleSeleccionResume">Tipo de tenencia:<span id="wizard_tenencia_seleccion" style="color:grey; font-weight:500">Todas las tenencias</span></p></div>'+
										'<div id="seleccionTenencia-wrap"></div>'+
									'</div>'+
									//botón aceptar
									'<div class="wizardAcceptbtn_wrap">'+
										'<button class="wizardAcceptSelection">aceptar</button>'
									'</div>'+
								'</div>'+
							'</div>'//fin wizard_contents
					'</div>';
						
			$('body').append('<div id="agro-wizard-dialog"><div id="agro-wizard-dialog-container"></div></div>');
			$( "#agro-wizard-dialog" ).dialog({ width: 497,position: ['center',200] });
			$('#agro-wizard-dialog-container').html(cadena);
			obj.geoSelected=obj.currentData.geoSelected;
			obj.eventos();
			obj.printGeoList(); //contenido de geográfico
			obj.printActivitiesWizard();
			obj.wizardTenencia();//Tenencia 
		},
		eventos:function(){
			var obj=this;
			$(".wizard-imgMenu").click(function(){
				//ocultar contenedores de sección y mostrar el contenedor del menú seleccionado
				var btn="menuClick"
				var identificador=$(this).attr("id");
				var currentMenu=obj.menuSelected;
				var step=$(this).attr("step"); 
				if(step<4){
					$("#wizard-btn-siguiente").show();
				}else{$("#wizard-btn-siguiente").hide();}
				var allowNext=obj.validaNextStep(step, identificador); //valida si puede avanzar en el menú
				if(allowNext==true){
					obj.updateMenu(btn,currentMenu,identificador);
					obj.menuSelected=identificador;
				}
			});
			
			$("#wizard-btn-anterior").click(function(){
				var btn="anterior";
				var currentMenu=obj.menuSelected;
				var section=obj.newMenuSection(obj.menuSelected,btn);
				var step=obj.menuStep; 
					$("#wizard_product_selection").html("Todos los productos")
					if(step<4){
					$("#wizard-btn-siguiente").show();
					}	
				obj.updateMenu(btn,currentMenu,section);
				
			});
			$("#wizard-btn-siguiente").click(function(){
				var btn="siguiente"
				var currentMenu=obj.menuSelected;
				var section=obj.newMenuSection(obj.menuSelected,btn);
				var step=obj.menuStep; 
				if(step==4){
					$("#wizard-btn-siguiente").hide();
				}
				obj.updateMenu(btn,currentMenu,section);
			});
			$(".wizardAcceptSelection").click(function(){
				console.log(obj.sendSelection);
			});
		},
		
		
		//Validar si al dar siguiente ya puede avanzar
		validaNextStep:function(paso,identificador){
			var obj=this;
			var allowNext=false;
			var step=parseInt(paso);
			var color=$("#"+identificador+"").attr('color');
			if ((step<=obj.menuStep)||(color=="blue")){
				allowNext=true;
				obj.menuStep=step;
			}
			return(allowNext);
		},
		//cambiar el contenedor a la sección que corresponda al usar los botones anterior y siguiente
		newMenuSection:function(currentSection,direccion){
			var obj=this;
			var r=null;
			switch(currentSection) {
				case 'geografico':
					 if(direccion=="anterior")	{
					 	r="geografico";
						obj.menuStep=1;
					 }else{r="actividad";obj.menuStep=2;}
					break;
				case 'actividad':
					if(direccion=="anterior")	{
					 	r="geografico";
						obj.menuStep=1;
					 }else{r="tenencia";obj.menuStep=3;}
					break;
				case 'tenencia':
					if(direccion=="anterior")	{
					 	r="actividad";
						obj.menuStep=2;
					 }else{r="seleccion"; obj.menuStep=4;}
					break;	
				case 'seleccion':
					if(direccion=="anterior")	{
					 	r="tenencia";
						obj.menuStep=3;
					 }else{r="seleccion";obj.menuStep=4;}
					break;
			}
			obj.menuSelected=r;
			return r;
		},
		
		updateMenu:function(btn,currentMenu,identificador){
			var obj=this;
			$(".wizard-section-container").css("display","none");
			$("#wizard_"+identificador+"_container").css("display","block");
			if(btn){
				$(".wizard-"+currentMenu+"-01").css("display", "none"); //del anterior oculta el gris
				$(".wizard-"+currentMenu+"-03").css("display", "none"); //del anterior oculta el verde
				$(".wizard-"+currentMenu+"-02").show(); //enciende el azul
				$("#"+currentMenu+"").attr('color','blue');
			}
			$(".wizard-"+identificador+"-01").css("display", "none"); //del actual oculta el gris
			$(".wizard-"+identificador+"-02").css("display", "none"); //del actual oculta el azul
			$(".wizard-"+identificador+"-03").show(); //enciende el verde
			$("#"+identificador+"").attr('color','green');
		},
		
		//Contenido geografico
		printGeoList:function(isRefresh){
			var obj = this;
			var typeList = ['Estados'];
			var list=obj.currentData.currentGeo;
			
			 
				if(list.length > 0){
					var cadena = '';
					for(var x in list){
							var item = list[x];
							var isSelected = (obj.geoSelected.indexOf(item.cvegeo) >= 0);
							cadena+= '<div class="wizard-geoEdo-item" label="'+item.nombre.toLowerCase()+'" idparent="'+parent+'" idref="'+item.cvegeo+'" '+((isSelected)?'selected="selected"':'')+'>';
							cadena+= '	<div class="wizard-geoEdo-item-label">'+item.nombre+'</div>';
							cadena+= '	<div class="wizard-geoEdo-icon" idref="'+item.cvegeo+'">';
							cadena+= '		<div class="wizard-geoEdo-icon-sel sprite-agropecuario-circle"></div>';
							cadena+= '		<div class="wizard-geoEdo-icon-unsel sprite-agropecuario-ok"></div>';
							cadena+= '	</div>';

							//comentado VER MAS...
							if(item.childs && false){ //preenta icono de avanzar solo cuando llega hasta nivel de municipio
								cadena+= '	<div idref="'+item.cvegeo+'" class="wizard-geo-seemore">';
								cadena+= '		<div class="wizard-geoEdo-icon-seemore sprite-agropecuario-forward"></div>';
								cadena+= '	</div>';
							}
							cadena+= '</div>';
					 }
					$('.wizard-section-container-geo').html(cadena);

					$('.wizard-geoEdo-icon').each(function(){
						$(this).click(function(e){
							var idref = $(this).attr('idref');
							obj.selGeoItem(idref);
							//sendSelection:{geografico:"00",actividad:["00"],agua:null,tenencia:["00"]},
							//guardar valor para enviar parametros para tematización
							obj.sendSelection.geografico=idref;
							obj.geoName=$(this).prev('div:first').html(); 
							$("#wizard_geografico_selection").html(obj.geoName);
							e.stopPropagation();
						})

					});

				}
				obj.main.getGeoListItems(function(list){
					obj.currentData.currentGeo = list;
					if(!obj.edoList)obj.edoList = list;
				});
		 },
		 selGeoItem:function(idgeo){
			 	 var obj = this;
				 //var gsel = obj.main.currentData.geoSelected;
			 	 var gsel = obj.geoSelected;

				 if(gsel.indexOf(idgeo) >= 0){
					 if(idgeo != '00'){
						if(gsel.length > 1){
							gsel.splice(gsel.indexOf(idgeo),1);
							obj.geoSelected = gsel;
						}
					 }
				 }else{
					 gsel = []; //Limpia siempre eleccion, sólo uno a la vez
					 /*
					 //si no es nacional permite seleccionar estados
					 if(gsel[0] == '00' || idgeo == '00'){
						 gsel = [];
					 }
					 */

					 gsel.push(idgeo);
					 obj.geoSelected = gsel;
				 }
				 obj.printGeoList('refresh');
				 
		 },
		 clearAllActivities:function(){
				var obj = this;
				var list = obj.currentData.activitiesList;
				for(var x in list){
					list[x].active = false;
				}  	
			},
				
		 //Contenido geografico
		//Contenido Actividadades
			printActivitiesWizard:function(){
			  var obj = this;
			  var list = obj.currentData.activitiesList;
			  var cadena = '';
			  cadena+='<div class="wizard_seleccion_title">2. Seleccione la actividad principal</div>';
				for(var x in list){
					var item = list[x];
					var active = list[x].active;
					cadena+= '<div idref="'+item.id+'" pos="'+x+'" class="wizard-activity-item  sprite-agropecuario-activity-'+item.id+((active)?'':'-off')+'"></div>';	
				}
			  $('#wizard_activity_container').html(cadena);
			
			
		 		$('.wizard-activity-item').each(function(){
		 			  $(this).click(function(){
						 var pos = parseInt($(this).attr('pos'),10);
						  obj.activitySelected =list[pos];
						 //si es agricultura a cielo abierto mostrar en el resumen de selección el tipo de riego elegido
						 if(pos==1){
							 $("#wizard_resume_tipoAgua").show();
						 }else{$("#wizard_resume_tipoAgua").hide();}
						 // 
						 if(list[pos].id != obj.currentData.activity){
							obj.clearAllActivities();
							obj.clearAllActivityProducts();
							list[pos].active = true;
							obj.currentData.activity = list[pos].id;
							obj.printActivitiesWizard();
							 //Actualiza tenencias
							//obj.setActiveAllTenencia();
							//obj.printTenencia();
						}
						//Llenar el apartado de ver selección 
						$("#wizard_activity_selection").html(obj.activitySelected.nombre);
						//
						 if(pos<5){
							 $("#wizard_resume_product").show();
						 }else{$("#wizard_resume_product").hide();}
							obj.printProducts();
							
		  				});
					});
  		},
		loadProducts:function(idact,func){
			var obj =this; 
			//var list = obj.currentData.activitiesList;
			var list=obj.localList;
			var ds = obj.main.options.config.dataSources;
			var dataSource = $.extend(true,{},ds.activityProducts);
				dataSource.url+='?id_actividad='+idact;
				obj.getData(dataSource,{},function(data){
					func(data)
				});
 		 },
		 getActiveProducts:function(){
			  var obj = this;
			  var list = obj.currentData.activitiesList;
			  var products = [];
			  for(var x in list){
				  if(list[x].id == obj.currentData.activity){
					var prods = list[x].products;
					if(prods)
						for(var y in prods){
						if(prods[y].active){
							products.push(prods[y]);
						}
					}
				  }
				  
			  }
			  return products;
		  },
		  
		  isValidProduct:function(pto,geos){
				var obj = this;
				var list = obj.productTable;
				var act = obj.getCurrentActivity().prefijo;
				var r = true;
				var valid = false;
				for(var v in geos){
					var geo = geos[v];
					if(list['pto_'+pto] && list['pto_'+pto]['rel_'+act+'_'+geo]){
						valid = true;
					}
					if(!valid)break;
				}
				r = valid;
				return (r); 
		 },
		 
			getProduct:function(id){
				var obj = this;
				//var list = obj.currentData.activitiesList;
				var list=obj.localList;
				var currentActId=obj.activitySelected.id;
				var products = [];
				  for(var x in list){
					  if(list[x].id == currentActId ){
						var prods = list[x].products;
						if(prods)
							$.merge(products,prods);

					  }

					}
				var prods =  products; 
				var r = null;
				for(var x in prods){
					if(prods[x].id_producto == id){
						r = prods[x];
						break;
					}
				}
				return r;
		 },
		 
		  
	  clearAllActivityProducts:function(){
		var obj = this;
		//var list = obj.currentData.activitiesList;
		var list = obj.localList;   
		for(var x in list){
			var prods = list[x].products;
			if(prods)
			for(var y in prods){
				prods[y].active = false;
			}
		}  
	  },
		setActivityStatus:function(id,status){
			var obj = this;
			var list = obj.currentData.activitiesList;
			//var list = obj.localList; 
			if(id == 0){
				obj.clearAllActivityProducts();
			}else{
				for(var x in list){
					var prods = list[x].products;
					if(prods)
					for(var y in prods){
						if(prods[y].id_producto == id){
							prods[y].active = status;
							break;
						}
					}
				}  
			}
		  },
		  getCurrentActivity:function(){
			  var obj = this;
			  var obj = this;
			  var list = obj.currentData.activitiesList;
			  var act = null;
			  for(var x in list){
				  if(list[x].id == obj.currentData.activity){
					  act = list[x];
					  break;
				  }
			  }
			  return act;
		  },
		  countActiveTools:function(){
			  var obj = this;
			  var act = obj.getCurrentActivity();
			  var tools = (act.tools)?act.tools.attributes:[];
			  var count = 0;
			  if(tools){
				  for(var x in tools){
					  if(tools[x].active)
						  count++;
				  }
			  }
			  return count;
		  },
		
		   //PRUEBA
		   productRelation:function(func){
				var obj =this; 
				var list = obj.currentData.activitiesList;
				obj.localList=list;
			    var ds = obj.main.options.config.dataSources;
				var dataSource = $.extend(true,{},ds.productRelation);
					obj.getData(dataSource,{},function(data){
						func(data)
					});
			},
		
			
		  //PRUEBA
		printProducts:function(pos){
			var obj =this;
			$("#wizard_actividad_container").css("display", "block");
			//PRUEBA
			obj.productRelation(function(result){
					if(result.response && result.response.success){
						var r = {}; //traducción de objeto
						/*   Se traduce a la estructura siguiente
						'pto_02':{  producto_idcultivo
						'rel_a_02':'true'; relacion_actividad_estado
						}
						*/
						var list = result.data.products
						for(var x in list){
							var item = list[x];
							var id = item.cve_cultivo;
							r['pto_'+id] = {};
							var listppal = item.ppal;
							for(var y in listppal){
								var itemp = listppal[y];
								var idp = itemp.cve_ppal;
								var edos = itemp.cve_entidades;
								for(var z in edos){
									var iteme = edos[z];
									r['pto_'+id]['rel_'+idp+'_'+iteme] = true;
								}
							}
						}
						obj.productTable = r;
						
						
					}
				});
			//PRUEBA
			var obj =this;
			var list = obj.currentData.activitiesList;
			var act = obj.getCurrentActivity();
			var prods = act.products;
			if(!prods){ //de no tener cargados los productos los carga y asigna a el listado
				obj.loadProducts(act.id,function(data){
					if(data.response.success){
						 act.products = data.data.catalogo;
						 obj.printProducts();
					}
				});
			}else{
				if(prods.length > 0){ //Si tiene productos la actividad
					$('#wizard_actividad_container').effect('drop',600,function(){ //animacion de cambio de contenido
						var tools = act.tools;
						var geos = obj.currentData.geoSelected;
						var activeProds = obj.getActiveProducts();
						var cadena = '';
							cadena+= '	<div id="wizard_products_container" class="wizard-products-container" '+((tools)?'specialtools="true"':'')+'>';
							  cadena+='<div class="wizard_seleccion_title">2.1 Seleccione los productos</div>';
							//la activadad requiere selección especial de tipo
							if(tools){
								cadena+= '<div id="wizard_products_type" class="wizard-resize wizard-products-type">'; 
								var types = tools.attributes;
								for(var x in types){
									var item = types[x];
									cadena+= '<div title="'+item.label+'" idref="'+x+'" class="ui-corner-all sprite-agropecuario-dagua-tool sprite-agropecuario-dagua-'+x+' '+((item.active)?'active':'')+'"></div>';
								}
								cadena+= '</div>'
							 } 
						
							cadena+= '		<div id="wizard_products_content" class="wizardresize wizard-products-content">';
						
							
							//inclusión visual de producto
							var allActive = (activeProds.length == 0)?true:false;
								cadena+= '		<div class="wizard-products-item">';
								cadena+= '			<div class="wizard-products-item-label wizard-truncate" title="Todos los productos">Todos los productos</div>';
								cadena+= '			<div class="wizard-products-item-check" idref="00" active="'+allActive+'">';
								cadena+= '				<div idref="00" class="wizard-products-item-check-icon sprite-agropecuario sprite-agropecuario-circle inactive"></div>';   
								cadena+= '				<div idref="00" class="wizard-products-item-check-icon sprite-agropecuario sprite-agropecuario-ok active"></div>';
								cadena+= '			</div>';
								cadena+= '			<div class="wizard-products-item-disable-mask"></div>';
								cadena+= '		</div>';
							//fin de ajuste visual
						
		
							for(var x in prods){
								var item = prods[x];
									item.active = (item.active == true);
								var active = item.active;
								var disabled = (obj.isValidProduct(item.id_producto,geos))?'':' disabled="disabled" ';
								cadena+= '		<div class="wizard-products-item" '+disabled+'>';
								//cadena+= '			<span class="agropecuario-products-item-info ui-icon ui-icon-info" idref="'+item.id_producto+'"></span>';
								cadena+= '			<div class="wizard-products-item-label wizard-truncate" title="'+item.nombre+'">'+item.nombre+'</div>';
								cadena+= '			<div class="wizard-products-item-check" idref="'+item.id_producto+'" active="'+active+'">';
								cadena+= '				<div idref="'+item.id_producto+'" class="wizard-products-item-check-icon sprite-agropecuario sprite-agropecuario-circle inactive"></div>';   
								cadena+= '				<div idref="'+item.id_producto+'" class="wizard-products-item-check-icon sprite-agropecuario sprite-agropecuario-ok active"></div>';   
								cadena+= '			</div>';
								cadena+= '			<div class="wizard-products-item-disable-mask">';
								cadena+= '				<span class="wizard-products-item-info ui-icon ui-icon-info" idref="'+item.id_producto+'"></span>';	
								cadena+= '			</div>';
								cadena+= '		</div>';
							}
		
							cadena+= '		</div>';
							cadena+= '		<div id="wizard_products_bk_btn" class="wizard-resize agropecuario-animated wizard-products-bk-btn"><div class="sprite-agropecuario-bback"></div></div>';
							cadena+= '	</div>';
		
							$('#wizard_activity_container').html(cadena);
							
							$('#wizard_actividad_container').fadeIn();
		
							$('#wizard_products_content').attr('content','list');
							//obj.uiSettings.activityPanel = list; 
							
							$('.wizard-products-item-info').each(function(){
								$(this).click(function(){
									var idref = $(this).attr('idref');
									obj.showProductInfo(idref);
								});
							});
						
							$('.sprite-agropecuario-dagua-tool').each(function(){
								$(this).click(function(){
									var idref = $(this).attr('idref');
									var isActive = $(this).hasClass('active');
									//siempre debe existir una herramienta activada
									if(isActive){
										if(isActive && obj.countActiveTools() > 1){ //si esta activa la herramienta es porque se quiere desactivar, verificar si almenos hay otra activa
											tools.attributes[idref].active = !isActive;
											$(this).removeClass('active');
											
											//obj.prepareTheme();
										}
									}else{
										tools.attributes[idref].active = !isActive;
										$(this).addClass('active');
										//obj.prepareTheme();
									}
									//Sacar que disposición de agua quedó seleccionada
									$("#wizard_tipoAgr_selection").html();	
									var t=$(".sprite-agropecuario-dagua-t").hasClass('active');
									var r=$(".sprite-agropecuario-dagua-r").hasClass('active');
									if((r==true)&&(t==true)){
										$("#wizard_tipoAgr_selection").html("temporal, riego");	
										obj.sendSelection.agua=null;//enviar dato para tematización
									}else{
										if(r==true){
											$("#wizard_tipoAgr_selection").html("riego");
											obj.sendSelection.agua="r";//enviar dato para tematización
										}
										if(t==true){
											$("#wizard_tipoAgr_selection").html("temporal");
											obj.sendSelection.agua="t";//enviar dato para tematización
										}
									}
									//
									//console.log(idref)
								});
							});
						
		
							$('#wizard_products_bk_btn').click(function(){
								$('#wizard_products_container').fadeOut(function(){
									obj.printActivitiesWizard();
									$('#wizard_activity_container').effect('slide',600);
								});
							});
							$('.wizard-products-item-check').each(function(){
								$(this).click(function(){
									var idref = parseInt($(this).attr('idref'),10);
									var active = ($(this).attr('active') == 'true');
									var produSelect=activeProds;
										active = !active;
										obj.setActivityStatus(idref,active);
										$(this).attr('active',active);
										
										var activeProds = obj.getActiveProducts();
										
										
										//Llenar el apartado de ver selección 
										var productsName=[];
										var productsIdrefs=[];//juntar ids para enviar para tematizar
										for (var x in activeProds){
											var i= activeProds[x];
											productsName.push(i.nombre);
											productsIdrefs.push(i.id_producto)
										}
										var allSelectedProducts=productsName.join();
										
										//sendSelection:{geografico:"00",actividad:["00"],agua:null,tenencia:["00"]},
										//guardar valor para enviar parametros para tematización
										obj.sendSelection.actividad=productsIdrefs;
										
										$("#wizard_product_selection").html(allSelectedProducts);
										//
										
										//ajuste visual
										$('.wizard-products-item-check').each(function(){
											$(this).attr('active','false');
										})
										if(activeProds.length == 0){
											$('.wizard-products-item-check[idref=00]').attr('active',true);
											$("#wizard_product_selection").html("Todos los productos");
												produSelect=activeProds;
											//console.log(produSelect)
										}else{
											for(var x in activeProds){
												var prod = activeProds[x];
												//var produSelect=activeProds;
												$('.wizard-products-item-check[idref='+prod.id_producto+']').attr('active',true);
											}
											produSelect=activeProds;
											//console.log(produSelect)
										}
										//console.log(produSelect)
										//console.log(activeProds)
								});
							});
					}); 
				}
			}
			
		  },
		
		  showProductInfo:function(id){
			var obj = this;
			var pto = obj.getProduct(id);
			var list = obj.productTable['pto_'+id];
			var edos = obj.edoList;
			var edoList = {};
			var acts = obj.currentData.activitiesList;
			for(var x in edos){
				//if(edos[x].cvegeo != '00'){
					edoList['e_'+edos[x].cvegeo]=edos[x].nombre;
				//}
			}
			acts_list = {};
			for(var x in acts){
					acts_list[acts[x].prefijo]=acts[x].nombre;
			}


			var cadena = 'La información para el producto <b>'+pto.nombre+'</b> sólo está disponible para las siguientes entidades federativas:</br></br>';
				cadena+= '<table class="agropecuario-custom-table"><thead><tr><th width="20%">Producto</th><th width="20%">Actividad</th><th width="60%">Entidades federativas</th></tr></thead><tbody>';
			var type = '';
			for(var x in list){
				var item = x.split('_');
				var cultivo = item[1];
				var edo = item[2];
				//if(edo != '00'){
					if(type == ''){
						type = cultivo;
						cadena+= '<tr><td>'+pto.nombre+'</td><td>'+acts_list[cultivo]+'</td><td>';
					}else{
						if(cultivo != type){
							cadena+='</td></tr><tr><td>'+pto.nombre+'</td><td>'+acts_list[cultivo]+'</td><td>';
							type = cultivo;
						}
					}
					cadena+='<span class="agropecuario-products-item-edo" title="'+edoList['e_'+edo]+'">'+edoList['e_'+edo]+'</span></br>'
				//}
			}
			cadena+='</td></tr></tbody></table>';

			var mensaje = cadena;
			obj.main.options.systemMessage(mensaje,{title:'Información de '+pto.nombre,width:390,height:330});
  		  },
		
		  clearAllActivities:function(){
				var obj = this;
				var list = obj.currentData.activitiesList;
				for(var x in list){
					list[x].active = false;
				}  	
			},
		  clearAllActivityProducts:function(){
			var obj = this;
			var list = obj.currentData.activitiesList;
			for(var x in list){
				var prods = list[x].products;
				if(prods)
				for(var y in prods){
					prods[y].active = false;
				}
			}  
		  },
		//Fin CA
		/*Tenencia*/
		clearAllTenencia:function(){
			var obj =this;
			var list = $.extend([],obj.currentData.tenenciaList);
			var listTenencia=list;
			
			  for(var x in listTenencia){
				  listTenencia[x].active = false;
			  }
		 },
  		getActiveTenencia:function(){
			var obj =this;
			var list = obj.currentData.tenenciaList;
			var r = [];
			  for(var x in list){
				  if(list[x].active)
					r.push(list[x]);
			  }
			return r;
		 },
		 setTenenciaStatus:function(id,status){
			 var obj =this;
			 var list = obj.currentData.tenenciaList;
			  for(var x in list){
				  if(list[x].id == id){
					list[x].active = status;
					  break;
				  }
			  } 
		 },
		wizardTenencia:function(){
			var obj =this;
			var list = $.extend([],obj.currentData.tenenciaList);
			var listTenencia=list;  
				list=0;
			/*if(list.length ==  obj.currentData.tenenciaList.length){
				obj.clearAllTenencia();
				list = obj.currentData.tenenciaList;
			}*/
			  
			var act = list;
			if(listTenencia.length > 0){ //Si tiene tenencias
				var cadena = '';
					var t = obj.getActiveTenencia();
					var allActive = (t.length == 0)?true:false;
					//control visual para todas las tenencias
					cadena+='<div class="wizard_seleccion_title">3. Seleccione el tipo de Tenencia</div>';
					cadena+= '	<div class="wizar-tnencias">';
					cadena+= '		<div class="wizard-tenencia-item">';
					cadena+= '			<div class="wizard-tenencia-item-label wizard-truncate">Todas las tenencias</div>';
					cadena+= '			<div class="wizard-tenencia-item-check" idref="00" active="'+allActive+'">';
					cadena+= '				<div idref="00" class="wizard-tenencia-item-check-icon sprite-agropecuario sprite-agropecuario-circle inactive"></div>';   
					cadena+= '				<div idref="00" class="wizard-tenencia-item-check-icon sprite-agropecuario sprite-agropecuario-ok active"></div>';   
					cadena+= '			</div>';
					cadena+= '		</div>';
					cadena+= '	</div>';
		
					for(var x in listTenencia){
						var item = listTenencia[x];
							item.active = (item.active == true);
						var active = item.active;
						
						cadena+= '		<div class="wizard-tenencia-item">';
						cadena+= '			<div class="wizard-tenencia-item-label agropecuario-truncate">'+item.nombre+'</div>';
						cadena+= '			<div class="wizard-tenencia-item-check" idref="'+item.id+'" active="'+active+'">';
						cadena+= '				<div idref="'+item.id+'" class="wizard-tenencia-item-check-icon sprite-agropecuario sprite-agropecuario-circle inactive"></div>';   
						cadena+= '				<div idref="'+item.id+'" class="wizard-tenencia-item-check-icon sprite-agropecuario sprite-agropecuario-ok active"></div>';   
						cadena+= '			</div>';
						cadena+= '		</div>';
					}
		
					cadena+= '		</div>';
					cadena+= '	</div>';
		
					$('#wizard_tenencia_container').html(cadena);
					$('.wizard-tenencia-item-check').each(function(){
						$(this).click(function(){
							var idref = parseInt($(this).attr('idref'),10);
							var active = ($(this).attr('active') == 'true');
							obj.setTenenciaStatus(idref,!active);
							var t = obj.getActiveTenencia();
							/*if(!active || (active && t.length > 1)){
								active = !active;
								obj.setTenenciaStatus(idref,active);
								$(this).attr('active',active);
								obj.prepareTheme();
							}*/
							if(idref == 0 || (t.length ==  listTenencia)){
								obj.clearAllTenencia();
							}
							t = obj.getActiveTenencia();
							$('.wizard-tenencia-item-check').each(function(){
									$(this).attr('active','false');
							})
							if(t.length == 0){
								$('.wizard-tenencia-item-check[idref=00]').attr('active',true);
								obj.sendSelection.tenencia=["00"];
							}else{
								for(var x in t){
									var tenencia = t[x];
									tenSelected=t;
									$('.wizard-tenencia-item-check[idref='+tenencia.id+']').attr('active',true);
									
								}
							}
								//console.log(tenSelected)
								//Llenar el apartado de ver selección 
								var tenenciaSelected=[];
								var allSelectedTenencia='';
								var tenenciasId=[];
								$("#wizard_tenencia_seleccion").html("");
								if(idref==0){
									allSelectedTenencia="todas las tenencias";
									obj.sendSelection.tenencia=["00"];//enviar valor para tematización
									$("#wizard_tenencia_seleccion").html("Todas las tenencias");
								}else{
									for (var x in tenSelected){
										var i= tenSelected[x];
										tenenciaSelected.push(i.nombre);
										tenenciasId.push(i.id);
									}
								
									allSelectedTenencia=tenenciaSelected.join();
									obj.sendSelection.tenencia=tenenciasId;
								    $("#wizard_tenencia_seleccion").html(allSelectedTenencia);
									//$("#wizard_activity_selection").html(allSelectedProducts);
								//
								}
								console.log(obj.sendSelection.tenencia)
	
						});
						
					});
			}
		 },
		
		
		/*Fin tenencia*/	
	}
	return wizzard;
})