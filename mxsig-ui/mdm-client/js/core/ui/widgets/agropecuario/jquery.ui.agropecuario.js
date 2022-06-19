$.widget( "custom.agropecuario", {
  // default options
  options: {
	  config:null,
	  getMapStatus:function(){},
	  markerWithText:function(){},
	  initCount:0,
	  collision:function(){},
	  onCollision:function(){},
	  clearIdentify:function(){}
  },
  backupData:null,
  backupConfig:null,
  validatedParams:null, //parametros que han sido procesados de forma satiffactoria
  zoomData:[],
  geoProducts:{},
  edoList:null,
  productTable:null,
  currentCard:'resume',
  currentData:null,
  hasChanged:false,
  collapsed:true,
  ready:false,
  openCount:0,
  moduleEvent:function(evt,value){
	  	var obj = this;
		var modules = obj.options.modules;
	  	if(modules)
		for(var x in modules){
			modules[x].mainEvents({type:evt,value:value});
		}  
  },
  jsonTransactions:[],
  // the constructor
  startUp:function(){
	var obj = this;  
	var modules = obj.options.modules;
	if(modules)
	for(var x in modules){
		modules[x].connect(obj);
	}
	obj.options.onStart(); 
	 
	//levanta listados de datos antes de iniciar el widget  
	obj.loadDataList(function(){  //carga actividades
		  obj.loadTenenciaList(function(){ //carga tipo de tenencia
			  	//carga productos default
			  	obj.loadBootProducts(function(){
					obj.adjustProductByFilterTable();
					obj.createUI();
					obj.setActiveAllTenencia();
					obj.printTenencia();
					//inicia tema al iniciar
					obj.prepareTheme();
					obj._refresh();
					obj.printAbout();
					if(obj.options.initCount == 1)
						obj.disclaimer();
					
					obj.moduleEvent('onUICreated',true);
				})
		  });
	});  
  },
  restoreAllUI:function(){
	var obj = this;
	obj.currentData = $.extend(true,{},obj.backupConfig);
	obj.printGeoList();
	obj.printActivities();
	obj.printTenencia();
	obj.printThemeDetail();
	  
	setTimeout(function(){
		$('#agropecuario_activity_content').attr('content','mosaic');
	},100);
  },
  _create: function() {
	var obj = this;
	obj.tutor = obj.options.tutor;
	obj.id = obj.element.attr('id');
	obj.uiSettings = {activityPanel:'mosaic'}; 
	obj.currentData = $.extend({},obj.options.config.config.startingData);
	obj.settings = $.extend({},obj.options.config.config.settings);
	
	var customServer = $.getURLParam('agropecuarioserver'); 
	if(customServer && customServer != ''){
		obj.options.config.dataSources.server = customServer;
	}
	  
	this.element
	  // add a class for theming
	  .addClass( "custom-agropecuario agropecuario-animated toolCustomIdentify" ).attr('collapsed','true').attr('seltab','geo').attr('changed','false').attr('geotype',obj.currentData.geoType)
	  .attr('expandwidth','false').addClass('no-print')
	  // prevent double click to select text
	  .disableSelection();
	 this.id = this.element.attr('id');
	 
	obj.element.addClass('no-print');
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
			setTimeout(function(){ //inicia todo el widget una vez que se obtiene la tabla traducida
				obj.startUp();
			},500);
			
		}
	});
	
  },

  // called when created, and later when changing option
  _refresh: function() {
	  // trigger a callback/event
	  
  },

  // events bound via _on are removed automatically
  // revert other modifications here
  _destroy: function() {
	this.onClose();
	this.element
	  .removeClass( "custom-agropecuario" )
	  .enableSelection();
  },

  // _setOptions is called with a hash of all options that are changing
  // always refresh when changing options
  _setOptions: function() {
	// _super and _superApply handle keeping the right this-context
	this._superApply( arguments );
	this._refresh();
  },

  // _setOption is called for each individual option that is changing
  _setOption: function( key, value ) {
	// prevent invalid color values
	this._super( key, value );
  },
	
  getVarsData:function(){
	var obj = this;
	if(obj.element.attr('collapsed') == 'true'){
		return obj.currentData;
	}else{
		return obj.backupData;
	}
  },
  //------------------------
   createUI:function(){
	  var obj = this;
	  var cd = obj.currentData;  
	  obj.printGeoList();
	  var 	cadena = '<div id="agropecuario_header" class="agropecuario-header agropecuario-resize agropecuario-transition">';
			cadena+=	'<div id="agropecuario_header_logoLeft" idref="" class="agropecuario-logo-left"></div>';
	   		cadena+=	'<div id="agropecuario_header_title" class="agropecuario-truncate-text"><div id="agropecuario_var_title">Seleccione una variable</div><div id="agropecuario_subvar_title"></div></div>';
	   		cadena+=	'<div class="agropecuario-header-btns">';
	   		cadena+=	'	<div id="agropecuario_header_btnRight" idref="" class="agropecuario-header-btn">';
			cadena+=	'		<div id="agropecuario_header_btnRight_modify" title="Configurar" idref="" class="agropecuario-header-btn-inner sprite-agropecuario sprite-agropecuario-modify"></div>';
	   		cadena+=	'	</div>';
	   		cadena+=	'	<div id="'+obj.id+'_header_btnRight_close" title="Minimizar" idref="" class="agropecuario-header-btnRight-close agropecuario-header-btn-inner sprite-agropecuario sprite-agropecuario-down"></div>';
	   		cadena+=	'	<div id="'+obj.id+'_header_btnRight_swapcard" title="Cerrar" idref="" class="agropecuario-header-swapcard agropecuario-header-btn-inner sprite-agropecuario-sclose" style="display:none;"></div>';
	   		cadena+=	'</div>';
			
			cadena+= '<div class="agropecuario-strats-transparency-container">';
			cadena+= '		<div class="agropecuario-strats-transparency-title">Transparencia</div>';
			cadena+= '		<div id="agropecuario_strats_trasparencyControl" class="agropecuario-strats-transparency-tool"></div>';
			cadena+= '</div>';
	   
	   		cadena+=	'<div id="agropecuario_header_colors" class="agropecuario-header-colors" show="true">';
	   		cadena+=	'	<div id="agropecuario-year-min" title="Límite mínimo" class="agropecuario-header-year-label-left"></div>';
	   		cadena+=	'	<div class="agropecuario-colors-ramp">';

							var colors = obj.currentData.colors.colors;
							for(var x in colors){
								var color = colors[x];
								cadena+=	'<div class="agropecuario-header-colors-item" style="background-color:'+color+';width:'+Math.floor(100/colors.length)+'%"></div>';
							}
	   		cadena+=	'	</div>';
	   		cadena+=	'	<div id="agropecuario-year-max" title="Límite máximo" class="agropecuario-header-year-label-right"></div>';
			cadena+=	'</div>';
	   
	  		cadena+= '</div>';
	   		
			cadena+= '<div id="agropecuario_panels_container" class="agropecuario-panels-container agropecuario-animated" panel="vars">';

	   		cadena+= '	<div id="agropecuario_geo_container" class="agropecuario-geo-container agropecuario-tab-container">';
			cadena+= '		<div id="agropecuario_geo_bk_btn" class="agropecuario-resize agropecuario-animated agropecuario-geo-bk-btn"><div class="agropecuario-geo-bk-btn-icon sprite-agropecuario sprite-agropecuario-bback"></div></div>';
	   		cadena+= '		<div id="agropecuario_geo_content" class="agropecuario-resize agropecuario-animated agropecuario-geo-content"></div>';
	   		cadena+= '		<div id="agropecuario_geo_type" class="agropecuario-resize agropecuario-animated agropecuario-geo-type">';
			cadena+= '		</div>';
			cadena+= '	</div>';
	   		cadena+= '	<div id="agropecuario_conf_container" class="agropecuario-conf-container agropecuario-tab-container">';
			cadena+= '		<div id="agropecuario_conf_content" class="agropecuario-resize agropecuario-animated agropecuario-conf-content"></div>';
			cadena+= '	</div>';
	   		cadena+= '	<div id="agropecuario_activity_container" class="agropecuario-activity-container agropecuario-tab-container">';
			cadena+= '		<div id="agropecuario_activity_content" content="mosaic" class="agropecuario-resize agropecuario-activity-content">actividad</div>';
			cadena+= '	</div>';
	   		cadena+= '	<div id="agropecuario_type_container" class="agropecuario-type-container agropecuario-tab-container">';
			cadena+= '		<div id="agropecuario_type_content" class="agropecuario-resize agropecuario-animated agropecuario-type-content">tipo tenencia</div>';
			cadena+= '	</div>';
	   		cadena+= '	<div id="agropecuario_info_container" type="grid" class="agropecuario-info-container agropecuario-table-style agropecuario-tab-container">';
			cadena+= '		<div id="agropecuario_info_content" class="agropecuario-resize agropecuario-animated agropecuario-info-content"></div>';
	   		cadena+= '		<div id="agropecuario_info_content_graph" class="agropecuario-resize agropecuario-animated agropecuario-info-content-graph"></div>';
	   		cadena+= '		<div class="agropecuario-info-export-btns">';
			
	   		cadena+= '			<div title="Ver gráfica" idref="graph" class="agropecuario-btn-view-type sprite-agropecuario-graph"></div>';
	   		cadena+= '			<div title="Ver tabulado" idref="grid" class="agropecuario-btn-view-type sprite-agropecuario-grid"></div>';
	   
	   		cadena+= '			<div title="Descargar en formato XLS" idref="xls" class="agropecuario-btn-export-info sprite-agropecuario-doc-xls"></div>';
			cadena+= '			<div title="Descargar en formato CSV" idref="csv" class="agropecuario-btn-export-info sprite-agropecuario-doc-csv"></div>';
	   		cadena+= '			<div title="Expandir interfaz" type="expand" class="agropecuario-btn-window-size sprite-agropecuario-expand-limit"></div>';
	   		cadena+= '			<div title="Contraer interfaz" type="collapse" class="agropecuario-btn-window-size sprite-agropecuario-collapse-limit"></div>';
			cadena+= '		</div>';
			cadena+= '	</div>';
	   		cadena+= '	<div id="agropecuario_about_container" class="agropecuario-about-container agropecuario-tab-container">';
			cadena+= '		<div id="agropecuario_about_content" class="agropecuario-resize agropecuario-animated agropecuario-about-content">Acerca de</div>';
			cadena+= '	</div>';
	   		cadena+= '</div>';
	   
	   		cadena+= '<div id="agropecuario_toolbar_container" class="agropecuario-toolbar-container agropecuario-animated">';
	   		cadena+= '	<div id="agropecuario_tab_vars"  idref="var" class="agropecuario-resize agropecuario-tab sprite-agropecuario-vars"></div>';
	   		cadena+= '	<div id="agropecuario_tab_geo" title="Seleccione la entidad federativa a consultar" idref="geo" class="agropecuario-resize agropecuario-tab sprite-agropecuario-geo"></div>';
	   		cadena+= '	<div id="agropecuario_tab_activity" title="Seleccione la actividad principal a consultar" idref="activity" class="agropecuario-resize agropecuario-tab sprite-agropecuario-bactividad"></div>';
	   		cadena+= '	<div id="agropecuario_tab_type" title="Seleccione la tenencia a consultar" idref="type" class="agropecuario-resize agropecuario-tab sprite-agropecuario-btenencia"></div>';
	   		cadena+= '	<div id="agropecuario_tab_info" idref="info" title="Visualice la información detallada del tema actual" class="agropecuario-resize agropecuario-tab sprite-agropecuario-binfo"></div>';
	   		cadena+= '	<div id="agropecuario_tab_about" idref="about" title="Metodología y aviso" class="agropecuario-resize agropecuario-tab sprite-agropecuario-babout"></div>';
	   		/*cadena+= '	<div id="agropecuario_tab_bottom_container" idref="info" class="agropecuario-tab-bottom-container agropecuario-animated">';
			cadena+= '		<a href="'+obj.settings.docPath+'/'+obj.settings.mainDoc+'" target="_blank"><div id="agropecuario_tab_pdfdoc" title="Actualización del marco censal agropecuario 2016" idref="pdfdoc" class="agropecuario-resize sprite-agropecuario-doc-pdf"></div></a>';
			cadena+= '	</div>';*/
			cadena+= '</div>';

				
	  obj.element.html(cadena);
	  obj.printActivities();
	   
	   $('.agropecuario-btn-view-type').each(function(){
		  $(this).click(function(e){
			 	var type = $(this).attr('idref'); 
			  	$('#agropecuario_info_container').attr('type',type);
		  }); 
	   });
	   $('.agropecuario-btn-window-size').each(function(){
		  $(this).click(function(e){
			 	var expand = ($(this).attr('type')=='expand'); 
			  	obj.element.attr('expandwidth',expand);
		  });
	   });
	   $( "#agropecuario_strats_trasparencyControl" ).slider({
		  range: "max",
		  min: 1,
		  max: 100,
		  value: obj.settings.transparency,
		  slide: function( event, ui ) {
			obj.settings.transparency = ui.value;
			obj.settings.transparency = ui.value;
			obj.options.onTransparency(ui.value);
		  }
	  });
	  if(!obj.ready){
		  obj.options.onTransparency(obj.settings.transparency);
	  } 
	   
	   $('#'+obj.id+'_header_btnRight_swapcard').click(function(e){
		   var card = $('.agropecuario-header-card-container').attr('card') || 'resume';
		   card = (card == 'resume')?'detail':'resume';
		   obj.currentCard = card;
		   $('.agropecuario-header-card-container').attr('card',card);
		   $('#toolAgropecuario_header_btnRight_swapcard').hide();
		   e.stopPropagation();
	   });
	   $('.agropecuario-tool-option').each(function(){
		  $(this).click(function(){
			 var idref = $(this).attr('idref');
			 
			 if(idref != obj.currentData.typeVarSelection){
				obj.hasChanged = true;	  
				$('#agropecuario_container').attr('typeselection',idref);
				 obj.currentData.typeVarSelection = idref;
				 obj.prepareTheme();
			 }
		  });
	   });
	   
	   $('#'+obj.id+'_header_btnRight_close').click(function(){
		   obj.cancelModify();
	   });
	   
	   $('.agropecuario-tab').each(function(){
		   $(this).click(function(e){
			   var idref = $(this).attr('idref');
			   obj.element.attr('seltab',idref);
			   e.stopPropagation();
		   })
	   });
	   
	   
	  $('#agropecuario_header').click(function(e){
			  if($('#agropecuario_panels_container').height() == 0){
				obj.openConfig();
				e.stopPropagation();
			  }
   	 });

	  
	  $('#agropecuario_header_btnRight').click(function(){
		  if($('#agropecuario_panels_container').height() == 0){
			obj.openConfig();
		  }else{
			obj.closeConfig();
		  }
	   });
	   $('#agropecuario_bk_btn').click(function(){
		   if(obj.currentData.tree.length > 1){
			  obj.currentData.tree.pop();
			  if(obj.currentData.tree.length <= 1){
				  $('#agropecuario_container').attr('parent','false');
			  }
			  obj.loadTree(obj.currentData.tree[obj.currentData.tree.length-1].id,true);
		   }
	   });
	  
	  obj.element.fadeIn();
  },
	loadBootProducts:function(func){
		var obj = this;
		var actPreset = obj.options.config.config.startingData.activity;
		var actProdsPreset = obj.options.config.config.startingData.products;
		if(actPreset && actProdsPreset.length > 0){
			obj.loadProducts(actPreset,function(data){
				var act = obj.getCurrentActivity();
				if(data.response.success){
					var a = 'hola';
					act.products = data.data.catalogo;
					var lp = act.products;
					 for(var x in lp){
						 var item = lp[x];
						 var prefijo = item.prefijo;
						 if(actProdsPreset.indexOf(prefijo) >= 0){
							item.active = true;
						 }
					 }
					func();
				}
			});
		}else{
			func();
		}
	},
	loadDataList:function(func){
		var obj = this;
		var ds = obj.options.config.dataSources;
		var dataSource = $.extend(true,{},ds.activityList);
			//dataSource.url+='/'+cvegeo;
			obj.getData(dataSource,{},function(data){
			  if(data.response.success){
				obj.currentData.activitiesList = data.data.catalogo;
				
				for(var x in obj.currentData.activitiesList){
					var item = obj.currentData.activitiesList[x];
					//marca actividad que por defecto viene activa
					if(item.id == obj.currentData.activity){
						item.active = true;
					}
					//identifica si es una actividad que requiere de herramientas especiales
					if(item.prefijo.toLowerCase() == obj.settings.specialProducts.prefix.toLowerCase()){
						item.tools = obj.settings.specialProducts;
					};
				}
				obj.currentData.activitiesList.push({id: 99, nombre: "Todas las actividades"});
				func();
			  }
		});
	},
	loadTenenciaList:function(func){
		var obj = this;
		var ds = obj.options.config.dataSources;
		var dataSource = $.extend(true,{},ds.tenencia);
			//dataSource.url+='/'+cvegeo;
			obj.getData(dataSource,{},function(data){
			  if(data.response.success){
				obj.currentData.tenenciaList = data.data.catalogo;
				func();
			  }
		});
	},
//print geoTypes
 printGeoTypes:function(geo){
	var obj = this;
	geo = geo[0]; 
	var cData = obj.currentData;
	var rules = cData.geoTypes;
	var sRule = null; //rule result
	
	for(var x in rules){ //se implementa un sistema de reglas bajo las cuales se define el tipo de dato seleccionado
		var rule = rules[x].rule;
		var rt = rule.substr(0,1);
		
		switch(rt){
			case 'e': //equal
				sRule = (geo == rule.substr(1,rule.length-1))?rules[x].list:null;
			break;
			case 'n': //number of chars
				sRule = (geo.length == parseInt(rule.substr(1,rule.length-1),10)?rules[x].list:null);
			break;
		}
		if(sRule)break;
	}
	 
	//Impresion de typos geograficos para la selección
	if(sRule){
		var ltypes = [];
		for(var x in sRule){
			var sItem = sRule[x];
			ltypes.push(sItem.id);
		}
		if(ltypes.indexOf(cData.geoType)< 0){
			cData.geoType =ltypes[0];
		}
		
		var cadena = '';
		for(var x in sRule){
			var sItem = sRule[x];
			var isSelected = (cData.geoType == sItem.id)?'selected':'';
			cadena+= '<div idref="'+sItem.id+'" class="agropecuario-geotype-item '+isSelected+'">'+sItem.val+'</div>';
		}
		
		$('#agropecuario_geo_type').html(cadena);
		$('.agropecuario-geotype-item').each(function(){
			$(this).click(function(){
				var idref = $(this).attr('idref');
				cData.geoType = idref;
				$('.agropecuario-geotype-item.selected').removeClass('selected');
				$(this).addClass('selected');
				obj.hasChanged = true;
				obj.prepareTheme();
			});
		});
	}
 },
 //--------- Vars Display-----------------------------------------------------------------
  reloadTree:function(){
	  var obj = this;
	  var id = obj.currentData.tree[obj.currentData.tree.length-1].id;
	  obj.loadTree(id,true);
  },
  loadTree:function(id,reload){
	  var obj = this;
	  /*
	  var ds = obj.options.config.dataSources;
	  var dataSource = $.extend(true,{},ds.varlist);
	  
	  var params = {
		  		id:id
		  }

	  if(!reload || obj.currentData.tree.length == 0){
		  obj.getData(dataSource,params,function(data){
			   //si el elemento es una recarga no agrega el elemento al arbol de peticiones
			  if(data.data){
				   var list = data.data.registros || [];
				   
				   obj.currentData.tree.push({id:id,list:list});
				   //si hay mas de un nivel, coloca el boton de regreso
				   if(obj.currentData.tree.length > 1){
						$('#agropecuario_container').attr('parent','true');
				   }
				   
				   if(data && data.data && list){
					   obj.printItemList(list);
				   }
			   }
			   
		  });
	  }else{
		  obj.printItemList(obj.currentData.tree[obj.currentData.tree.length-1].list);
	  }
	  */
  },
  updateHeader:function(){
	var obj = this;
	//ajustes version  agropecuario
	if(obj.collapsed){ //animacion imagen encabezado
		if(!$('#agropecuario_header_logoLeft').hasClass('agropecuario-logo-left'))
			$('#agropecuario_header_logoLeft').fadeOut(function(){
				$('#agropecuario_header_logoLeft').attr('class','').addClass('agropecuario-logo-left');	
				$('#agropecuario_header_logoLeft').fadeIn();
			})
	}else{
		if(!$('#agropecuario_header_logoLeft').hasClass('sprite-agropecuario-activity-'+obj.currentData.activity))
			$('#agropecuario_header_logoLeft').fadeOut(function(){
				$('#agropecuario_header_logoLeft').attr('class','').addClass('sprite-agropecuario-activity-'+obj.currentData.activity);
				$('#agropecuario_header_logoLeft').fadeIn();
			})
	}
	
	var geoList = obj.getCurrentGeoDataActive();
	var geoNames = [];
	for(var x in geoList)geoNames.push(geoList[x].nombre);
	geoNames = geoNames.join();
	  
	var totalTerrain = obj.currentData.theme.terrenos;
	  
	var act = obj.getCurrentActivity();
	var prod = obj.getActiveProducts();
	var ten = obj.getActiveTenencia();
	  
	var actName = act.nombre;
	var prodsName = [];
	for(var x in prod)prodsName.push(prod[x].nombre);
	prodsName = prodsName.join();
	  
	  
	var geoType = obj.currentData.geoType;
	var typeVar = obj.currentData.typeVarSelection;
	  
	var t = obj.currentData.theme; //tema
		
	var cadena = '<div class="agropecuario-inner-header">';
		cadena+= '	<div class="agopecuario-header-title" title="'+geoNames+'">'+geoNames+'</div>';
		cadena+= '	<div class="agopecuario-header-total">';
	  	cadena+= '		<div class="agopecuario-header-total-value-container">';
	  	cadena+= '			<div class="agopecuario-header-total-label">Terrenos</div>';
	  	cadena+= '			<div class="agopecuario-header-total-value">'+totalTerrain.format()+'</div>';
		cadena+= '		</div>';
	  	cadena+= '		<div id="agopecuario_header_switch" class="agopecuario-header-switch">';
	  	cadena+= '			<div class="sprite-agropecuario-info-right" title="Totales por actividad"></div>';
	  	cadena+= '			<div class="sprite-agropecuario-info-left" title="Totales por actividad"></div>';
	  	cadena+= '		</div>';
	  	cadena+= '	</div>';
		cadena+= '</div>';
	  	cadena+= '<div class="agropecuario-header-card-container" card="'+obj.currentCard+'">';
	  	cadena+= '	<div class="agopecuario-header-card agopecuario-header-card-1">';
	  	cadena+= '		<div class="agopecuario-header-card-title">'+actName+'</div>';
	  	cadena+= '		<div class="agopecuario-header-card-subtitle">'+prodsName+'</div>';
	  	cadena+= '	</div>';
	  	cadena+= '	<div class="agopecuario-header-card agopecuario-header-card-2">';
	  	cadena+= '		<table class="agropecuario-header-detail" cellspaciong="0" cellpadding="0">';
	  	cadena+= '		<tr><td class="agropecuario-table-title" title="Agricultura">Agric.</td><td class="agropecuario-table-value" title="Agricultura">'+t.t_a.format()+'</td><td class="agropecuario-table-title" title="Ganadería">Ganad.</td><td class="agropecuario-table-value" title="Ganadería">'+t.t_g.format()+'</td><td class="agropecuario-table-title" title="Otra actividad">Otra act.</td><td class="agropecuario-table-value" title="Otra actividad">'+t.t_oa.format()+'</td></tr>';
	  	cadena+= '		<tr><td class="agropecuario-table-title" title="Forestal">Fores.</td><td class="agropecuario-table-value" title="Forestal">'+t.t_f.format()+'</td><td class="agropecuario-table-title" title="Sin actividad">Sin act.</td><td class="agropecuario-table-value" title="Sin actividad">'+t.t_sa.format()+'</td><td colspan="2"></td></tr>';
	  	cadena+= '		</table>';
	  	cadena+= '	</div>';
	  	cadena+= '</div>';
	  
	$('#agropecuario_var_title').html(cadena);  
	$('#agopecuario_header_switch').click(function(e){
		   var card = $('.agropecuario-header-card-container').attr('card') || 'resume';
		   card = (card == 'resume')?'detail':'resume';
		   obj.currentCard = card;
		   $('.agropecuario-header-card-container').attr('card',card);
		   $(this).attr('card',card);
		
			if(card == 'resume'){
				$('#toolAgropecuario_header_btnRight_swapcard').hide();
			}else{
				$('#toolAgropecuario_header_btnRight_swapcard').show();
			}
		
		   e.stopPropagation();
	   });
	
	  
  },
  printItemList:function(list){
	  var obj = this;
	  var cadena = '';
	  var count = 0;
	   for (var x in list){
		   var item = list[x];
		   var active = (obj.currentData.varActive && obj.currentData.varActive.id == item.id);
		   var canSelect = (item.theme);
			
			cadena+= '<div class="agropecuario-list-item" idref="'+item.id+'"><label>';
		   
		   if(item.metadata)
			cadena+= '	<span idref="'+item.variable+'" class="agropecuario-info-var-icon sprite-agropecuario-info"></span>';
			
		    cadena+= 	item.descripcion+'</label>';
			
			if(canSelect){
				if(!active){
					cadena+= '	<div idref="'+item.id+'" class="agropecuario-item-check sprite-agropecuario sprite-agropecuario-circle"></div>';   
				}else{
					cadena+= '	<div idref="'+item.id+'" class="agropecuario-item-check sprite-agropecuario sprite-agropecuario-ok"></div>';   
				}
			}
			
			if(item.subcat){
				cadena+= '<div idref="'+item.id+'" class="agropecuario-item-forward sprite-agropecuario sprite-agropecuario-forward"></div>';   
			}
			cadena+= '</div>';
	   }
	   $('#agropecuario_content').html(cadena);
	  
	   $('.agropecuario-info-var-icon').each(function(){
		  $(this).click(function(){
			  var idref = $(this).attr('idref');
			  obj.openMetadataVar(idref);
		  });
	   });
	  
	   $('.agropecuario-item-check').each(function(index, element) {
		   $(this).click(function(e){
				var idref = $(this).attr('idref');
				obj.selectVar(idref);
				e.stopPropagation();
		   })
		});
	   
	   $('.sprite-agropecuario-forward').each(function(index, element) {
		   $(this).click(function(e){
				var idref = $(this).attr('idref');
			   	obj.selectIndex(idref);
				e.stopPropagation();
		   })
		});
  },
  getVar:function(id){
	  var obj = this;
	  var r = null;
	  var list = obj.currentData.tree[obj.currentData.tree.length-1].list;
	  for(var x in list){
		  var item = list[x];
		  if(item.id == id){
			 r = item; 
			 break;
		  } 
	  }
	  return r;
  },
//------------------Geo display---------------------------------------	
 gotoExtent:function(cvegeo){
	var obj = this;
	var ds = obj.options.config.dataSources;
	 
	var dataSource = $.extend(true,{},ds.getExtent);
	 dataSource.url+='/'+cvegeo;
	  obj.getData(dataSource,{},function(data){
		  if(data.response.success){
				var extent = data.data.extent;
			  	obj.options.extent(extent);
		  }
	  });
 },
 getGeoListItems:function(func){
	var obj = this;
	
	var typeList = ['Estados'];
	// control Tree
	if(obj.currentData.geoTree.length == 0){
		obj.currentData.geoTree.push([{childs:false,cvegeo:"00",nombre:"Nacional",active:true}]); //in case first geo element
		obj.currentData.geoSelected = ['00'];
	}
	 
	obj.getGeoList(obj.currentData.geoIndex,function(data){
		var list = 	(data.Estados)?data.Estados:
					(data.municipios)?data.estados:null;
		
		if(data.Estados){
			list.unshift({childs:false,cvegeo:"00",nombre:"Nacional"});
		}
		
		func(list);
		
	});
	
 },
 printGeoList:function(isRefresh){
	var obj = this;
	var typeList = ['Estados'];
	
	 var printList = function(list){
		if(list.length > 0){
			var cadena = '';
			for(var x in list){
					var item = list[x];
					var isSelected = (obj.currentData.geoSelected.indexOf(item.cvegeo) >= 0);
					cadena+= '<div class="agropecuario-geoEdo-item" label="'+item.nombre.toLowerCase()+'" idparent="'+parent+'" idref="'+item.cvegeo+'" '+((isSelected)?'selected="selected"':'')+'>';
					cadena+= '	<div class="agropecuario-geoEdo-item-label">'+item.nombre+'</div>';
					cadena+= '	<div class="agropecuario-geoEdo-icon" idref="'+item.cvegeo+'">';
					cadena+= '		<div class="agropecuario-geoEdo-icon-sel sprite-agropecuario-circle"></div>';
					cadena+= '		<div class="agropecuario-geoEdo-icon-unsel sprite-agropecuario-ok"></div>';
					cadena+= '	</div>';
					
					//comentado VER MAS...
					if(item.childs && false){ //preenta icono de avanzar solo cuando llega hasta nivel de municipio
						cadena+= '	<div idref="'+item.cvegeo+'" class="agropecuario-geo-seemore">';
						cadena+= '		<div class="agropecuario-geoEdo-icon-seemore sprite-agropecuario-forward"></div>';
						cadena+= '	</div>';
					}
					cadena+= '</div>';
			 }
			$('#agropecuario_geo_content').html(cadena);
			
			$('.agropecuario-geoEdo-icon').each(function(){
				$(this).click(function(e){
					var idref = $(this).attr('idref');
					obj.selGeoItem(idref);
					e.stopPropagation();
				})
				
			});
			
		}
	}
	obj.getGeoListItems(function(list){
		obj.currentData.currentGeo = list;
		if(!obj.edoList)obj.edoList = list;
		printList(list);
		obj.printGeoTypes(obj.currentData.geoSelected); 
		obj.prepareTheme();
	});
 },
 selGeoItem:function(idgeo){
	 var obj = this;
	 var gsel = obj.currentData.geoSelected;
	 
	 if(gsel.indexOf(idgeo) >= 0){
		 if(idgeo != '00'){
			if(gsel.length > 1){
				gsel.splice(gsel.indexOf(idgeo),1);
				obj.currentData.geoSelected = gsel;
		 		obj.adjustProductByFilterTable();
				obj.hasChanged = true;
				obj.prepareTheme();
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
		 
		 obj.gotoExtent(idgeo);
		 
		 gsel.push(idgeo);
		 obj.currentData.geoSelected = gsel;
		 obj.adjustProductByFilterTable();
		 obj.hasChanged = true;
		 obj.prepareTheme();
	 }
	 obj.printGeoList('refresh');
	 obj.printActivities();
 },
 getGeoList:function(parent,func){
	  var obj = this;
	  var ds = obj.options.config.dataSources;
	  
	  var idGeo = parent;
	  /*var func = geo.callback;*/
	  
	  var obj = this;
	  var dataSource = $.extend(true,{},ds.geolist);
	  obj.getData(dataSource,{cvegeo:idGeo,zm:false},function(data){
		  if(data.response.success){
				if ($.isFunction(func)){
					func(data.data);
			    }
		  }
	  });
  },
spinner:function(option){
	var obj = this;
	if(option == 'show'){
		if(!$('#agropecuario_spinner_panel').attr('id')){
			var w = obj.element.width();
			var h = obj.element.height();
			var cadena = '<div id="agropecuario_spinner_panel" class="agropecuario-spinner-panel" count="1" style="width:'+w+'px;height:'+h+'px">';
				cadena+= '	<div class="ui-widget-overlay agropecuario-block-overlay"></div>';
				cadena+= '	<div class="agropecuario-spinner-image-container"><span class="agropecuario-spinner-image"></div>';
				cadena+= '<div>';

			obj.element.append(cadena);	
		}else{
			var count = parseInt($('#agropecuario_spinner_panel').attr('count'),10);
			$('#agropecuario_spinner_panel').attr('count',count+1);

		}
	}else{
		if($('#agropecuario_spinner_panel').attr('id')){
			var count = parseInt($('#agropecuario_spinner_panel').attr('count'),10);
			if(count > 1){
				$('#agropecuario_spinner_panel').attr('count',count-1);	
			}else{
				$('#agropecuario_spinner_panel').remove();
			}
		}
	}
},
// Print theme detail
createThemeGraphUI:function(data){
	var obj = this;
	var list = data.headers;
	var prods = obj.getActiveProducts();
	var sup = [];
	for(var x in list){
		var item = list[x];
		if(item.indexOf('superficie') >= 0){
			sup.push({var:x,label:item});
		}
	}
	
	var cadena = '';
	cadena+= '<div class="agropecuario-graph-selector-container">';
	cadena+= '<label><b>Seleccionar el tipo de tenencia a graficar</b></label>';
	cadena+= '<div id="agropecuario_tenencia_list">';
	var count = 0;
	for(var x in sup){
		var item = sup[x];
		cadena+= '<div  class="agropecuario-tenencia-graph-item" idref="'+item.var+'" ><label>'+item.label+'</label>';
		
		cadena+= '	<div class="agropecuario-tenencia-graph-item-check" label="'+item.label+'" idref="'+item.var+'" active="'+((count == 0)?'true':'false')+'">';
		cadena+= '		<div idref="'+item.var+'" class="agropecuario-tenencia-item-check-icon sprite-agropecuario sprite-agropecuario-circle inactive"></div>';   
		cadena+= '		<div idref="'+item.var+'" class="agropecuario-tenencia-item-check-icon sprite-agropecuario sprite-agropecuario-ok active"></div>';
		cadena+= '	</div>';
		
		cadena+= '</div>';
		count++;
	}
	cadena+= '</div>';
	
	if(prods.length > 0){
		cadena+= '</br><label><b>Seleccionar el producto a graficar</b></label>';
		cadena+= '<select id="agropecuario_product_list" class="agropecuario-graph-list">'
		for(var x in prods){
			var item = prods[x];
			cadena+= '<option value="'+item.prefijo+'" '+((x == 0)?'selected="selected"':'')+'>'+item.nombre+'</option>';

		}
		cadena+= '</select>';
	}
	cadena+= '<button id="agropecuario-btn-graph" class="agropecuario-btn-clean">Ver gráfica</button>';
	
	cadena+= '</div>';
	
	cadena+= '<div id="agropecuario_graph_container" class="agropecuario-graph-container">';
	cadena+= '</div>';
	
	
	$('#agropecuario_info_content_graph').html(cadena).attr('type','selectors');
	
	
	var getSelected = function(){
		var sel = [];
		$('.agropecuario-tenencia-graph-item-check[active=true]').each(function(){
			sel.push({id:$(this).attr('idref'),label:$(this).attr('label')});
		});
		return sel;
	}
	
	$('.agropecuario-tenencia-graph-item-check').each(function(){
		$(this).click(function(){
			var selected = getSelected();
			var active = $(this).attr('active');
			if((active =='true') && (selected.length > 1)){
				$(this).attr('active',!(active == 'true'));
			}else{
				if(active =='false')
					$(this).attr('active',!(active == 'true'));
			}
		});
	});
	
	$('#agropecuario-btn-graph').click(function(){
		$('#agropecuario_info_content_graph').attr('type','graph');
		var s = getSelected();
		var p = null;
		if($('#agropecuario_product_list').attr('id')){
			p = {id:$('#agropecuario_product_list option:selected').val(),label:$('#agropecuario_product_list option:selected').text()}
		}
		
		obj.createGraph({data:data,s:s,p:p});
	});
},
createThemeGraphUI_:function(data){
	var obj = this;
	var list = data.headers;
	var prods = obj.getActiveProducts();
	var sup = [];
	for(var x in list){
		var item = list[x];
		if(item.indexOf('superficie') >= 0){
			sup.push({var:x,label:item});
		}
	}
	
	var cadena = '';
	cadena+= '<div class="agropecuario-graph-selector-container">';
	cadena+= '<label>Tipo de tenencia</label>';
	cadena+= '<select id="agropecuario_superfice_list" class="agropecuario-graph-list">'
	var count = 0;
	for(var x in sup){
		var item = sup[x];
		cadena+= '<option value="'+item.var+'" '+((count == 0)?'selected="selected"':'')+'>'+item.label+'</option>';
		count++;
	}
	cadena+= '</select>';
	
	if(prods.length > 0){
		cadena+= '<label>Producto</label>';
		cadena+= '<select id="agropecuario_product_list" class="agropecuario-graph-list">'
		for(var x in prods){
			var item = prods[x];
			cadena+= '<option value="'+item.prefijo+'" '+((x == 0)?'selected="selected"':'')+'>'+item.nombre+'</option>';

		}
		cadena+= '</select>';
	}
	cadena+= '<button id="agropecuario-btn-graph" class="agropecuario-btn-clean">Ver gráfica</button>';
	
	cadena+= '</div>';
	
	cadena+= '<div id="agropecuario_graph_container" class="agropecuario-graph-container">';
	cadena+= '</div>';
	
	
	$('#agropecuario_info_content_graph').html(cadena).attr('type','selectors');
	$('#agropecuario-btn-graph').click(function(){
		$('#agropecuario_info_content_graph').attr('type','graph');
		var s = {id:$('#agropecuario_superfice_list option:selected').val(),label:$('#agropecuario_superfice_list option:selected').text()}
		var p = null;
		if($('#agropecuario_product_list').attr('id')){
			p = {id:$('#agropecuario_product_list option:selected').val(),label:$('#agropecuario_product_list option:selected').text()}
		}
		
		obj.createGraph({data:data,s:s,p:p});
	});
},
createGraph:function(options){
	var obj = this;
	var data = options.data.content;
	var s = options.s;
	var p = options.p;
	var act = obj.getCurrentActivity();
	var colors = obj.settings.colorGraph; 
	
	
	var title = '';
	var _data = [];
	var series = [];
	
	var titlesTenencia = [];
	var saveSerieVal = function(label,val){  //guarda el valor en la serie correspondiente
			var found = false;
			for(var x in series){
				if(series[x].name == label){
					found = true;
					series[x].data.push(val);
				}
			}
			if(!found){
				series.push({name:label,data:[]});
				saveSerieVal(label,val);
			}
	}
	
	
	
	
	for(var c in s){
		var sitem = s[c];
		//title = p.label.toUpperCase();
		title = (p)?sitem.label.toUpperCase()+':'+p.label.toUpperCase():sitem.label.toUpperCase();
		//title = title.replace('HA','(HA)');
		var cats = [];
		var count = 0;
		for(var x in data){
			var xitem = data[x];
			
			titlesTenencia.push(xitem.nombre);  //guarda las categorias
			
			var getCats = (cats.length == 0);
			if( (p && (xitem['producto_especie'].toLowerCase() == p.label.toLowerCase())) || (!p)  ){ //label.toLowerCase()){
				var val = ($.isNumeric(xitem[sitem.id]))?xitem[sitem.id]:0;
				count++;
				var label = sitem.label.replace(' ha ',' (ha) ')
				saveSerieVal(label,val);
			}
		}
	}
	
	var width = ($(window).width()*0.7);
	var height = 500+((s.length)*350);
	$('#agropecuario_graph_inner').css('width',(width-130)+'px').css('height',height+'px');
	
	//agropecuario_graph_container
	var cadena = '<div id="agropecuario_graph_bk_btn" class="agropecuario-resize agropecuario-animated agropecuario-graph-bk-btn"><div class="sprite-agropecuario-bback"></div></div>';
		cadena+= '<div id="agropecuario_graph"><div id="agropecuario_graph_inner"></div></div>';

	$('#agropecuario_graph_container').html(cadena);
	$('#agropecuario_graph_bk_btn').click(function(){
		$('#agropecuario_info_content_graph').attr('type','selectors');	
	});
	$('#agropecuario_graph_bk_btn').css({width:'25px'});
	
	var typeBar = {t_2:'entidad federativa',t_5:'municipio'}
	var l = data[1].cvegeo.length;

	typeBar = (l)?typeBar['t_'+l]:'';
	Highcharts.chart('agropecuario_graph_inner', {
		chart: {
			type: 'bar',
			height:height,
			width:(width-140)
		},
		lang: { 
            printChart: 'Imprimir gráfica',
            downloadPNG: 'Descargar imágen PNG',
            downloadJPEG: 'Descargar imágen JPEG',
            downloadPDF: 'Descargar como PDF',
            downloadSVG: 'Descargar como SVG',
            contextButtonTitle: 'Menú contextual'
        },
		title: {
			text: 'Superfice por '+typeBar
		},
		subtitle: {
			text: ((p)?'Producto: '+p.label:'') 
		},
		xAxis: {
			categories: titlesTenencia,
			title: {
				text: null
			}
		},
		yAxis: {
			min: 0,
			title: {
				text: 'Superficie (Ha)',
				align: 'high'
			},
			labels: {
				overflow: 'justify',
                formatter: function () {
					var ret,
					numericSymbols = ['mil', 'M', 'G', 'T', 'P', 'E'],
					i = numericSymbols.length;
					if(this.value >=1000) {
					   while (i-- && ret === undefined) {
						   multi = Math.pow(1000, i + 1);
						   if (this.value >= multi && numericSymbols[i] !== null) {
							  ret = (this.value / multi) + numericSymbols[i];
						   }
					   }
					}
					return (ret ? ret : this.value);
                }            
			}
		},
		tooltip: {
			valueSuffix: ''
		},
		plotOptions: {
			bar: {
				dataLabels: {
					enabled: true,
					allowOverlap:true,
					style:{
						fontSize:'9px'
					}
				}
			}
		},
		legend: {
			itemDistance: 20,
			layout:"vertical",
			verticalAlign:'top',
			y:55
		},
		credits: {
			enabled: false
		},
		series: series
	});
},
_createGraph:function(options){
	var obj = this;
	var data = options.data.content;
	var s = options.s;
	var p = options.p;
	var act = obj.getCurrentActivity();
	
	
	var title = '';
	var _data = [];
	if(!p || p.id == '00'){
		title = s.label.toUpperCase();
		for(var x in data){
			var xitem = data[x];
			var val = ($.isNumeric(xitem[s.id]))?xitem[s.id]:0;
			_data.push({
				name: xitem['cvegeo'],
				label:xitem['nombre'],
				y: val
				});
		}
	}else{
		title = s.label.toUpperCase()+':'+p.label.toUpperCase();
		for(var x in data){
			var xitem = data[x];
			if($.isNumeric(xitem['producto_especie']) && xitem['producto_especie'] == parseInt(parseInt(p.id),10) ){//} || xitem['cultivo'].toLowerCase() == parseInt(p.label).toLowerCase() ){ //label.toLowerCase()){
				var val = ($.isNumeric(xitem[s.id]))?xitem[s.id]:0;
				_data.push({
					name: xitem['cvegeo'],
					label:xitem['nombre'],
					y:val
				});	
			}
		}
		
	}
	var width = ($(window).width()*0.7);
/*	width = (width < 920)?920;
	width = ($(window).width() <= 800)?$(window).width()*0.9:width;
*/
	$('#agropecuario_graph_container').css('width',(width-100)+'px');
	
	//agropecuario_graph_container
	var cadena = '<div id="agropecuario_graph_bk_btn" class="agropecuario-resize agropecuario-animated agropecuario-graph-bk-btn"><div class="sprite-agropecuario-bback"></div></div>';
		cadena+= '<div id="agropecuario_graph"></div>';
	$('#agropecuario_graph_container').html(cadena);
	$('#agropecuario_graph_bk_btn').click(function(){
		$('#agropecuario_info_content_graph').attr('type','selectors');	
	});
	$('#agropecuario_graph_bk_btn').css({width:'25px'});
	
	var typeBar = {t_2:'Entidad federativa',t_5:'Municipio'}
		typeBar = typeBar['t_'+data[0].cvegeo.length];
	
	Highcharts.chart('agropecuario_graph', {
		lang: {
            printChart: 'Imprimir gráfica',
            downloadPNG: 'Descargar imágen PNG',
            downloadJPEG: 'Descargar imágen JPEG',
            downloadPDF: 'Descargar como PDF',
            downloadSVG: 'Descargar como SVG',
            contextButtonTitle: 'Menú contextual'
        },
		exporting: {
			enabled: true
		},
		chart: {
			type: 'column',
			height:312,
			width:(width-140)
		},
		title: {
			text: ''
		},
		subtitle: {
			text: title
		},
		xAxis: {
			type: 'category',
			labels: {
            		rotation: -90,
					style: {
						fontSize: '13px',
						fontFamily: 'Verdana, sans-serif'
					}
        	}
		},
		yAxis: {
			title: {
				text: ''
			},
			labels: {
                formatter: function () {
					var ret,
					numericSymbols = ['mil', 'M', 'G', 'T', 'P', 'E'],
					i = numericSymbols.length;
					if(this.value >=1000) {
					   while (i-- && ret === undefined) {
						   multi = Math.pow(1000, i + 1);
						   if (this.value >= multi && numericSymbols[i] !== null) {
							  ret = (this.value / multi) + numericSymbols[i];
						   }
					   }
					}
					return (ret ? ret : this.value);
                }            
            }
		},
		legend: {
			enabled: false
		},
		plotOptions: {
			series: {
				borderWidth: 0,
				dataLabels: {
					enabled: false,
					format: '{point.y}'
				}
			}
		},

		tooltip: {
			headerFormat: '<span style="font-size:11px">{series.name}</span><br>',
			pointFormat: '<span>{point.label}</span>: <b>{point.y}</b><br/>'
		},

		series: [{
			name: typeBar,
			colorByPoint: true,
			data: _data
		}]
	});
	
},
printAbout:function(){
	var obj = this;
	var cadena = '';
	
	cadena+= '<table width="100%" border="0">';
  	cadena+= '	<tbody>';
    cadena+= '		<tr>';
    cadena+= '			<th colspan="3">Marco Censal Agropecuario 2016</th>';
    cadena+= '		</tr>';
    cadena+= '		<tr>';
    cadena+= '  		<td width="45%">&nbsp;</td>';
    cadena+= '  		<td width="2%">&nbsp;</td>';
    cadena+= '  		<td width="53%">&nbsp;</td>';
    cadena+= '		</tr>';
    cadena+= '		<tr>';
    cadena+= '  		<td align="center" bgColor="#A9CDB9">Metodología</td>';
    cadena+= '  		<td>&nbsp;</td>';
    cadena+= '  		<td align="center" bgColor="#A9CDB9">Aviso sobre la información contenida</td>';
    cadena+= '		</tr>';
    cadena+= '		<tr>';
    cadena+= ' 			<td align="center"><a href="'+obj.settings.docPath+'/'+obj.settings.mainDoc+'" target="_blank"><div class="agropecuario-resize sprite-agropecuario-doc-pdf-27"></div></a></td>';
    cadena+= '  		<td>&nbsp;</td>';
    cadena+= '  		<td align="center"><div id="agropecuario_btn_notice" class="agropecuario-resize sprite-agropecuario-info"></div></td>';
    cadena+= '		</tr>';
  	cadena+= '	</tbody>';
	cadena+= '</table>';
		
	$('#agropecuario_about_container').html(cadena);
	$('#agropecuario_btn_notice').click(function(){
		obj.disclaimer();
	});
},
printThemeDetail:function(){
	var obj = this;
	var list = obj.currentData.theme['info'];
	var cadena = '';
	var exlude = ['orden']; //atributos a excluir de los datos
	var rename = {'producto_especie':'producto_/_especie'};
	var exportData = {headers:null,content:[]};
	if(list.length > 0){
		var headers = {};
		
		var hds = list[0];
		//obtencion de todos los encabezados en la lista
		for(var x in list){
			var item = list[x];
			for(var y in item){
				if(!headers[y] && exlude.indexOf(y.toLowerCase()) < 0){
					var value = y.toLowerCase().replace('_ha_','_(ha)_');
					//renombrar encabezados
					value = (rename[value])?rename[value]:value;
					headers[y] = value.replace(new RegExp('_', 'g'), ' ');
				}
			}
		}
		exportData.headers = headers;
		
		cadena+= '<table id="tableDetail" class="agropecuario-custom-table selectable-row"><thead><tr>';
		for(var x in headers)cadena+= '<td>'+headers[x].replace(new RegExp('_', 'g'), ' ')+'</td>';
		cadena+= '</tr></thead>';
		
		
		cadena+= '<tbody>';
		var itemToDelete = null;
		var ccount = 0;
		for(var x in list){
			var item = list[x];
			var hdr = $.extend({},headers);
			for(var y in item){
				var val = item[y];
				if(exlude.indexOf(y.toLowerCase()) < 0){
					if($.isNumeric(val) && val == -6)
						val = 'No disponible';
				
					hdr[y] = val;
				}
			}
			
			exportData.content.push(hdr);
			
			var cvegeo = item.cvegeo;
			if (cvegeo==obj.validatedParams.cvegeo) {
                itemToDelete=x;
            }
			cadena+= '<tr class="agropecuario-detail-data-row" '+((cvegeo)?' cvegeo="'+cvegeo+'" ':'')+' >';
			
			for(var y in hdr){
				var val = hdr[y];
				var bold = (['terrenos','superficie_total__ha'].indexOf(y) >= 0 || ccount == 0)?' style="font-weight:bold;color:#000" ':'';
				if(y != 'cvegeo' && $.isNumeric(val))
					cadena+= '<td align="right" '+bold+'>'+val.format()+'</td>'
				else
					cadena+= '<td '+bold+'>'+val+'</td>';
			}
			cadena+= '</tr>';
			ccount++;
		}
		cadena+= '</tbody></table>';
		obj.exportData = exportData;
		
		
	}
	$('#agropecuario_info_content').html(cadena);
	
	var exportDataGraph = $.extend(true,{}, exportData);
	if (itemToDelete) {
        exportDataGraph.content.splice(itemToDelete,1);
    }
	obj.createThemeGraphUI(exportDataGraph);
	
	$('.agropecuario-btn-export-info').each(function(){
		  $(this).click(function(){
			  	var type = $(this).attr('idref');
				obj.exportDataToFile(type,obj.exportData);   
		  })
	});
	
	$('.agropecuario-detail-data-row').each(function(){
		$(this).click(function(){
			var cvegeo = $(this).attr('cvegeo');
			if(cvegeo)
				obj.gotoExtent(cvegeo);
		})
	})
	
	
},
//print panel config
  createRampColor:function(ramp){
	  	var obj = this;
		var colorRamps = obj.settings.colorRamps;
		var currentRamp = obj.currentData.colors;
		var colors = ramp.colors;
		var snum = obj.settings.numStrats;
		var cadena= '<div idref="'+ramp.id+'" class="agropecuario-strats-colorRamp" '+((currentRamp.id == ramp.id)?'selected="selected"':'')+'>';
			for(var x in colors){
				var width = 100/snum;
				cadena+='<div class="agropecuario-strats-ramp-color" style="background-color:'+colors[x]+';width:'+width+'%"></div>';	
			}
			cadena+='</div>';
		return cadena; 
 },
 printConfig:function(){
	 var obj = this;
	 var cd = obj.currentData;
	 /*var colorRamps = obj.settings.colorRamps;
	 
	 var cadena = '';
	 	
	 
	 
	 	 cadena+= '<div id="'+obj.id+'_graph_strat'+'" class="agropecuario-graph-strat"></div>'; 
	 	 cadena+= '<div class="agropecuario-strats-transparency-container">';
		 cadena+= '		<div class="agropecuario-strats-transparency-title">Transparencia</div>';
		 cadena+= '		<div id="agropecuario_strats_trasparencyControl" class="agropecuario-strats-transparency-tool"></div>';
		 cadena+= '</div>';
	 
	 	 cadena+= '<div class="agropecuario-theme-normalInfo agropecuario-theme-info">';
		 cadena+= '		<div><b>Total:</b><label>'+obj.formatMoney(cd.theme.indicator)+'<label></div>';
		 cadena+= '		<div><b>Elementos:</b><label>'+obj.formatMoney(cd.theme.n)+'</label></div>';
		 cadena+= '		<div><b>D.Estd:</b><label>'+obj.formatMoney(cd.theme.sd)+'<label></div>';
	 	 cadena+= '</div>';
		 
		 cadena+= '<div class="agropecuario-theme-mainInfo agropecuario-theme-info">';
	 	 cadena+= '		<div><b>Media:</b><label>'+obj.formatMoney(cd.theme.mean)+'<label></div>';
		 cadena+= '		<div><b>Mediana:</b><label>'+obj.formatMoney(cd.theme.median)+'<label></div>';
		 cadena+= '		<div><b>Moda:</b><label>'+obj.formatMoney(cd.theme.mode)+'<label></div>';
	 	 cadena+= '</div>';
	 
	 
	 	 cadena+=	'<div class="agropecuario-strat-method agropecuario-animated">';
	   
		 var methods = obj.settings.methods; //print methods
		 for(var x in methods){
			var method = methods[x];
			cadena+=	'<div val="'+method.name+'" class="agropecuario-strat-item '+((method.name == cd.method)?'selected':'')+'">'+method.title+'</div>';
		 }

		 cadena+=	'</div>';

		 cadena+=	'<div class="agropecuario-years agropecuario-animated">';
		 cadena+=	'	<div class="agropecuario-years-title">Año</div>';

		 var years = obj.settings.years;  //print years
		 for(var x in years){
			var year = years[x];
			cadena+=	'<div val="'+year+'" class="agropecuario-years-item '+((year == cd.year)?'selected':'')+'">'+year+'</div>';
		 } 
		 cadena+=	'</div>';
	 	 */
	 
	 	//cadena+=	'<div class="agropecuario-config-titles"><label></label></div>'; 
	 	/*cadena+=	'<div class="agropecuario-strats-currentRamps-container">';
	 
		 for(var x in colorRamps){
				var ramp = colorRamps[x];   
				cadena+= obj.createRampColor(ramp); 
		 }
	 	 cadena+=	'</div>';
	 
	 
	 $('#agropecuario_conf_container').html(cadena);*/
	/* 
	 $( "#agropecuario_strats_trasparencyControl" ).slider({
		  range: "max",
		  min: 1,
		  max: 100,
		  value: obj.settings.transparency,
		  slide: function( event, ui ) {
			obj.settings.transparency = ui.value;
			obj.settings.transparency = ui.value;
			obj.options.onTransparency(ui.value);
		  }
	 });*/
	 
	 $('.agropecuario-strats-colorRamp').each(function(){
			$(this).click(function(){
				
				$('.agropecuario-strats-colorRamp[selected=selected]').each(function(index, element) {
						$(this).removeAttr('selected');
				});
				$(this).attr('selected','selected');
				var idref = $(this).attr('idref');
	   			
				obj.rollbackColor = $.extend(true,{},obj.currentData.colors);
				obj.currentData.colors = obj.settings.colorRamps[parseInt(idref,10)];
				obj.changeColorMap();
				obj.hasChanged = true;
				obj.prepareTheme();
				//obj.printGraphData();
				//obj.printThemeStats();
			})   
	   })
	 
	 
	 
	 $('.agropecuario-strat-item').each(function(index, element) {
      		$(this).click(function(){
				var val = $(this).attr('val');
				$('.agropecuario-strat-item.selected').removeClass('selected');
				$(this).addClass('selected');
				
				obj.currentData.method = val;
				obj.hasChanged = true;
				obj.prepareTheme();
				
		    });  
      });
	  $('.agropecuario-years-item').each(function(index, element) {
      		$(this).click(function(){
				var val = $(this).attr('val');
				$('.agropecuario-years-item.selected').removeClass('selected');
				$(this).addClass('selected');
				
				cd.year = parseInt(val,10);
				obj.hasChanged = true;
				obj.prepareTheme();
				
		    });  
      });
	 //print Graph
	 //obj.printGraphData();
 },
//---------------------------------------------------------------------	
  getData:function(source,params,callback,error,before,complete){
		var obj = this;
		if(source){
			var spinner = this.spinner;
			var url = source.url;
			var urlType = url.split('/').slice(-1)[0];
			//Anexo de parametros que vengan definidos desde fuente de datos
			var s_params = source.params;
			var stringify = source.stringify;
			
			//control de servidor personalizado
			var proyect = source.proyect;
			var server = obj.options.config.dataSources.server; 
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
					   			obj.options.systemMessage(mensaje,{height:130});
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
					   if(errorExcepcion != 'abort'){
							obj.options.systemMessage('Sistema en mantenimiento, favor de verificar más tarde',{height:130});
							if ($.isFunction(error)){
								error(errorDescripcion,errorExcepcion);
							};
				   		}
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
			var ajax = $.ajax(dataObject);
			obj.controlAjax({ajax:ajax,type:urlType});
			
		}
	},
	controlAjax:function(data){
		var obj = this;
		var list = obj.jsonTransactions;
		var _list = [];
		for(var x in list){
			var item = list[x];
			if(item.ajax.readyState < 4){
				var type = item.type;
				var inType = data.type;
				if(type == inType){
					item.ajax.abort();
				}else{
					_list.push(item);
				}
			}
		}
		list = _list;
		
		if(list.length > 10){
			list.shift();	
		}
		obj.jsonTransactions = list;
		obj.jsonTransactions.push(data);
		for(var x in obj.jsonTransactions){
		
		}
	},
	changeUI:function(){
		var obj = this;
		var cdata = obj.currentData;
		var act = obj.getCurrentActivity();
		var prod = obj.getActiveProducts();
		var ten = obj.getActiveTenencia();
		var ctools = obj.countActiveTools();
		
		//oculta o presenta la opcion de tenencia
		if(!act.tools){
			$('#agropecuario_tab_type').show();
		}else{
			if(act.tools && ctools > 1){
				$('#agropecuario_tab_type').show();
			}else{
				$('#agropecuario_tab_type').hide();
				obj.setActiveAllTenencia();
			}
		}
	},
	getCurrentDataParams:function(){
		var obj = this;
		
		var cData = obj.currentData;
		var act = obj.getCurrentActivity();
		var prod = obj.getActiveProducts();
		var ten = obj.getActiveTenencia();
		var ctools = obj.countActiveTools();
		
		//si no hay ninguna tenencia envia todas
		if(ten.length == 0)ten = obj.getAllTenencia();
		
		var params = {
			"cvegeo":cData.geoSelected.join(),
			"actividades":act.prefijo,
			"productos":null,
			"tenencias":null,
			"dagua":null
		};
		if(prod.length > 0){
			var ids = [];
			for(var x in prod)
				ids.push(prod[x].prefijo);
			params.productos = ids.join();
		}
		if(ten.length > 0){
			var ids = [];
			for(var x in ten)
				ids.push(ten[x].prefijo);
			params.tenencias = ids.join();
		}
		if(ctools == 1){
			var ids = [];
			var tools = act.tools.attributes;
			for(var x in tools)
				if(tools[x].active)
					params.dagua = x.toUpperCase();
		}
		//si viene valor en el riego no hay tenencia
		if(params.dagua){
			params.tenencias = null;
		}
		
		if(act.id == 99){
			params.actividades = obj.getAllActivities().join();
		}
		
		return params;
	},
	prepareTheme:function(func){
		var obj = this;
		obj.changeUI();
		var params = obj.getCurrentDataParams();
		//revisa si ha sufrido cambios la configuracion
		if(!(JSON.stringify(params) == JSON.stringify(obj.backupData))){
			obj.backupData = params;
			//obj.updateHeader();
			obj.updateTheme(params,func);
			//obj.mapChange();
			/*if(obj.ready){  //bandera que indica que el widget esta listo
					obj.getGridData();
			}*/
		}else{
			if($.isFunction(func))
				func();  //imprimir productos
		}
		
	},
	updateTheme:function(params,func){
		var obj = this;
		var cData = obj.currentData;
		
		var dataSource = $.extend(true,{},obj.options.config.dataSources.theme);
		obj.getData(dataSource,params,function(data){
				if(data.response && data.response.success){
					obj.validatedParams = obj.backupData;
					obj.currentData.theme = data.data.result[0];

					obj.updateHeader();
					obj.printThemeDetail();
					obj.options.clearIdentify();
					/*if(!obj.ready){  //bandera que indica que el widget esta listo
						obj.ready = true;
						obj.getGridData();
					}*/
					obj.ready = true;
					obj.getGridData(func);
				}else{
					if(obj.backupConfig){
						obj.restoreAllUI();
					}		
				}
		},function(){
			if(obj.backupConfig){
				obj.restoreAllUI();
			}
		});
	},
	onClose:function(){ //when destroy object
		var obj = this;
		var themeParams = {'LAYERS':'damca'}
		themeParams['MAPATERRENO'] = 0
		obj.options.refreshMap(themeParams);
	},
	checkThemeColor:function(){
		var obj = this;
		var cData = obj.currentData;
		if(cData.colors.id != 0){
			obj.changeColorMap();
		}
	},
  // Export
	exportDataToFile:function(type,dataIn){
		var obj = this;
		var data = $.extend(true,{},dataIn);
		//var data = obj.exportData;
		var hdr = data.headers;
		var body = data.content;
		
		//incluye fuente
		body.push({c1:'Fuente',c2:obj.settings.proyName});
		
		
		var columns = [];
		var values = [];
		
		//vaciado de columnas
		for(var x in hdr)
			columns.push(hdr[x]);
		
		//vaciado de columnas
		for(var x in body){
			var row = body[x];
			var _row = [];
			for(var y in row){
				_row.push(row[y]);
			}
			values.push(_row);
		}
		
		var ds = $.extend(true,{},obj.options.config.dataSources.exportData);
		var params = {title:'Datos de Agropecuario',columns:columns,values:values};
		obj.getData(ds,params,function(data){
		  if(data && data.response.success){
			var url = ds.urlGet+'/'+type+'/'+data.data.id;
			window.location.assign(url);
		  }
		});
		
	},
	exportStrats:function(type){
	  var obj = this;
	  var data = obj.currentData;
	  var theme = data.theme;
	  if(theme){
		  var _var = data.varActive.descripcion;  
		  var detail = theme.detail;
		  
		  var columns = ['Estrato','Clave Geográfica','Nombre','Valor'];
		  var values = [];
		  
		  for (var x in detail){
				var item = detail[x];  
				var geos = item.cvegeo;
				for(var y in geos){
					var geo_item = geos[y];
					values.push([item.stratum,geo_item.cvegeo,geo_item.nombre,geo_item.indicador]);
				}
		  }
		  //Agregar fuente y variable tematizada
		  var sourceCredits = {};
		  
		  var typeVar = obj.backupData.typeVarSelection;
		  
		  values.push([]);
		  values.push(['Variable:'+_var])
		  values.push(['Fuente:'+obj.settings.source[typeVar]]);
		  
		  var ds = $.extend(true,{},obj.options.config.dataSources.exportData);
		  var params = {title:'Detalle de Estratos',columns:columns,values:values};
		  obj.getData(ds,params,function(data){
			  if(data && data.response.success){
			  	var url = ds.urlGet+'/'+type+'/'+data.data.id;
			  	window.location.assign(url);
			  }
		  });
		  //obj.getData();
	  }
  },
	
	rem_openMetadataVar:function(_var){
		var obj = this;
		var path = obj.settings.docPath;
		var geoType = obj.currentData.geoType;
		var doc = path+'/'+geoType+'_'+_var+'.html';
		
		var file='<iframe src="'+doc+'" width="100%" height="255px" class="agropecuario-metadata-frame"></iframe>';
		 
		obj.options.systemMessage(file,{width:500,height:350,title:'Información'});
	},
  //Funciones para la tematización
	rem_selectVar:function(id){
		var obj = this;
		obj.currentData.varActive = obj.getVar(id);
		obj.hasChanged = true;
		obj.prepareTheme();
		obj.reloadTree();
	},
	selectIndex:function(id){
		var obj = this;
		obj.currentData.index = id;
		obj.loadTree(id);   
	},
	openConfig:function(){
		var obj = this;
		
		obj.element.attr('collapsed','false');
		obj.collapsed = false;
		
		//carga arbol en posición
		obj.loadTree(obj.currentData.index,true);
		obj.printConfig();
		obj.updateHeader();
		obj.mapChange();
		obj.openCount++;
		if(obj.openCount == 1){
			setTimeout(function(){
				obj.detectCollision();	
			},1000);
		}
	},
	cancelModify:function(){
		var obj = this;
		obj.hasChanged = false;
		//obj.currentData = obj.backupData;
		//obj.reloadTree();
		//obj.printThemeDetail();
		//obj.printGeoList(true); //just refresh
		//obj.backupData = null;
		obj.closeConfig();
	},
	closeConfig:function(){
		var obj = this;
		obj.element.attr('collapsed','true');
		obj.collapsed = true;
		obj.updateHeader();
		obj.mapChange();
	},
	rem_printGraphData:function(){
		var obj = this;
		
		var data = obj.currentData;
		var theme = data.theme;
		if(theme){
		  var title = data.varActive.descripcion;  
		  var detail = theme.detail;
		  var cadena = '';	
		 //var graphObj = obj.crateGraphData(detail,title);	 //grafica anterior, bloquedas por el momento Marzo 2017
		  for (var x in detail){
			  var item = detail[x];
			  cadena+= '<div class="agropecuario-stratum-detail-item">';
			  cadena+= '	  	<div class="agropecuario-stratum-detail-item-color" style="background-color:rgb('+(item.rgb.replace(/ /g,','))+');"></div>';
			  cadena+= '	  	<div class="agropecuario-stratum-detail-item-info">';
			  cadena+= '			<div class="agropecuario-stratum-detail-item-info-vals">';
			  cadena+= '				<label class="agropecuario-stratum-detail-item-info-label">Estrato</label>';
			  cadena+= '				<label class="agropecuario-stratum-detail-item-info-value">'+item.stratum+'</label>';
			  cadena+= '			</div>';
			  cadena+= '			<div class="agropecuario-stratum-detail-item-info-vals">';
			  cadena+= '				<label class="agropecuario-stratum-detail-item-info-label">Frecuencia</label>';
			  cadena+= '				<label class="agropecuario-stratum-detail-item-info-value">'+(item.cvegeo.length)+'</label>';
			  cadena+= '			</div>';
			  cadena+= '		</div>';
			  cadena+= '</div>';
		  }
		  $('#'+obj.id+'_graph_strat').html(cadena);
		}
	},
	rem_crateGraphData:function(data,title){
		var obj = this;
		
		//obtieniendo colores
		var colors = obj.currentData.colors.colors;
		
		//Creando datos
		var listSerie = [{
							name:'Estratos',
							colorByPoint: true,
							data:[]
						}];
		var listDrillDown =  {
						drillUpButton: {
							relativeTo: 'spacingBox',
							position: {
								y: 0,
								x: 0
							}
						},
						series: []
					}
		
		for(var x in data){
			var sum = 0;
			var item = data[x];
			var id = item.stratum;
			
			//drillSerie
			var dserie ={
						name: 'Estrato '+id,
						id: 'estrato'+id,
						data: []
			};
			
			for(var y in item.cvegeo){
				var yitem = item.cvegeo[y];
				sum+=parseInt(yitem.indicador,10);
				dserie.data.push({
					name: yitem.cvegeo,
					label: yitem.nombre,
					y: parseInt(yitem.indicador,10),
					drilldown: 'estrato'+id
				});
			}
			listDrillDown.series.push(dserie);
			
			listSerie[0].data.push({
				name: 'E'+id,
				label: 'E'+id,
				y: sum,
				drilldown: 'estrato'+id
			})
		}
		
		var gobj = {
			chart: {
				type: 'column'
			},
			title: {
				text: ''
			},
			colors: colors,
			subtitle: {
				//text: 'Clic en las columnas para ver los detalles.'
			},
			xAxis: {
				type: 'category'
			},
			yAxis: {
				title: {
					text: ''
				}

			},
			legend: {
				enabled: false
			},
			plotOptions: {
				series: {
					borderWidth: 0,
					dataLabels: {
						enabled: false,
						format: '{point.y:.1f}%'
					}
				}
			},

			tooltip: {
				headerFormat: '<span style="font-size:11px">{series.name}</span><br>',
				pointFormat: '<span style="color:{point.color}">{point.label}</span>: <b>{point.y}</b><br/>'
			},

			series: listSerie,
			drilldown: listDrillDown
		}
		
		
		Highcharts.setOptions({
				lang:{drillUpText: "Regresar a {series.name}"},
				
		});
		
		Highcharts.chart(obj.id+'_graph_strat', gobj);
		
	},
	formatMoney:function(nStr){
            nStr += '';
			x = nStr.split('.');
			x1 = x[0];
			x2 = x.length > 1 ? '.' + x[1] : '';
			var rgx = /(\d+)(\d{3})/;
			while (rgx.test(x1)) {
				x1 = x1.replace(rgx, '$1' + ',' + '$2');
			}
			return x1 + x2;
  },
  //COLOR FUNCTIONS
  hexToRgb:function(hex) { //#FFFFFF
	var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
	return result ? {
		r: parseInt(result[1], 16),
		g: parseInt(result[2], 16),
		b: parseInt(result[3], 16)
	} : null;
  },
 //CONTROL DE ACTIVIDADES
  printActivities:function(){
	  var obj = this;
	  var list = obj.currentData.activitiesList;
	  var cadena = '';
	  	for(var x in list){
			var item = list[x];
			var active = list[x].active;
	  		cadena+= '<div idref="'+item.id+'" pos="'+x+'" class="activity-item .activity-item sprite-agropecuario-activity-'+item.id+((active)?'':'-off')+'"></div>';	
		}
	  $('#agropecuario_activity_content').html(cadena).attr('content','mosaic');;
	  
	  $('.activity-item').each(function(){
		  $(this).click(function(){
			 var pos = parseInt($(this).attr('pos'),10);
			 if(list[pos].id != obj.currentData.activity){
				obj.clearAllActivities();
				obj.clearAllActivityProducts();
			 	list[pos].active = true;
			 	obj.currentData.activity = list[pos].id;
				obj.printActivities();
				 //Actualiza tenencias
				obj.setActiveAllTenencia();
				obj.printTenencia();
			}
			obj.prepareTheme(function(){
				if(list[pos].id != 99){
			 		obj.printProducts();
			 	}
			});
		  });
	  })
	  
  },
  productRelation:function(func){
	var obj =this; 
	var list = obj.currentData.activitiesList;
	var ds = obj.options.config.dataSources;
	var dataSource = $.extend(true,{},ds.productRelation);
		obj.getData(dataSource,{},function(data){
			func(data)
		});
  },
  loadProducts:function(idact,func){
	var obj =this; 
	var list = obj.currentData.activitiesList;
	var ds = obj.options.config.dataSources;
	var dataSource = $.extend(true,{},ds.activityProducts);
		dataSource.url+='?id_actividad='+idact;
		obj.getData(dataSource,{},function(data){
			func(data)
		});
  },
  printProducts:function(){
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
			$('#agropecuario_activity_content').effect('drop',600,function(){ //animacion de cambio de contenido
				var tools = act.tools;
				var geos = obj.currentData.geoSelected;
				var activeProds = obj.getActiveProducts();
				var cadena = '';
					cadena+= '	<div id="agropecuario_products_container" class="agropecuario-products-container" '+((tools)?'specialtools="true"':'')+'>';
					//la activadad requiere selección especial de tipo
					if(tools){
						cadena+= '<div id="agropecuario_products_type" class="agropecuario-resize agropecuario-products-type">'; 
						var types = tools.attributes;
						for(var x in types){
							var item = types[x];
							cadena+= '<div title="'+item.label+'" idref="'+x+'" class="ui-corner-all sprite-agropecuario-dagua-tool sprite-agropecuario-dagua-'+x+' '+((item.active)?'active':'')+'"></div>';
						}
						cadena+= '</div>'
					 } 
				
					cadena+= '		<div id="agropecuario_products_content" class="agropecuario-resize agropecuario-products-content">';
				
				    
					//inclusión visual de producto
					var allActive = (activeProds.length == 0)?true:false;
						cadena+= '		<div class="agropecuario-products-item">';
						cadena+= '			<div class="agropecuario-products-item-label agropecuario-truncate" title="Todos los productos">Todos los productos</div>';
						cadena+= '			<div class="agropecuario-products-item-check" idref="00" active="'+allActive+'">';
						cadena+= '				<div idref="00" class="agropecuario-products-item-check-icon sprite-agropecuario sprite-agropecuario-circle inactive"></div>';   
						cadena+= '				<div idref="00" class="agropecuario-products-item-check-icon sprite-agropecuario sprite-agropecuario-ok active"></div>';
						cadena+= '			</div>';
						cadena+= '			<div class="agropecuario-products-item-disable-mask"></div>';
						cadena+= '		</div>';
					//fin de ajuste visual
				

					for(var x in prods){
						var item = prods[x];
							item.active = (item.active == true);
						var active = item.active;
						var disabled = (obj.isValidProduct(item.id_producto,geos))?'':' disabled="disabled" ';
						cadena+= '		<div class="agropecuario-products-item" '+disabled+'>';
						//cadena+= '			<span class="agropecuario-products-item-info ui-icon ui-icon-info" idref="'+item.id_producto+'"></span>';
						cadena+= '			<div class="agropecuario-products-item-label agropecuario-truncate" title="'+item.nombre+'">'+item.nombre+'</div>';
						cadena+= '			<div class="agropecuario-products-item-check" idref="'+item.id_producto+'" active="'+active+'">';
						cadena+= '				<div idref="'+item.id_producto+'" class="agropecuario-products-item-check-icon sprite-agropecuario sprite-agropecuario-circle inactive"></div>';   
						cadena+= '				<div idref="'+item.id_producto+'" class="agropecuario-products-item-check-icon sprite-agropecuario sprite-agropecuario-ok active"></div>';   
						cadena+= '			</div>';
						cadena+= '			<div class="agropecuario-products-item-disable-mask">';
						cadena+= '				<span class="agropecuario-products-item-info ui-icon ui-icon-info" idref="'+item.id_producto+'"></span>';	
						cadena+= '			</div>';
						cadena+= '		</div>';
					}

					cadena+= '		</div>';
					cadena+= '		<div id="agropecuario_products_bk_btn" class="agropecuario-resize agropecuario-animated agropecuario-products-bk-btn"><div class="sprite-agropecuario-bback"></div></div>';
					cadena+= '	</div>';

					$('#agropecuario_activity_content').html(cadena);
					
					$('#agropecuario_activity_content').fadeIn();

					$('#agropecuario_activity_content').attr('content','list');
					obj.uiSettings.activityPanel = list; 
					
					$('.agropecuario-products-item-info').each(function(){
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
									obj.prepareTheme();
								}
							}else{
								tools.attributes[idref].active = !isActive;
								$(this).addClass('active');
								obj.prepareTheme();
							}
							
						});
					});
				

					$('#agropecuario_products_bk_btn').click(function(){
						$('#agropecuario_activity_content').fadeOut(function(){
							obj.printActivities();
							$('#agropecuario_activity_content').effect('slide',600);
						});
					});
					$('.agropecuario-products-item-check').each(function(){
						$(this).click(function(){
							var idref = parseInt($(this).attr('idref'),10);
							var active = ($(this).attr('active') == 'true');
								active = !active;
								obj.setActivityStatus(idref,active);
								$(this).attr('active',active);
								
								var activeProds = obj.getActiveProducts();
								//ajuste visual
								$('.agropecuario-products-item-check').each(function(){
									$(this).attr('active','false');
								})
								if(activeProds.length == 0){
									$('.agropecuario-products-item-check[idref=00]').attr('active',true);
								}else{
									for(var x in activeProds){
										var prod = activeProds[x];
										$('.agropecuario-products-item-check[idref='+prod.id_producto+']').attr('active',true);
									}
								}
								obj.prepareTheme();
						});
					});
			}); 
		}
	}
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
  setActivityStatus:function(id,status){
	var obj = this;
	var list = obj.currentData.activitiesList;
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
  adjustProductByFilterTable:function(){
		var obj = this;
	  	var act = obj.getCurrentActivity();
	  	var geos = obj.currentData.geoSelected;
		var prods = act.products;
	  	var changedSome = false;
	  	var notAllowed = [];
	  	if(prods){
			for(var x in prods){ //agrega propiedad de activo o inactivo si no la tiene
				var p = prods[x];
				if(p.active){
					var valid = obj.isValidProduct(p.id_producto,geos);
					if(!valid){
						changedSome = true;
						notAllowed.push(p.nombre);
					}
					p.active = valid;
				}
			}
			if(changedSome){
				var sing = 'El cultivo <b>'+notAllowed.join()+'</b> no es representativo en esta entidad.';
				var plur = 'Los cultivos <b>'+notAllowed.join()+'</b> no son representativos en esta entidad.';
				
				var mensaje = ((notAllowed.length == 1)?sing:plur);
				obj.options.systemMessage(mensaje,{title:'Aviso',height:130});
			}
		}
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
		obj.options.systemMessage(mensaje,{title:'Información de '+pto.nombre,width:390,height:330});
  },
  getAllActivities:function(){
	var obj = this;
	var list = obj.currentData.activitiesList;
	var excludeList = obj.settings.excludeOnAllLayerEvent;
	var r = [];
	for(var x in list){
		var item = list[x];
		if(item.prefijo && excludeList.indexOf(item.prefijo) < 0){
			r.push(item.prefijo);
		}
	}
	return r;
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
  getProduct:function(id){
	var obj = this;
	
	var list = obj.currentData.activitiesList;
	var products = [];
	  for(var x in list){
		  if(list[x].id == obj.currentData.activity){
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
  getCurrentActivities:function(){
	  var obj = this;
	  var list = obj.currentData.activitiesList;
	  var actives = [];
	  for(var x in list){
		  if(list[x].active)actives.push(list[x]);
	  }
	  return actives;
  },
  printActivitiesProducts:function(pos){
	  var obj = this;
	  var list = obj.currentData.activitiesList;
	  var active = obj.getCurrentActivities();
	  
	  $('#agropecuario_activity_content').attr('content','list').fadeOut(function(){
		  
		  var cadena = '';
		  
		  
	  });
  },
  getCurrentGeoDataActive:function(){
	var obj = this;
	var cData = obj.currentData;
	var sel = cData.geoSelected;
	var r = [];
	var geoList = cData.currentGeo;
	for(var y in geoList){
		var geo = geoList[y];
		if(sel.indexOf(geo.cvegeo)>= 0){
			r.push(geo)
		}
	}
	return r;
  },
  //Control de tenencia -----------------------------------------------------------------------
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
  getAllTenencia:function(){
	var obj =this;
	var list = obj.currentData.tenenciaList;
	var r = [];
	  for(var x in list){
		  	r.push(list[x]);
	  }
	return r;
  },
  setActiveAllTenencia:function(){
	var obj =this;
	var list = obj.currentData.tenenciaList;
	  for(var x in list){
		  list[x].active = true;
	  }
  },
  clearAllTenencia:function(){
	var obj =this;
	var list = obj.currentData.tenenciaList;
	  for(var x in list){
		  list[x].active = false;
	  }
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
  printTenencia:function(pos){
	var obj =this;
	var list = obj.currentData.tenenciaList;
	  
	if(list.length ==  obj.currentData.tenenciaList.length){
		obj.clearAllTenencia();
		list = obj.currentData.tenenciaList;
	}
	  
	var act = list[pos];
	if(list.length > 0){ //Si tiene tenencias
		var cadena = '';
			cadena+= '	<div id="agropecuario_tenencia_container" class="agropecuario-tenencia-container">';
			cadena+= '		<div id="agropecuario_tenencia_content" class="agropecuario-resize agropecuario-tenencia-content">';
			
			var t = obj.getActiveTenencia();
			var allActive = (t.length == 0)?true:false;
			//control visual para todas las tenencias
			cadena+= '		<div class="agropecuario-tenencia-item">';
			cadena+= '			<div class="agropecuario-tenencia-item-label agropecuario-truncate">Todas las tenencias</div>';
			cadena+= '			<div class="agropecuario-tenencia-item-check" idref="00" active="'+allActive+'">';
			cadena+= '				<div idref="00" class="agropecuario-tenencia-item-check-icon sprite-agropecuario sprite-agropecuario-circle inactive"></div>';   
			cadena+= '				<div idref="00" class="agropecuario-tenencia-item-check-icon sprite-agropecuario sprite-agropecuario-ok active"></div>';   
			cadena+= '			</div>';
			cadena+= '		</div>';

			for(var x in list){
				var item = list[x];
					item.active = (item.active == true);
				var active = item.active;
				
				cadena+= '		<div class="agropecuario-tenencia-item">';
				cadena+= '			<div class="agropecuario-tenencia-item-label agropecuario-truncate">'+item.nombre+'</div>';
				cadena+= '			<div class="agropecuario-tenencia-item-check" idref="'+item.id+'" active="'+active+'">';
				cadena+= '				<div idref="'+item.id+'" class="agropecuario-tenencia-item-check-icon sprite-agropecuario sprite-agropecuario-circle inactive"></div>';   
				cadena+= '				<div idref="'+item.id+'" class="agropecuario-tenencia-item-check-icon sprite-agropecuario sprite-agropecuario-ok active"></div>';   
				cadena+= '			</div>';
				cadena+= '		</div>';
			}

			cadena+= '		</div>';
			cadena+= '	</div>';

			$('#agropecuario_type_container').html(cadena);

			$('.agropecuario-tenencia-item-check').each(function(){
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
					if(idref == 0 || (t.length ==  obj.currentData.tenenciaList.length)){
						obj.clearAllTenencia();
					}
					t = obj.getActiveTenencia();
					$('.agropecuario-tenencia-item-check').each(function(){
							$(this).attr('active','false');
					})
					if(t.length == 0){
						$('.agropecuario-tenencia-item-check[idref=00]').attr('active',true);
					}else{
						for(var x in t){
							var tene = t[x];
							$('.agropecuario-tenencia-item-check[idref='+tene.id+']').attr('active',true);
						}
					}
					
					obj.prepareTheme();
				});
				
			});
	}
  },
  mapChange:function(){
	 var obj = this;
	 var status = obj.options.getMapStatus();
	 var zoom = status.zoom;
	 var table = [null,6,5,4,3,2,1]
	 var grid = (zoom >= 7)?0:table[zoom];
	 
	 var isRampVisible =  ($('#agropecuario_header_colors').attr('show') == 'true');
	 var isCollapsed = obj.element.attr('collapsed') == 'true';
	 
	 if(isCollapsed){
		 if(grid == 0 && isRampVisible){
			 $('#agropecuario_header_colors').slideUp();
			 $('#agropecuario_header_colors').attr('show','false');
		 }
		 if(grid > 0 && !isRampVisible){
			  $('#agropecuario_header_colors').slideDown();
			  $('#agropecuario_header_colors').attr('show','true');
		 }
	 }else{
		 $('#agropecuario_header_colors').hide().attr('show','false');
	 }
	  
	 if(zoom && obj.ready){
		 obj.getGridData();
	 }
  },
  updateLayerOnMap:function(theme){
		var obj = this;
		
	  	var params = obj.getCurrentDataParams();
		var mapStatus = obj.options.getMapStatus();
		var zoom = mapStatus.zoom;
		var table = [null,6,5,4,3,2,1]
		var grid = (zoom >= 7)?0:table[zoom];
	  
		var themeParams = {'LAYERS':'damca'}

		themeParams['MALLA'] = grid;
		themeParams['MAPATERRENO'] = theme.id
		
		obj.options.refreshMap(themeParams);
  },
  saveZoomData:function(key,zoom,data){
	var obj = this;
	var zdata = obj.getZoomData(key);
	if(!zdata){
		obj.zoomData.push({key:key,levels:{}})
		zdata = obj.getZoomData(key);
	}
	zdata.levels['z_'+zoom] = data;
  },
  getZoomData:function(key){
	  var obj = this;
	  var list = obj.zoomData;
	  var r = null;
	  if(list.length > 0){
		  for(var x in list){
		  	var item = list[x];
			if(item.key == key){
				r = item;
				break;
			}
		 }
	  }
	  return r;
  },
  getGridData:function(func,estrato){
	  var obj = this;
	  var params = obj.getCurrentDataParams();
	  var mapStatus = obj.options.getMapStatus();
	  var zoom = mapStatus.zoom;
	  var table = [null,6,5,4,3,2,1]
	  var grid = (zoom >= 7)?0:table[zoom];
	  params.malla = grid;
	  
	  if(estrato === undefined){
	  	params.estratos = 5;
		estrato = params.estratos;
	  }else{
		params.estratos = estrato;  
	  }
	  
	  //Revizar si hay cambios en la configuración de la información
	  var key = $.extend({},params);
	  delete key.malla;
	  var dataExists = obj.getZoomData(JSON.stringify(key));
	  if(!(dataExists && dataExists.levels['z_'+zoom])){
		  var dataSource = $.extend(true,{},obj.options.config.dataSources.grid);
		  obj.getData(dataSource,params,function(data){
			  if(data.response.success){
				  obj.saveZoomData(JSON.stringify(key),zoom,data.data);
				  var min = data.data.minimo;
				  var max = data.data.maximo;
				  if(typeof(min) == 'string')min = parseFloat(min);
				  if(typeof(max) == 'string')max = parseFloat(max);

				  $('#agropecuario-year-min').html(min.format()+' (Ha)');
				  $('#agropecuario-year-max').html(max.format()+' (Ha)');
				  obj.updateLayerOnMap(data.data);
				  
				  obj.backupConfig = $.extend(true,{},obj.currentData);
				  
				  if($.isFunction(func))
						func();
				  
			  }else{
				  if(data.response && data.response.message == 'false#409' && estrato > 1){
						obj.getGridData(func,estrato-1);	
				  }else{
					  if(data.response && data.response.message == 'false#409' && estrato <= 1){
					  		var mensaje = 'No hay terrenos que cumplan con los criterios seleccionados';
						  	obj.options.systemMessage(mensaje,{height:130});
					  }
						  
					  if(obj.backupConfig){
						obj.restoreAllUI();
				  	  }
				  }
			  }
		  },function(){
			  if(obj.backupConfig){
				obj.restoreAllUI();
			  }
		  });
	  }else{
		  var data = dataExists.levels['z_'+zoom];
		  var min = data.minimo;
		  var max = data.maximo;
		  if(typeof(min) == 'string')min = parseFloat(min);
		  if(typeof(max) == 'string')max = parseFloat(max);
		  $('#agropecuario-year-min').html(min.format()+' (Ha)');
		  $('#agropecuario-year-max').html(max.format()+' (Ha)');
		  
		  obj.updateLayerOnMap(data);
	  }
  },
  disclaimer:function(func){
		var obj = this;

		var localUrl = require.toUrl("widgets");
			localUrl = localUrl.split('?')[0];
			localUrl+='/agropecuario/doc/';


		var cadena=  '<div id="agropecuario_disclaimer" title="Marco Censal Agropecuario 2016">';
			cadena+= '</div>';

		$("#panel-center").append(cadena);
		$('#agropecuario_disclaimer').load(localUrl+'disclaimer.html');

		$( "#agropecuario_disclaimer" ).dialog({
			width:550,
			height:370,
			resizable: false,
			closeOnEscape: true,
			close: function(event, ui){
				$(this).dialog('destroy').remove();
			},
			modal: true,
			buttons: {
				Cerrar: function() {
				  $(this).dialog('close');
				}
			   /*Acepto: function() {
				  $(this).dialog('close');
				  if($.isFunction(func))
					func();
			   },
			   'No acepto': function() {
				  $(this).dialog('close');
			   } */
			}
		});
  },
  detectCollision:function(){
	  var obj = this;
	  var isCollision = obj.options.collision(obj.element);
	  if(isCollision){
		  obj.options.onCollision();
	  }
  },
  
  onIdentify:function(data){
	var obj = this;
	var mapStatus = obj.options.getMapStatus();
	var zoom = mapStatus.zoom;
	var idAction = Math.floor((Math.random() * 100000) + 1);
	obj.currentIdentify = idAction;
	var position = data.pos;
	  
	obj.afterIdentify = function(){
	  	data.func(data.pos)
	}
	
	//equivalencias
	
	$('#agropecuario_card_detail').remove();	
	  
	var ds = obj.options.config.dataSources;
	var dataSource = $.extend(true,{},ds.getCardInfo);  

	  var params = obj.getCurrentDataParams();
	  var mapStatus = obj.options.getMapStatus();
	  var zoom = mapStatus.zoom;
	  var table = [null,6,5,4,3,2,1]
	  var grid = (zoom >= 7)?0:table[zoom];
	  params.malla = grid;
	  params.point = 'POINT('+position.lon+' '+position.lat+')';
	  
		//dataSource.url+='/'+cvegeo;
		obj.getData(dataSource,params,function(data){
		  	//if(data.response.success){
			if(data.response && data.response.success && data.data){
				var cell = data.data.valuecell;
				var polygon = null;
				
				var exportData = {headers:null,content:[]};
				
				
				var prods = obj.getActiveProducts();
				var cadena = '<div class="agropecuario-markerBox-container">';
				var windowSize = {resizable:false,width:280,height:450};
				if(cell){
					polygon = cell.geometry;
					
					windowSize = {resizable:false,width:280,height:87};
					cadena+= '<label>Superficie sembrada aproximada</label>';
					cadena+= '<table class="agropecuario-custom-table">';
					cadena+= '<tr><td>Hectareas</td><td align="right">'+cell.valuecell+'</td></tr>';
					cadena+= '</table>';
					//cadena+= '<div idref="xls" class="agropecuario-btn-export-card sprite-agropecuario-doc-xls"></div>';
					exportData.headers = {c1:'Superficie sembrada aproximada',c2:''};
					exportData.content = [{c1:'Hectareas',c2:cell.valuecell}];
					obj.exportData = exportData;
					
				}else{
					var terreno = data.data.terreno;
					data = data.data.control;
					polygon = (terreno)?terreno[0].geometry:data.geometry;
					
					exportData.headers = {c1:'Datos del área de control',c2:''};
					
					cadena+= '<label class="space">Datos del área de control</label>';
					cadena+= '<table class="agropecuario-custom-table">';
					cadena+= '<tr><td><strong>Estado</strong></td><td>'+data.nom_ent+'</td></tr>';
					cadena+= '<tr><td><strong>Municipio</strong></td><td>'+data.nom_mun+'</td></tr>';
					cadena+= '<tr><td><strong>Clave de área de control</strong></td><td>'+data.control+'</td></tr>';
					cadena+= '<tr><td><strong>Total de terrenos</strong></td><td>'+data.terrenos+'</td></tr>';
					cadena+= '</table></br>';
					
					exportData.content.push({c1:'Estado',c2:data.nom_ent});
					exportData.content.push({c1:'Municipio',c2:data.nom_mun});
					exportData.content.push({c1:'Clave de área de control',c2:data.control});
					exportData.content.push({c1:'Total de terrenos',c2:data.terrenos});
					
					cadena+= '<table class="agropecuario-custom-table">';
					cadena+= '<thead>';
					cadena+= '	<tr><td>Act. Principales</td>';
					cadena+= '	<td align="right">Terrenos</td><Tr>';
					cadena+= '</thead>';
					cadena+= '<tbody>';
					cadena+= '	<tr><td>Agrícola</td><td align="right">'+data.t_a+'</td></tr>';
					cadena+= '	<tr><td>Ganadera</td><td align="right">'+data.t_g+'</td></tr>';
					cadena+= '	<tr><td>Forestal</td><td align="right">'+data.t_f+'</td></tr>';
					cadena+= '	<tr><td>Otra actividad</td><td align="right">'+data.t_oa+'</td></tr>';
					cadena+= '	<tr><td>Sin actividad</td><td align="right">'+data.t_sa+'</td></tr>';
					cadena+= '</tbody>';
					cadena+= '</table></br>';
					
					exportData.content.push({c1:'Act. Principales',c2:'Terrenos'});
					exportData.content.push({c1:'Agrícola',c2:data.t_a});
					exportData.content.push({c1:'Ganadera',c2:data.t_g});
					exportData.content.push({c1:'Forestal',c2:data.t_f});
					exportData.content.push({c1:'Otra actividad',c2:data.t_oa});
					exportData.content.push({c1:'Sin actividad',c2:data.t_sa});
					
					var act = obj.getCurrentActivity();
					var ctools = obj.countActiveTools();
					var tool = '';
					if(ctools == 1){
						var ids = [];
						var tools = act.tools.attributes;
						for(var x in tools)
							if(tools[x].active)
								tool = (x.toUpperCase() == 'R')?'de riego':'de temporal';
					}
					
					//var txtAgua = (act.tools)?
					
					cadena+= '<label>Actividad seleccionada</label>';
					cadena+= '<table class="agropecuario-custom-table">';
					cadena+= '<tr><td>'+act.nombre+' '+tool+'</td></tr>';
					cadena+= '</table>';
					
					exportData.content.push({c1:'Actividad seleccionada',c2:''});
					exportData.content.push({c1:act.nombre,c2:''});
					
				/*	if(prods && prods.length > 0){
						
						cadena+= '<label>Productos</label>';
						cadena+= '<table class="agropecuario-custom-table">';
						
						var pnames = [];
						for(var x in prods){
							var item = prods[x];
							pnames.push(item.nombre);

						}
						cadena+= '<tr><td>'+pnames.join()+'</td></tr>';
						cadena+= '</table>';
						
						exportData.content.push({c1:'Productos',c2:pnames.join()});
					}*/
					var isConfidential = (!terreno)?true:false;
					var cparams = obj.getCurrentDataParams();
					
					var act = obj.getCurrentActivity();
					var title_s_v = ['a','ca','ap'];
					var title_s ='Superficie total (Ha)'; //cambios oct  'Superficie sembrada
					
					if(cparams.actividades == 'ca'){
						 title_s ='Superficie sembrada (Ha)';
					}
					if(cparams.actividades == 'ca' && isConfidential){
						 title_s ='Superficie total (Ha)';
					}
					
					if(act.prefijo == 'ca') // si es a cielo abierto cambiar a siguiente titulo
						 title_s = 'Superficie total (Ha)'
					
						/*
					(
									(title_s_v.indexOf(act.prefijo) >= 0)
								  )?'Superficie sembrada (Ha)':'Superficie total (Ha)';
					/*		
						if(((['a','ca','ap'].indexOf(act.prefijo) > 0) && (prods.length == 0 ))){ //si es agricultura y no hay producto
							title_s = 'Superficie total (Ha)';
						}*/
					
					if(prods.length > 0){
						for(var x in prods){
							var item = prods[x];

							var t = data['t_'+item.prefijo+'_'+act.prefijo];
							var s = data['s_'+item.prefijo+'_'+act.prefijo].toFixed(2);
							var p = data['p_'+item.prefijo+'_'+act.prefijo];
							
							//OCT 2018
							if(act.prefijo == 'ca') // si es a cielo abierto cambiar la fuente del valor
						 		 s = data['s_'+item.prefijo+'_a'];
							
							//nov 2018 Cambio basado en FER, si viene s_tp reemplazo el valor de s
							if(data['s_tp']){
								s = data['s_tp'];
							}
							
							cadena+= '<label class="space">Totales de '+item.nombre+'</label>';
							cadena+= '<table class="agropecuario-custom-table">';
							cadena+= '<tr><td>Total de terrenos</td><td align="right">'+t+'</td></tr>';
							cadena+= '<tr><td>'+title_s+'</td><td align="right">'+parseFloat(s,10).toFixed(2)+'</td></tr>';
							//cadena+= '<tr><td>Total de productores</td><td align="right">'+p+'</td></tr>';
							cadena+= '</table>';

							exportData.content.push({c1:'Totales de '+item.nombre,c2:''});
							exportData.content.push({c1:'Total de terrenos',c2:t});
							exportData.content.push({c1:title_s,c2:s});
							//exportData.content.push({c1:'Total de productores',c2:p});

						}
					}else{
							var t = data['t_'+act.prefijo];
							var s = data['s_'+act.prefijo].toFixed(2);
						
							//OCT 2018
							if(act.prefijo == 'ca') // si es a cielo abierto cambiar la fuente del valor
						 		 s = data['s_a'];
						
							//nov 2018 Cambio basado en FER, si viene s_tp reemplazo el valor de s
							if(data['s_tp']){
								s = data['s_tp'];
							}
						
							
							cadena+= '<label class="space">Totales de '+act.nombre+'</label>';
							cadena+= '<table class="agropecuario-custom-table">';
							cadena+= '<tr><td>Total de terrenos</td><td align="right">'+t+'</td></tr>';
							cadena+= '<tr><td>'+title_s+'</td><td align="right">'+parseFloat(s,10).toFixed(2)+'</td></tr>';
							cadena+= '</table>';

							exportData.content.push({c1:'Totales de '+act.nombre,c2:''});
							exportData.content.push({c1:'Total de terrenos',c2:t});
							exportData.content.push({c1:title_s,c2:s});
						
					}
					//inclusión de terrenos
					if(terreno){
							var list = terreno;
							cadena+= '<label class="space">Datos en el grupo de terrenos</label>';
							exportData.content.push({c1:'Datos en el grupo de terrenos',c2:''});
							var actNames = ['a','ca','ap'];
							
							for(var x in terreno){
								var item = terreno[x];
								
								//Parche temporal de datos
								/*var cparams = obj.getCurrentDataParams();
								if(cparams.dagua && cparams.actividades == 'ca'){
									item.super_cart = item.sup_sem;
								}*/
								//
								
								var prod = obj.getProduct(item.producto);
								if(terreno.length > 0 && prod){
									cadena+= '<label class="space">Producto: '+prod.nombre+'</label>';
									exportData.content.push({c1:'Producto:'+prod.nombre,c2:''});
								}
								cadena+= '<table class="agropecuario-custom-table">';	
								
								//cadena+= '<tr><td>Productores</td><td align="right">'+item.productores+'</td></tr>';
								//exportData.content.push({c1:'Productores',c2:item.productores});
								
								cadena+= '<tr><td>Total de terrenos</td><td align="right">'+item.total_terrenos+'</td></tr>';
								cadena+= '<tr><td>Superficie total (Ha)</td><td align="right">'+item.super_cart.toFixed(2)+'</td></tr>';
								
								exportData.content.push({c1:'Total de terrenos',c2:item.total_terrenos});
								exportData.content.push({c1:'Superficie total (Ha)',c2:item.super_cart});
								
								if(actNames.indexOf(act.prefijo) >= 0 )
									if(!((['a','ca','ap'].indexOf(act.prefijo) >= 0) && (prods.length == 0 ))){ //si es agricultura y no hay producto)
										cadena+= '<tr><td>Superficie sembrada (Ha)</td><td align="right">'+item.sup_sem.toFixed(2)+'</td></tr>';
										exportData.content.push({c1:'Superficie sembrada (Ha)',c2:item.sup_sem});
									}
								
								cadena+= '</table>';
								
								
								
							}
							
							
					}
					cadena+= '<div idref="xls" class="agropecuario-btn-export-card sprite-agropecuario-doc-xls"></div>';
					
					obj.exportData = exportData;
					//inclusión de fuente
					$('#agropecuario_card_detail').html(cadena);
						
				}
				cadena+= '</div>';
				if (idAction == obj.currentIdentify){
					//pinta poligono en mapa
					
					MDM6('deletePolygons','SpetialPolygons'); 
					var options = {fColor:"none",lSize:2,lColor:"#66C5CC",lType:"line",type:'buffer',clearFeatures:true,layer:'SpetialPolygons'};
					MDM6('addPolygon',polygon,options);
					
					//--------------------------
					setTimeout(function(){
						obj.options.markerWithText(position,cadena,function(){
							$('.agropecuario-btn-export-card').each(function(){
								$(this).click(function(){
									obj.exportDataToFile('xls',obj.exportData);
								});
							});
						});
					},100);
					
					
					
				}
				
			}else{
				obj.afterIdentify();
			}
				
		},function(){
			obj.afterIdentify();
		});
	
  }
});






















